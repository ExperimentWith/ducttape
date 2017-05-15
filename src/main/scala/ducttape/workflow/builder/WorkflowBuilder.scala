// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.workflow.builder

import ducttape.hyperdag.meta.PhantomMetaHyperDagBuilder
import ducttape.hyperdag.PackedVertex
import ducttape.syntax.Namespace
import ducttape.syntax.AbstractSyntaxTree.ASTType
import ducttape.syntax.AbstractSyntaxTree.BranchGraft
import ducttape.syntax.AbstractSyntaxTree.BranchPointDef
import ducttape.syntax.AbstractSyntaxTree.BranchPointRef
import ducttape.syntax.AbstractSyntaxTree.Comments
import ducttape.syntax.AbstractSyntaxTree.ConfigAssignment
import ducttape.syntax.AbstractSyntaxTree.ConfigVariable
import ducttape.syntax.AbstractSyntaxTree.CrossProduct
import ducttape.syntax.AbstractSyntaxTree.Literal
import ducttape.syntax.AbstractSyntaxTree.LiteralSpec
import ducttape.syntax.AbstractSyntaxTree.PlanDefinition
import ducttape.syntax.AbstractSyntaxTree.Sequence
import ducttape.syntax.AbstractSyntaxTree.SequentialBranchPoint
import ducttape.syntax.AbstractSyntaxTree.Spec
import ducttape.syntax.AbstractSyntaxTree.SubmitterDef
import ducttape.syntax.AbstractSyntaxTree.TaskDef
import ducttape.syntax.AbstractSyntaxTree.TaskHeader
import ducttape.syntax.AbstractSyntaxTree.TaskVariable
import ducttape.syntax.AbstractSyntaxTree.Unbound
import ducttape.syntax.AbstractSyntaxTree.VersionerDef
import ducttape.syntax.AbstractSyntaxTree.WorkflowDefinition
import ducttape.syntax.AbstractSyntaxTreeException
import ducttape.syntax.BashCode
import ducttape.syntax.FileFormatException
import ducttape.workflow.Branch
import ducttape.workflow.BranchFactory
import ducttape.workflow.BranchPoint
import ducttape.workflow.BranchPointFactory
import ducttape.workflow.HyperWorkflow
import ducttape.workflow.NoSuchBranchException
import ducttape.workflow.NoSuchBranchPointException
import ducttape.workflow.RealizationPlan
import ducttape.workflow.SpecGroup
import ducttape.workflow.Task
import ducttape.workflow.TaskTemplate
import ducttape.workflow.Types.PackedWorkVert
import ducttape.workflow.SpecTypes.SpecPair
import scala.collection.Seq
import scala.collection.Set
import scala.collection.Map
import scala.collection.mutable
import grizzled.slf4j.Logging

/**
 * This is where the real magic happens of turning an Abstract Syntax Tree
 * into an immutable HyperWorkflow that everything else can use to perform actions.
 *
 * This builder constructs a MetaHyperDAG whose edges represent *temporal* relationships
 * between tasks (e.g. intermediate files). The builder resolves parameters as literals
 * ahead of time and associates them with "phantom" vertices that are hidden since they
 * are trivially complete.
 *
 * See [[ducttape.hyperdag.meta.PhantomMetaHyperDag]] for an explanation of phantom vertices.
 */
class WorkflowBuilder(wd: WorkflowDefinition, configSpecs: Seq[ConfigAssignment], builtins: Seq[WorkflowDefinition])
  extends Logging {

  import WorkflowBuilder._

  //val branchPointFactory = new BranchPointFactory
  //val branchFactory = new BranchFactory(branchPointFactory)
  // see [[ducttape.workflow.Types]] for an explanation of how generic HyperDAG types
  // correspond to workflow-specific types
  val dag = new PhantomMetaHyperDagBuilder[TaskTemplate, BranchPoint, Branch, SpecGroup]()

  def catcher[U](func: => U)(implicit ref: BranchPointRef) = try { func } catch {
    case e: NoSuchBranchPointException => {
      throw new FileFormatException("No such branch point: %s".format(e.msg), ref)
    }
    case e: NoSuchBranchException => {
      throw new FileFormatException("No such branch: %s".format(e.msg), ref)
    }
  }

  def buildPlans(planDefs: Seq[PlanDefinition], branchFactory: BranchFactory): Seq[RealizationPlan] = {
    planDefs.flatMap { planDef: PlanDefinition =>
      val numReachClauses = planDef.crossProducts.size
      var i = 0
      planDef.crossProducts.map { cross: CrossProduct =>
        i += 1
        val realizations: Map[BranchPoint, Set[String]] = cross.value.map { implicit ref: BranchPointRef =>
          catcher {
            val branchPoint: BranchPoint = branchFactory.getBranchPoint(ref.name)
            // TODO: Change branches back to Branch after we get the baseline/branch name duality hammered out?
            val branches: Set[String] = ref.branchNames.flatMap { element: ASTType =>
              element match {
                case l: Literal => Seq(l.value.toString)
                case s: Sequence => {
                  for (x: BigDecimal <- s.start to s.end by s.increment) yield {
                    x.toString
                  }
                }
                case e: ASTType => throw new AbstractSyntaxTreeException(e, "Element cannot be used to refer to a branch name")
              }
            }.toSet
            (branchPoint, branches) // map entry
          }
        }.toMap

        val fullName = if (numReachClauses > 1) s"${planDef} (Clause $i of $numReachClauses)"
                       else planDef.toString
        new RealizationPlan(planDef, cross.goals, realizations, fullName)
      }
    }
  }
  
  /** getHyperedges() is called by traverse() when traversing a nested branch point tree
   *  to add graph structure leading to a particular task's newly created sink vertex.
   */
  def getHyperedges(task: TaskTemplate,
                    specPhantomV: PackedVertex[Option[TaskTemplate]],
                    curNode: BranchPointTreeGrafts,
                    debugNesting: Seq[Branch])
                   (implicit toVertex: TaskDef => PackedVertex[Option[TaskTemplate]])
    : Seq[(Branch, Seq[(PackedVertex[Option[TaskTemplate]], SpecGroup)])] = {

    // we create a phantom vertex when:
    // 1) we need an imaginary home for config specs
    // 2) we have more than one branch point in a row (nested branch points)

    // possibleHyperedges will become the return value of this function,
    // which is also the type expected by dag.addMetaEdge()
    val possibleHyperedges = curNode.tree.children.map { branchChild: BranchTree =>

      // find edges for which we need to recursively call traverse()
      // due to further nested branch points
      val nestedBranchEdges: Seq[(PackedVertex[Option[TaskTemplate]], SpecGroup)] = {
        // we might have multiple branch point trees if there are different graft sets
        // but this is already taken care of in the structure of the branch point tree
        branchChild.children.map { bpChild: BranchPointTreeGrafts =>
          // we have more than one branch point in a row: create a phantom V for each graft
          val branchPhantomV: PackedVertex[Option[TaskTemplate]]
            = dag.addPhantomVertex(comment = "Phantom:%s.%s.nestedBranch[%s]".format(task.name, branchChild.branch.toString, bpChild.grafts.mkString(",")))
          traverse(task, specPhantomV, bpChild, debugNesting ++ Seq(branchChild.branch), branchPhantomV)
          (branchPhantomV, SpecGroup.empty(grafts=bpChild.grafts))
        }
      }

      // branches with no further branch points nested under them
      // get normal edges attached to them, which lead back to previous
      // tasks
      // we distinguish between literals (as specified by .isParam)
      // and specs that depend on previous tasks (not params)
      val terminalEdges: Seq[(PackedVertex[Option[TaskTemplate]], SpecGroup)] = {
        branchChild.terminalData.map { data: TerminalData =>
          if(data.isParam) {
            // no temporal dependency
            (specPhantomV, new SpecGroup(data.specs, data.grafts))
          } else {
            data.task match {
              // has a temporal dependency on a previous task
              case Some(task) => (toVertex(task), new SpecGroup(data.specs, data.grafts))
              // depends on a non-task such as a config/global assignment
              case None => (specPhantomV, new SpecGroup(data.specs, data.grafts))
            }
          }
        }
      }
      debug("Task=%s; Found nested edges: %s and terminal edges: %s".format(task, nestedBranchEdges, terminalEdges))

      (branchChild.branch, nestedBranchEdges ++ terminalEdges)
    } // end map function
    
    // result type: (PackedVertex[Option[TaskTemplate]], SpecGroup)
    possibleHyperedges.filter {
      // don't include hyperedges with zero source vertices
      case (branchInfo, edges) => edges.size > 0
    }
  }

  /** traverse() recursively traverses a branch point tree starting from the current vertex
   *  being created, adding meta-edges at each recursive call level until we reach each source
   *  task's vertex.
   *
   * Overview of some of the compicated things traverse must deal with here -- in traverse() and getHyperedges():
   * * Nested branch points
   * * Grafting inside branch point defs
   * * Grafting of the same branch point that is currently being defined, inside that branch point def
   * * Having the same branch (a hyperedge) introducing different graft sets on its component edges (this happens frequently for Baseline.baseline)
   *   - choosing one branch must jointly activate all graft sets -- don't accidentally iterate over (branch, graftSet) in epsilon vertex
   * * Having different branches with the same graft set (consider 2 branches, 1 graft set)
   *   - if we accidentally have an epsilon vertex for each (branch, graftSet) pair, then the branches will collide and cancel out
   * * Having grafts at different levels of branch point nesting
   *
   * We will add one metaedge per branch point.
   * The Baseline branch point and baseline branch are automatically added by findTasks() in the first pass.
   *
   * specPhantomV: phantom vertex to which all literals for this task are attached
   * curNode: the current level of the nested branch point tree that we're handling in this recursive call
   * nestedBranches: the branches that have been accumulated so far, down the nested branch point tree
   * sinkV: the current task's vertex toward which our new edges are directed */
  def traverse(task: TaskTemplate,
    specPhantomV: PackedVertex[Option[TaskTemplate]],
    curNode: BranchPointTreeGrafts,
    nestedBranches: Seq[Branch],
    sinkV: PackedVertex[Option[TaskTemplate]])(implicit toVertex: TaskDef => PackedVertex[Option[TaskTemplate]]) {

    // the branch point associated with the meta edge being created
    val tree: BranchPointTree = curNode.tree
    val branchPoint: BranchPoint = tree.branchPoint
    debug("Task=%s %s: BranchPointTreeData is %s".format(task, nestedBranches, curNode))

    // getHyperedges() is mutually recursive with traverse()
    // getHyperedges() will create any necessary phantom vertices and additional graph
    // structure necessary if it encounters nested branch points
    val hyperedges: Seq[(Branch, Seq[(PackedVertex[Option[TaskTemplate]], SpecGroup)])]
      = getHyperedges(task, specPhantomV, curNode, nestedBranches)

    if (!hyperedges.isEmpty) {
      val name = "Epsilon:%s:%s".format(branchPoint.toString, task.name)
      debug("Task=%s %s: Adding metaedge '%s' for branchPoint %s to HyperDAG: Component hyperedges are: %s".
        format(task, name, nestedBranches, branchPoint, hyperedges))
      dag.addMetaEdge(branchPoint, hyperedges, sinkV, comment=name)
    } else {
      debug("Task=%s %s: No metaedge for branchPoint %s is needed (zero component hyperedges)".
        format(task, nestedBranches, branchPoint))
    }
  }

  // create dependency pointers based on workflow definition
  //
  // TODO: Implement branch globbing. Here are some notes on how that might be done:
  /*
  	The AST creation needs to recognize globs and create an AST node for the glob
 	 The TaskTemplateBuilder will pattern match this new node at some point
 	 At that point it needs to create a new GlobSpec type
 	 
   */
  def build(branchFactory: BranchFactory): HyperWorkflow = {
        
    // first, find temporal and structural dependencies among tasks and store them as an edge map
    // also, pre-resolve any non-temporal dependencies such as parameters
    val foundTasks: FoundTasks = WorkflowBuilder.findTasks(wd, configSpecs, branchFactory)

    // == we've just completed our first pass over the workflow file and linked everything together ==

    val vertices = new mutable.HashMap[Namespace, PackedVertex[Option[TaskTemplate]]]
    for (tt <- foundTasks.taskTemplates) {
      if (vertices.contains(tt.name)) {
        val prev: TaskTemplate = vertices(tt.name).value.get
        throw new FileFormatException("Duplicate task name: %s".format(tt.name.toString),
          List(tt.taskDef, prev.taskDef))
      }
      vertices += tt.name -> dag.addVertex(tt, comment=tt.name.toString)
    }
    implicit def toVertex(t: TaskDef): PackedVertex[Option[TaskTemplate]] = vertices(t.name)

    // now build a graph representation by adding converting to (meta/hyper) edges
    for (v: PackedVertex[Option[TaskTemplate]] <- vertices.values) {
      val taskT: TaskTemplate = v.value.get
      debug("Adding %s to HyperDAG".format(taskT))
      val nestedBranchInfo: BranchPointTreeGrafts = foundTasks.parents(taskT)
      val specPhantomV: PackedVertex[Option[TaskTemplate]] = dag.addPhantomVertex(comment = "Phantom:%s.literals".format(taskT.name))
      traverse(taskT, specPhantomV, nestedBranchInfo, Nil, v)
    }

    // organize packages
    val packageDefs = wd.packages.map { p => (p.name, p) }.toMap
    val plans: Seq[RealizationPlan] = buildPlans(wd.plans, branchFactory)

    // TODO: More checking on submitters and versioners?
    val submitters: Seq[SubmitterDef] = wd.submitters ++ builtins.flatMap { b: WorkflowDefinition => b.submitters }
    debug("Workflow has submitters: %s".format(submitters.map(_.name).mkString(" ")))
    val versioners: Seq[VersionerDef] = wd.versioners ++ builtins.flatMap { b: WorkflowDefinition => b.versioners }
    debug("Workflow has versioners: %s".format(versioners.map(_.name).mkString(" ")))

    val result = new HyperWorkflow(dag.build(), wd, packageDefs, plans, submitters, versioners, branchFactory)
    debug("Workflow has %d vertices".format(result.dag.size))
    result
  } // end build()
}

// TODO: Internal classes for this object (other than ResolveMode)
// can be factored out into their own files
object WorkflowBuilder {

  def findBranchPoints(elements: Seq[ASTType]): BranchFactory = {

    val branchFactory = new BranchFactory()
    
    for (element <- elements) {
      branchFactory.findBranchPoints(element)
    }
    
    return branchFactory
  }
  
  
  def findTasks(wd: WorkflowDefinition, configSpecs: Seq[ConfigAssignment], branchFactory: BranchFactory): FoundTasks = {
    
    val confSpecs: Map[String, Spec] = configSpecs.map { a: ConfigAssignment => (a.spec.name, a.spec) }.toMap
    
    // resolver has no knowledge of DAGs nor the dag builder
    val resolver = new TaskTemplateBuilder(wd, confSpecs, branchFactory)

    // first, find temporal and structural dependencies among tasks and store them as an edge map
    // also, pre-resolve any non-temporal dependencies such as parameters
    val foundTasks: FoundTasks = resolver.findTasks()
    
    return foundTasks
  }

}
