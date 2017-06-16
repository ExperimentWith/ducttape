// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.syntax

import ducttape.cli.Directives
import ducttape.exec.Submitter
import ducttape.exec.Versioners
import ducttape.exec.PackageVersionerInfo
//import ducttape.exec.UnpackedRealDagVisitor
import ducttape.graph.UnpackedGraph
import ducttape.graph.{UnpackedGraph => unpacked}
import ducttape.graph.traversal.Traversal
import ducttape.graph.traversal.Visitor
//import ducttape.workflow.HyperWorkflow
//import ducttape.workflow.RealTask
//import ducttape.workflow.Visitors
import ducttape.workflow.Realization
//import ducttape.workflow.PlanPolicy
//import ducttape.versioner.WorkflowVersionInfo
import ducttape.syntax.AbstractSyntaxTree.BranchPointDef
import ducttape.syntax.AbstractSyntaxTree.ConfigAssignment
import ducttape.syntax.AbstractSyntaxTree.Spec
import ducttape.syntax.AbstractSyntaxTree.SubmitterDef
import ducttape.syntax.AbstractSyntaxTree.WorkflowDefinition
import ducttape.syntax.AbstractSyntaxTree.TaskDef
import ducttape.syntax.AbstractSyntaxTree.ConfigDefinition
import ducttape.syntax.AbstractSyntaxTree.PackageDef
import ducttape.syntax.AbstractSyntaxTree.VersionerDef
import ducttape.syntax.AbstractSyntaxTree.ActionDef
import ducttape.syntax.AbstractSyntaxTree.Literal
import grizzled.slf4j.Logging
import collection._

/**
 * Most checks can be done on the raw WorkflowDefinition.
 * 
 * Checks that require access to resolved parameters could be implemented
 * in an unpacked workflow visitor.
 */
class WorkflowChecker(workflow: WorkflowDefinition)
    extends Logging {
  
  
  // TODO: Break up into several methods
  def check(): (Seq[FileFormatException],Seq[FileFormatException]) = {
    
    val warnings = new mutable.ArrayBuffer[FileFormatException]
    val errors = new mutable.ArrayBuffer[FileFormatException]

    // check for using imports without explicitly enabling them
    if (!workflow.directives.enableImports && workflow.usesImports) {
      errors += new FileFormatException("Workflow uses the experimental 'import' feature, but does not explicitly enable them using 'ducttape_experimental_imports=true'", workflow)
    }

    // check for using experimental package syntax
    if (!workflow.directives.enablePackages) {
      for (task: TaskDef <- workflow.tasks; if (task.packages.size > 0) ) {
        errors += new FileFormatException("Workflow uses the experimental 'package' syntax, but does not explicitly enable it using 'ducttape_experimental_packages=true'. Minor changes to this syntax are planned for a future release.", task)
      }
    }

    // check for using experimental submitter syntax
    if (!workflow.directives.enableSubmitters) {
      for (task: TaskDef <- workflow.tasks) {
        val hasSubmitter = task.params.exists { spec => spec.dotVariable && spec.name == "submitter" }
        if (hasSubmitter) {
          errors += new FileFormatException("Workflow uses the experimental 'submitter' syntax, but does not explicitly enable it using 'ducttape_experimental_submitters=true'. Major changes to this syntax are planned for a future release.", task)
        }
      }
    }

    // check all task-like things declared with the task keyword
    for (task: TaskDef <- workflow.tasks ++ workflow.packages) {
      
      val vars = new mutable.HashMap[String, Spec]
      for (spec: Spec <- task.allSpecs) {
        vars.get(spec.name) match {
          case Some(prev) => {
            errors += new FileFormatException("Task variable %s originally defined at %s:%d redefined at %s:%d".
                  format(prev.name, prev.declaringFile, prev.pos.line, spec.declaringFile, spec.pos.line),
                  List(prev, spec))
          }
          case None => ;
        }
        vars += spec.name -> spec
      }
      
      // don't allow branch points on outputs
      for (out: Spec <- task.outputs) {
        out.rval match {
          case _: BranchPointDef => {
            errors += new FileFormatException("Outputs may not define branch points at %s:%d".
              format(out.rval.declaringFile, out.rval.pos.line),
              List(out))
          }
          case _ => ;
        }
      }
    }
    
    val globals = new mutable.HashMap[String, Spec]
    for (a: ConfigAssignment <- workflow.confSpecs) {
      globals.get(a.spec.name) match {
        case Some(prev) => {
          errors += new FileFormatException("Global variable %s originally defined at %s:%d redefined at %s:%d".
                format(a.spec.name, prev.declaringFile, prev.pos.line, a.spec.declaringFile, a.spec.pos.line),
                List(prev, a))
        }
        case None => ;
      }
      globals += a.spec.name -> a.spec
    }
    
    // don't allow globals to have a name
    for (globalBlock: ConfigDefinition <- workflow.globalBlocks) globalBlock.name match {
      case None => ; // good
      case Some(name) => {
        errors += new FileFormatException("Global variable block defined at %s:%d has a name. This is not allowed.".
                format(globalBlock.declaringFile, globalBlock.pos.line, globalBlock.declaringFile),
                globalBlock)
      }
    }
    
    // check versioners to make sure they're sane
    for (v: VersionerDef <- workflow.versioners) {
      {
        if (!v.packages.isEmpty)
          errors += new FileFormatException("Versioners cannot define packages: Versioner '%s'".format(v.name), v)
        if (!v.inputs.isEmpty)
          errors += new FileFormatException("Versioners cannot define inputs: Versioner '%s'".format(v.name), v)
        if (!v.outputs.isEmpty)
          errors += new FileFormatException("Versioners cannot define outputs: Versioner '%s'".format(v.name), v)
        v.params.find { p => p.dotVariable && p.name == "submitter" } match {
          case None => ;
          case Some(spec) => errors += new FileFormatException("Versioners cannot define a submitter: Versioner '%s'".format(v.name), spec)
        }
      }
      
      val info = new PackageVersionerInfo(v)
      
      {
        val checkout: ActionDef = info.checkoutDef
        if (!checkout.packages.isEmpty)
          errors += new FileFormatException("The checkout action cannot define packages: Versioner '%s'".format(v.name), checkout)
        if (!checkout.inputs.isEmpty)
          errors += new FileFormatException("The checkout action cannot define inputs: Versioner '%s'".format(v.name), checkout)
        if (checkout.outputs.map(_.name) != Seq("dir"))
          errors += new FileFormatException("The checkout action must define exactly one output called 'dir': Versioner '%s'".format(v.name), checkout)
        if (!checkout.params.isEmpty)
          errors += new FileFormatException("The checkout action cannot define parameters: Versioner '%s'".format(v.name), checkout)
      }
      
      {
        val repoVer: ActionDef = info.repoVersionDef
        if (!repoVer.packages.isEmpty)
          errors += new FileFormatException("The repo_version action cannot define packages: Versioner '%s'".format(v.name), repoVer)
        if (!repoVer.inputs.isEmpty)
          errors += new FileFormatException("The repo_version action cannot define inputs: Versioner '%s'".format(v.name), repoVer)
        if (repoVer.outputs.map(_.name) != Seq("version"))
          errors += new FileFormatException("The repo_version action must define exactly one output called 'version': Versioner '%s'".format(v.name), repoVer)
        if (!repoVer.params.isEmpty)
          errors += new FileFormatException("The repo_version action cannot define parameters: Versioner '%s'".format(v.name), repoVer)
      }
      
      {
        val localVer: ActionDef = info.localVersionDef
        if (!localVer.packages.isEmpty)
          errors += new FileFormatException("The local_version action cannot define packages: Versioner '%s'".format(v.name), localVer)
        if (!localVer.inputs.isEmpty)
          errors += new FileFormatException("The local_version action cannot define inputs: Versioner '%s'".format(v.name), localVer)
        if (localVer.outputs.map(_.name).toSet != Set("version", "date"))
          errors += new FileFormatException("The local_version action must define exactly two outputs called 'version' and 'date': Versioner '%s'".format(v.name), localVer)
        if (!localVer.params.isEmpty)
          errors += new FileFormatException("The local_version action cannot define parameters: Versioner '%s'".format(v.name), localVer)
      }
    }
    
    // make sure that each package has defined all of the dot variables required by its versioner
//    val versionerDefs = (workflow.versioners ++ builtins.flatMap(_.versioners)).map { v => (v.name, v) }.toMap
    val versionerDefs = workflow.versioners.map { v => (v.name, v) }.toMap
    debug("Versioners are: " + versionerDefs)
    for (packageDef: PackageDef <- workflow.packages) {
      
      packageDef.params.find { p => p.dotVariable && p.name == "submitter" } match {
        case None => ;
        case Some(spec) => errors += new FileFormatException("Packages cannot define a submitter: Package '%s'".format(packageDef.name), spec)
      }
      
      for (param <- packageDef.params) param.rval match {
        case _: Literal => ;
        case _ => errors += new FileFormatException("Package parameters must be literals: Package '%s'".format(packageDef.name), packageDef)
      }
      
      try {
        // constructor throws if versioner cannot be found or correct actions are not defined
        val versionerDef = Versioners.getVersioner(packageDef, versionerDefs)
        val info = new PackageVersionerInfo(versionerDef)
        
        // check that package defines as dot parameters all parameters required by versioner
        val dotParams: Set[String] = packageDef.params.filter(_.dotVariable).map(_.name).toSet
        for (requiredParam: Spec <- versionerDef.params) {
          if (!dotParams.contains(requiredParam.name)) {
            errors += new FileFormatException(
                "Package '%s' does not define dot parameter '%s' required by versioner '%s'".format(
                   packageDef.name, requiredParam.name, versionerDef.name),
                List(packageDef, requiredParam))
          }
        }
        
      } catch {
        case e: FileFormatException => {
          errors += e
        }
      }
    }
    
    (warnings, errors)
  }
  
  // Grumble: It's only tractable to do this for the selection?
  // Is there any way we can do this for tasks that use only literal submitters?
  def checkUnpacked(graph: UnpackedGraph,
                    traversal: Traversal, submitters:Seq[SubmitterDef]): (Seq[FileFormatException],Seq[FileFormatException]) = {
    
    val warnings = new mutable.ArrayBuffer[FileFormatException]
    val errors = new mutable.ArrayBuffer[FileFormatException]
    
    val submitter = new Submitter(submitters)
    val visitor = new Visitor {
      override def visit(task: unpacked.Task) {
//        for (packageSpec: Spec <- task.packages) {
//          // TODO: Check for each task having the params defined for each of its versioners
//          packageSpec
//        }
        
        // make sure each submitter is defined
        try {
          // notice that the submitter may be defined inside a branch point
          // such that it can't be resolved before unpacking
          val taskSubmitter = submitter.getSubmitter(task)
          val dotParams: Set[String] = task.specs.params.filter(_.dotVariable).map(_.name).toSet
          val requiredParams = taskSubmitter.params.filterNot(_.dotVariable).filterNot { spec =>
            Submitter.SPECIAL_VARIABLES(spec.name)
          }
          for (requiredParam <- requiredParams) {
            if (!dotParams(requiredParam.name)) {
              val workflow = graph.goals.packedGraph.workflow
              val tasks = workflow.tasks ++ workflow.functionCallTasks
              val taskList = tasks.filter{ t => t.name == task.name }.toList
              errors += new FileFormatException(
                s"Dot parameter '${requiredParam.name}' required by submitter '${taskSubmitter.name}' not defined by task '${task.name}'", taskList) //,
//                List(task.taskDef, requiredParam))
            }
          }
        } catch {
          // throws if submitter is not defined
          case e: FileFormatException => errors += e 
        }
      }
    }
    
    // TODO: We could really get away without versions here if we
    // wanted to maintain another code path
    Visitor.visitAll(graph, visitor, 1, traversal)
    
    (warnings, errors)
  }
}
