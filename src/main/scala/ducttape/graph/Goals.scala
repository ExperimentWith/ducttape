package ducttape.graph

import ducttape.graph.{PackedGraph => packed}
import ducttape.syntax.{AbstractSyntaxTree => ast}
import ducttape.workflow.Branch
import ducttape.workflow.BranchFactory
import ducttape.workflow.BranchPoint
import ducttape.workflow.Realization

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet

import grizzled.slf4j.Logging

/** Maps from task name to the required set of realizations for that task. */
class Goals private(private val packedGraph:PackedGraph) extends Logging { // private() declares the default constructor to be private
  
  /** Maps from task name to the required set of realizations for that task. */
  private[graph] val values = new HashMap[String, HashSet[Realization]]
  
  
  private[graph] val comments = new HashMap[(String,Realization), String]
  
  override def toString() : String = {

    val s = new StringBuilder()
    
    for (taskName <- values.keySet.toList.sorted) {
      
      for (realization <- values(taskName).toList.sortWith{(a,b) => a.toFullString() < b.toFullString() }) {
        
        s.append(s"reach ${taskName} via ${realization.toFullString()}\t${comments((taskName,realization))}\n")
        
      }
      
    }
    
    return s.toString()
  }
  
  /** Adds realizations to the set required for a specified task. */
  private def add(taskName:String, realizations:Seq[Realization], comment:String): Unit = {
    
    val realizationsForTask = values.getOrElseUpdate(taskName, new HashSet[Realization])
    
      //values.getOrElseUpdate(taskName, new HashSet[Realization]())
    //debug(s"Contains ${taskName}:\t${realizationsForTask.size}\t${values.contains(taskName)}")
    
    for (realization <- realizations) {
      realizationsForTask.add(realization)
      //debug(s"Contains ${taskName}:\t${realizationsForTask.size}\t${values.contains(taskName)}")
      
      val tuple = (taskName, realization)
      if (! comments.contains(tuple)) {
        comments.put(tuple, comment)
      }
      //debug(s"Size is now ${this.size}\t${values.get(taskName)}")
    }
    
    //System.exit(-1)
  }
  
  private def contains(taskName:String, realization:Realization): Boolean = {
    
    values.get(taskName) match {
      case Some(set) => return set.contains(realization)
      case None      => return false
    }
    
  }
  
  
  private def remove(taskName:String, realization:Realization): Unit = {
    var shouldWarn = false
    
    values.get(taskName) match {
      case Some(set) => set.remove(realization)
      case None      => shouldWarn = true
    }
    
    val tuple = (taskName, realization)
    comments.get(tuple) match {
      case Some(comment) => comments.remove(tuple)
      case None      => shouldWarn = true
    }
    
    if (shouldWarn) {
      warn(s"An attempt was made to remove realization ${realization.toFullString()} as a goal for task ${taskName}, but that task currently has no associated realizations")
    }
    
  }
  
  private def removeAll(taskName:String): Unit = {
    values.remove(taskName)
    
    // The keys to the comments map are (name, realization) tuples.
    //
    // Gather up the (name, realization) tuples where name==taskName
    val keys = comments.keys.collect{ case (name, realization) if name==taskName => (name, realization) }
    
    // Then, remove them from the comments map
    for (key <- keys) {
      comments.remove(key)
    }
    
  }
  
  def tasks: Iterable[String] = values.keys
  
  def apply(taskName:String): Iterable[Realization] = values(taskName)
 
  def size: Int = values.values.flatten.size


  /** Recursively processes the inputs, outputs, and parameters of a [[ducttape.PackedGraph.Task]].
    * 
    * If the recursive processing of all of these is successful, this method will return <code>true</code>.
    * 
    * Otherwise, this method will return <code>false</code>, indicating that the provided task (or a task it refers to) is incompatible in some way with the provided realization.
    */
  def recursivelyProcess(task:packed.Task, realization:Realization, comment:String): Boolean = {
        
    val success = 
      recursivelyProcess(task.inputs,  realization, s"${comment} ${task.name} input") &&
      recursivelyProcess(task.params,  realization, s"${comment} ${task.name} param") &&
      recursivelyProcess(task.outputs, realization, s"${comment} ${task.name} output")
      
    return success
    
  }

  
  /** Recursively processes a sequence of [[ducttape.PackedGraph.Spec]] node objects.
    * 
    * If the recursive processing of all of these is successful, this method will return <code>true</code>.
    * 
    * Otherwise, this method will return <code>false</code>, indicating that the provided node (or a task it refers to) is incompatible in some way with the provided realization.
    */  
  def recursivelyProcess(specs:Seq[packed.Spec], realization:Realization, comment:String): Boolean = {
    
    for (spec <- specs) {
      
      val success = recursivelyProcessSpec(spec.value, realization, s"${comment} ${spec.name}")       
      if (! success) return false
      
    }
    
    // Only return true if every spec in the sequence succeeds
    return true
  }
  
  def recursivelyProcessVariableReference(task:PackedGraph.Task, realization:Realization, comment:String, variableName:String): Boolean = {
    if (task.containsVariable(variableName)) {
      val success = recursivelyProcess(task, realization, comment)
		  if (success) {
		    if (! this.contains(task.name, realization)) {
			    this.add(task.name, Seq(realization), s"${comment} $$${variableName}@${task.name}")
			  }
		    return true
		  } else {
			  return false
		  }
    } else {
      warn(s"A reference to ${variableName}@${task.name} was found, but no variable with that name is defined for task ${task.name}")
      return false      
    }
  }  
  
  /** Recursively processes a [[ducttape.PackedGraph.Spec]] node object.
    * 
    * If any of the following conditions are met, this method will return <code>true</code>:
    *  - the node is a [[ducttape.PackedGraph.Literal]]
    *  - the node contains a path of [[ducttape.PackedGraph.BranchPointNode]]-[[ducttape.PackedGraph.BranchNode]] pair(s) compatible with the provided [[ducttape.workflow.Realization]] that terminates in a [[ducttape.PackedGraph.Literal]]
    *  - the node is a [[ducttape.PackedGraph.Reference]] and a recursive call of this method on the referred task returns <code>true</code>
    *  - the node contains a path of [[ducttape.PackedGraph.BranchPointNode]]-[[ducttape.PackedGraph.BranchNode]] pair(s) compatible with the provided [[ducttape.workflow.Realization]] that terminates in a [[ducttape.PackedGraph.Reference]], and a recursive call of this method on the referred task returns <code>true</code>
    * 
    * Otherwise, this method will return <code>false</code>, indicating that the provided node (or a task it refers to) is incompatible in some way with the provided realization.
    */
  def recursivelyProcessSpec(node:packed.ValueBearingNode, realization:Realization, comment:String): Boolean = {
    
    val defaultRealization = (realization == ducttape.workflow.Task.NO_REALIZATION)
    
    node match {
      
      case packed.Literal(_) => return true
      
      case packed.BranchPointNode(bp, branches) => {
        //if (realization.explicitlyRefersTo(bp) || defaultRealization) 
        {
          val successes = branches.map{ branchNode => recursivelyProcessSpec(branchNode, realization, s"${comment}[${bp.name}") }
          for (success <- successes) { if (success) return true } // If at least one branch succeeds, we're good
          return false
        } 
        //else {
        //  return false
        //}
      }
      
      case packed.BranchNode(branch, value) => {
        if (realization.explicitlyRefersTo(branch) || branch.baseline) { //(branch.baseline && defaultRealization)) {
          return recursivelyProcessSpec(value, realization, s"${comment}:${branch.name}]")
        } else {
          return false
        }
      }
      
      case packed.Reference(variableName, possibleTaskName, graftsList) => {
        possibleTaskName match {
          
          // Look up a task
          case Some(taskName) => {
            
            packedGraph.task(taskName) match {
            
              case Some(task) => {
                //if (task.containsVariable(variableName)) {
                  
                  if (graftsList.isEmpty) {
                		  val success = recursivelyProcessVariableReference(task, realization, comment, variableName)
                		  return success                    
                  } else {
                	  for (graftRealization <- graftsList) {
                		  val newRealization = Realization.applyGraft(graft=graftRealization, original=realization)
                		  val success = recursivelyProcessVariableReference(task, newRealization, comment, variableName)
                		  if (!success) return false
                	  }
                	  return true
                  }
                //} else {
                //  warn(s"A reference to ${variableName}@${taskName} was found, but no variable with that name is defined for task ${taskName}")
                //  return false
                //}
              }
              
              case None       => {                
                warn(s"A reference to task ${variableName} was found, but no task with that name exists")
                return false                
              }
              
            }
            
          }
          
          // Look up a global variable
          case None           => {
          
            packedGraph.global(variableName) match {
              
              case Some(globalVariable) => {
                val spec = globalVariable.value
                return recursivelyProcessSpec(spec.value, realization, comment)
              }
              
              case None                 => {
                warn(s"A reference to global variable ${variableName} was found, but no global variable with that name exists")
                return false
              }
              
            }
            
          }
          
        }
        //return false
      }
      
    }
    
  }
      
}


object Goals extends Logging {

 
  def fromPlans(packedGraph:PackedGraph): Goals = {
    
    debug("Searching for goals")
    
    val goals:Goals = initialGoalsFromPlans(packedGraph)
    //val goals:Goals = initialGoalsFromPlans(packedGraph.plans, packedGraph.branchFactory)
    
    debug(s"Found ${goals.size} goals")
    var counter = 0
    for (taskName <- goals.tasks) {
      counter += 1
      
      packedGraph.task(taskName) match {
        case Some(packedTask) => {
        	var counter2 = 0
        	for (realization <- goals(taskName)) {
        	  counter2 += 1
        	  val key = (packedTask.name, realization)
        	  val comment = goals.comments(key)
        	  val success = goals.recursivelyProcess(packedTask, realization, comment)

        		if (success) {
        		  goals.debug(s"${counter}\t${counter2}\tSpecified goals require task ${taskName} with realization ${realization.toFullString()}")
        		} else {
        			goals.debug(s"${counter}\t${counter2}\tSpecified goals requested task ${taskName} with realization ${realization.toFullString()}, but that combination cannot be fulfilled")
        			goals.remove(taskName, realization)
        		}
          }          
        }
        
        // This case indicates that the packed graph does not contain a task with that taskName
        //
        // This case probably should not ever happen, but in case it does, handle it gracefully
        case None             => {  
          warn(s"A plan contains a reference to task ${taskName}, but no task with that name could be found.")
          goals.removeAll(taskName)
        }
      }
            
    }
    goals.debug(s"Ended with ${goals.size} goals")
    return goals
    
  }

  
  //private def initialGoalsFromPlans(plans:Seq[ast.PlanDefinition], branchFactory:BranchFactory): Goals = {
  private def initialGoalsFromPlans(packedGraph:PackedGraph): Goals = {
    
    val goals = new Goals(packedGraph)

    // A ducttape workflow may have zero or more "plan" blocks.
    //
    // Each "plan" block may contain zero or more "reach..via" clauses.
    
    
    val totalNumReachClauses = packedGraph.plans.map{ plan => plan.crossProducts.size}.sum

    // If there are zero plans, or if no plan contains any "reach..via" clauses,
    //   we act as if the following plan were in effect:
    //
    // plan {
    //   reach task_1 via (Baseline: baseline)
    //   reach task_2 via (Baseline: baseline)
    //   ...
    //   reach task_n via (Baseline: baseline)
    // }
    //
    if (packedGraph.plans.isEmpty || totalNumReachClauses==0) {
      
      val baselineOnly = Seq(ducttape.workflow.Task.NO_REALIZATION)
      val baselineComment = ducttape.workflow.Task.NO_REALIZATION.toString()
    
      for (taskName <- packedGraph.taskNames) {
        goals.add(taskName, baselineOnly, baselineComment)
      }
      
    } else {
    
      // If there are plans with "reach..via" clauses, process each plan to identify the explicit realizations required by the plan
      for (plan <- packedGraph.plans) {
      
        // Get the number of "reach..via" clauses explicitly mentioned in the plan
        val numReachClauses = plan.crossProducts.size
      
        // Each crossProduct represents a "reach..via" clause within a "plan" block
        // We keep track of the index so that we can report user-friendly messages that include clause numbers
        for ((reachVia,index) <- plan.crossProducts.zipWithIndex) {
    
          // Get the task names explicitly requested by the "reach" component of this "reach...via" clause 
          val reach = reachVia.goals
        
          // Get the branches explicitly requested by the "via" component of this "reach..via" clause
          val branchTuples:Seq[(BranchPoint, Seq[Branch])] = reachVia.value.map { via => via.getBranches(packedGraph.branchFactory) }
        
          // Gather a map, where the keys are branch points, and the values are the branches (of each respective branch point) that were explicitly requested by the "via" component of this "reach..via" clause
          val branchMap:   Map[ BranchPoint, Seq[Branch] ] = branchTuples.toMap
      
        
          val realizations:Seq[Realization] = Realization.crossProduct(branchMap, withOmissions=false)
      
          val comment = if (numReachClauses > 1) s"${plan} (Clause $index of $numReachClauses)" else plan.toString
                       
          for (taskName <- reach) {
            goals.add(taskName, realizations, comment)
          }
        }
      }
    }
    return goals
  }
  
  
}