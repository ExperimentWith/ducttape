package ducttape

import ducttape.{PackedGraph => packed}
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
  private val values = new HashMap[String, HashSet[Realization]]
  
  
  private val comments = new HashMap[(String,Realization), String]
  
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
    
    node match {
      
      case packed.Literal(_) => return true
      
      case packed.BranchPointNode(bp, branches) => {
        if (realization.explicitlyRefersTo(bp)) {
          val successes = branches.map{ branchNode => recursivelyProcessSpec(branchNode, realization, s"${comment}[${bp.name}") }
          for (success <- successes) { if (success) return true } // If at least one branch succeeds, we're good
          return false
        } else {
          return false
        }
      }
      
      case packed.BranchNode(branch, value) => {
        if (realization.explicitlyRefersTo(branch)) {
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
                if (task.containsVariable(variableName)) {
                  val success = recursivelyProcess(task, realization, comment)
                  if (success && ! this.contains(task.name, realization)) {
                    this.add(task.name, Seq(realization), s"${comment} $$${variableName}@${task.name}")
                  }
                  
                  return success
                } else {
                  warn(s"A reference to ${variableName}@${taskName} was found, but no variable with that name is defined for task ${taskName}")
                  return false
                }
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
        return false
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
    
    for (plan <- packedGraph.plans) {
      val numReachClauses = plan.crossProducts.size
      
      for ((crossProduct,index) <- plan.crossProducts.zipWithIndex) {
    
        val taskNames = crossProduct.goals
      
        val branchTuples:Seq[(BranchPoint, Seq[Branch])] = crossProduct.value.map { branchPointRef => branchPointRef.getBranches(packedGraph.branchFactory) }
        val branchMap:   Map[ BranchPoint, Seq[Branch] ] = branchTuples.toMap
      
        val realizations = Realization.crossProduct(branchMap, withOmissions=true)
      
        val comment = if (numReachClauses > 1) s"${plan} (Clause $index of $numReachClauses)" else plan.toString
                       
        for (taskName <- taskNames) {
          goals.add(taskName, realizations, comment)
        }
      }
    }
    
    return goals
  }
  
  
}