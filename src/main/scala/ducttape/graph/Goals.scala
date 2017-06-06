package ducttape.graph

import ducttape.graph.Goals.Status
import ducttape.graph.Goals.Failure
import ducttape.graph.Goals.Underspecified
import ducttape.graph.Goals.Success
import ducttape.graph.{PackedGraph => packed}
import ducttape.syntax.{AbstractSyntaxTree => ast}
import ducttape.workflow.Branch
import ducttape.workflow.BranchFactory
import ducttape.workflow.BranchPoint
import ducttape.workflow.Realization
import ducttape.workflow.Task.NO_REALIZATION

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
  
  /** Adds a realization to the set required for a specified task. */
  private def add(taskName:String, realization:Realization, comment:String): Unit = {
    
    val realizationsForTask = values.getOrElseUpdate(taskName, new HashSet[Realization])
    realizationsForTask.add(realization)
      
    val tuple = (taskName, realization)
    if (! comments.contains(tuple)) {
    	comments.put(tuple, comment)
    }
    
  }
  
  private def contains(taskName:String, realization:Realization): Boolean = {
    
    values.get(taskName) match {
      case Some(set) => return set.contains(realization)
      case None      => return false
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
  def recursivelyProcess(task:packed.Task, realization:Realization, comment:String): Status = {
    
    var newComment = s"${comment} ${task.name} input"
    val inputsStatus = recursivelyProcess(task.inputs,  realization, newComment)
    if (inputsStatus == Failure) return Failure
    
    newComment = s"${comment} ${task.name} param"
    val paramsStatus = recursivelyProcess(task.params,  realization, newComment) 
    if (paramsStatus == Failure) return Failure
    
    newComment = s"${comment} ${task.name} output"
    val outputsStatus = recursivelyProcess(task.outputs, realization, newComment)
    if (paramsStatus == Failure) return Failure
    
    val values = Seq(inputsStatus, paramsStatus, outputsStatus)
    val result = Goals.unify(values)
    
    result match {
      case Failure => /* Do nothing */
      case Underspecified => {
    	  if (! this.contains(task.name, NO_REALIZATION)) {
    		  this.add(task.name, NO_REALIZATION, comment)
    	  }        
      }
      case Success(resultRealization) => {
    	  if (! this.contains(task.name, resultRealization)) {
    		  this.add(task.name, resultRealization, comment)
    	  }
      }
    }
    
    return result

  }

  
  /** Recursively processes a sequence of [[ducttape.PackedGraph.Spec]] node objects.
    * 
    * If the recursive processing of all of these is successful, this method will return <code>true</code>.
    * 
    * Otherwise, this method will return <code>false</code>, indicating that the provided node (or a task it refers to) is incompatible in some way with the provided realization.
    */  
  def recursivelyProcess(specs:Seq[packed.Spec], realization:Realization, comment:String): Status = {
    
    val results:Seq[Status] = specs.map{ spec =>
      
      val newComment = s"${comment} ${spec.name}"
      val result = recursivelyProcessSpec(spec.value, realization, newComment, Seq()) 
      if (result == Failure) return Failure
      
      result
    }
    
    val status = Goals.unify(results)
    
    status match {
      case Failure         => return Failure
      case Underspecified  => return Underspecified
      case Success(result) => return Success(result)
    }

  }
  
  def recursivelyProcessVariableReference(task:PackedGraph.Task, realization:Realization, grafting:Boolean, comment:String, variableName:String, branchesSeen:Seq[Branch]): Status = {
    
    if (task.containsVariable(variableName)) {

      val status = recursivelyProcess(task, realization, comment)
      status match {
        
        case Failure                       => return Failure
        case Underspecified                => {
          val result:Realization = NO_REALIZATION

          if (! this.contains(task.name, result)) {
			      this.add(task.name, result, s"${comment} $$${variableName}@${task.name}")
			    }
          
          return Underspecified
        }
        case Success(recursiveRealization) => {
          
          val result:Realization = 
            if (branchesSeen.isEmpty) {
              if (grafting) {
                NO_REALIZATION
              } else {
                recursiveRealization
              }
            } else {
              Realization.fromUnsorted(branchesSeen)
            }
          
		      if (result==NO_REALIZATION) {
		        return Underspecified
		      } else {
		        return Success(result)
		      }
		      
        }
        
      }
      
    } else {
      
      warn(s"A reference to ${variableName}@${task.name} was found, but no variable with that name is defined for task ${task.name}")
      return Failure
      
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
  def recursivelyProcessSpec(node:packed.ValueBearingNode, realization:Realization, comment:String, branchesSeen:Seq[Branch]): Status = {
    
    val defaultRealization = (realization == ducttape.workflow.Task.NO_REALIZATION)
    
    node match {
      
      case packed.Literal(_) => {
        if (branchesSeen.isEmpty) {
          return Underspecified
        } else {
          return Success(Realization.fromUnsorted(branchesSeen))
        }
      }
      
      case packed.BranchPointNode(bp, branches) => {
    	  val newComment = s"${comment}[${bp.name}"
    	  val successes = branches.map{ branchNode => recursivelyProcessSpec(branchNode, realization, newComment, branchesSeen) }
    	  for (success <- successes) {
    		  success match {
    		    case Underspecified | Success(_) => return success // If at least one branch succeeds, we're good
    		    case Failure                     => /* Do nothing */
    		  }
    	  } 
    	  return Failure        
      }
      
      case packed.BranchNode(branch, value) => {
        if (realization.explicitlyRefersTo(branch) || (branch.baseline && !realization.explicitlyRefersTo(branch.branchPoint))) { //(branch.baseline && defaultRealization)) {
          val newComment = s"${comment}:${branch.name}]"
          val newBranches = branchesSeen ++ Seq(branch)
          return recursivelyProcessSpec(value, realization, newComment, newBranches)
        } else {
          return Failure
        }
      }
      
      case packed.Reference(variableName, possibleTaskName, graftsList) => {
        possibleTaskName match {
          
          // Look up a task
          case Some(taskName) => {
            
            packedGraph.task(taskName) match {
            
              case Some(task) => {
            	  
                if (graftsList.isEmpty) {
                		
                	val success = recursivelyProcessVariableReference(task, realization, grafting=false, comment, variableName, branchesSeen)
                	return success
                		
                } else {
                	  
                	for (graftRealization <- graftsList) {
                		val newRealization = Realization.applyGraft(graft=graftRealization, original=realization)
                		val success = recursivelyProcessVariableReference(task, newRealization, grafting=true, comment, variableName, branchesSeen)
                		if (success==Failure) return Failure
                	}
                	  
                	if (branchesSeen.isEmpty) {
                		return Underspecified
                	} else {
                		return Success(Realization.fromUnsorted(branchesSeen))  
                	}
                	  
                }
                
              }
              
              case None       => {                
                warn(s"A reference to task ${variableName} was found, but no task with that name exists")
                return Failure                
              }
              
            }
            
          }
          
          // Look up a global variable
          case None           => {
          
            packedGraph.global(variableName) match {
              
              case Some(globalVariable) => {
                val spec = globalVariable.value
                return recursivelyProcessSpec(spec.value, realization, comment, branchesSeen)
              }
              
              case None                 => {
                warn(s"A reference to global variable ${variableName} was found, but no global variable with that name exists")
                return Failure
              }
              
            }
            
          }
          
        }
        
      }
      
    }
    
  }
      
}


object Goals extends Logging {

 
  def fromPlans(packedGraph:PackedGraph): Goals = {
    
    debug("Searching for goals")   
    val requestedGoals:Seq[Goal] = initialGoalsFromPlans(packedGraph)
    debug(s"Found ${requestedGoals.size} initial goals in the specified plan(s)")
    
    // Construct a Goals object, which will initially contain no actual goals
    val goals = new Goals(packedGraph)
    
    for (goal <- requestedGoals) {
      
      packedGraph.task(goal.taskName) match {
        case Some(task) => {
 
          // Recursively process the specified task. This will cause it and its ancestors to be added to the Goals object.
          val status = goals.recursivelyProcess(task, goal.realization, goal.comment)

        	status match {
        	    
        	  case Failure              =>  goals.debug(s"Specified plan(s) require task ${goal.taskName} with realization ${goal.realization.toFullString()}, but that combination cannot be fulfilled")
        	  case Underspecified       =>  goals.debug(s"Specified plan(s) require task ${goal.taskName} with realization ${goal.realization.toFullString()}, which will be fulfilled by ${NO_REALIZATION.toFullString()}")
        	  case Success(realization) =>  goals.debug(s"Specified plan(s) require task ${goal.taskName} with realization ${goal.realization.toFullString()}, which will be fulfilled by ${realization.toFullString()}")

          }

        }
        
        // This case indicates that the packed graph does not contain a task with that taskName
        //
        // This case probably should not ever happen, but in case it does, handle it gracefully
        case None             => {  
          warn(s"Plan ${goal.comment} contains a reference to task ${goal.taskName}, but no task with that name could be found.")
        }
      }
            
    }
    goals.debug(s"Ended with ${goals.size} goals")
    return goals
    
  }

  private final case class Goal(taskName:String, realization:Realization, comment:String)
  
  private def initialGoalsFromPlans(packedGraph:PackedGraph): Seq[Goal] = {
    
    //val goals = new Goals(packedGraph)

    val goals = Seq.newBuilder[Goal]
    
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
      
      val baselineOnly = ducttape.workflow.Task.NO_REALIZATION
      val baselineComment = ducttape.workflow.Task.NO_REALIZATION.toString()
    
      for (taskName <- packedGraph.taskNames) {
        goals += Goal(taskName, baselineOnly, baselineComment)
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
            for (realization <- realizations) {
              goals += Goal(taskName, realization, comment)
            }
          }

        }
      }
    }
    return goals.result()
  }
 
  sealed trait Status
  final case object Failure extends Status
  final case object Underspecified extends Status
  final case class  Success(result:Realization) extends Status
  
  def unify(values:Seq[Status]): Status = {
    
    var resultSoFar:Status = Underspecified
    
    for (value <- values) {
      
      value match {
        
        case Failure               => return Failure
        case Underspecified        => /* Do nothing */
        case Success(realizationA) => {
          
          resultSoFar match {
            
            case Failure               => return Failure
            case Underspecified        => resultSoFar = Success(realizationA)
            case Success(realizationB) => resultSoFar = Success(Realization.union(realizationA, realizationB))
            
          }
        }
      }
      
    }
    
    return resultSoFar
  }
  
}