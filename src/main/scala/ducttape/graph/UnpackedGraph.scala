// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.graph

import ducttape.graph.{PackedGraph => packed}
import ducttape.syntax.{AbstractSyntaxTree => ast}
import ducttape.syntax.BashCode
import ducttape.workflow.Branch
import ducttape.workflow.Realization

import scala.collection.Map
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import grizzled.slf4j.Logging


class UnpackedGraph private(val goals:Goals) extends Logging {

  private[graph] val values = new HashMap[(packed.Task,Realization), UnpackedGraph.Task]
  
  private var root:UnpackedGraph.Root = null
  
  def getRoot:UnpackedGraph.Root = root
  
  def size = values.size
  
  def tasks = values.values
  
  def getTask(taskName:String, realization:Realization): Option[UnpackedGraph.Task] = {
  
    val packedTask = goals.packedGraph.task(taskName)
    
    packedTask match {
      case None => return None
      case Some(task) => {
        val tuple = (task, realization)
        return this.values.get(tuple)
      }
    }
    
  }
  
  def branchFactory = this.goals.packedGraph.branchFactory
  
  override def toString(): String = {
    
    val map = new HashMap[packed.Task, HashSet[UnpackedGraph.Task]]
    
    val packedTasks = values.map{ tuple => tuple._1._1 }
    
    values.foreach{ tuple => 
      val packedTask = tuple._1._1
      val unpackedTask = tuple._2
      
      val set = map.getOrElseUpdate(packedTask, new HashSet[UnpackedGraph.Task])
      set.add(unpackedTask)
    }

    UnpackedGraph.toGraphviz(map)
  }
  
  private def recursivelyProcess(task:packed.Task, realization:Realization): UnpackedGraph.Task = {
    
    val key = (task, realization)
    values.get(key) match {
      
      case Some(unpackedTask) => return unpackedTask /* We have already recursively processed this tuple */
      
      case None    => {
        
        val inputDependencies = new HashSet[UnpackedGraph.Task]
        val paramDependencies = new HashSet[UnpackedGraph.Task]
        
        val inputs  = recursivelyProcessInputs(task.inputs,   realization:Realization, inputDependencies)
        val params  = recursivelyProcessParams(task.params,   realization:Realization, paramDependencies)
        val outputs = recursivelyProcessOutputs(task.outputs, realization:Realization, task.name)
        val packages = recursivelyProcessPackages(task.packages, realization:Realization) 
        
        val specs = UnpackedGraph.Specs(inputs, params, outputs, packages)
        val dependencies = UnpackedGraph.Dependencies(temporal=inputDependencies.toSet, nontemporal=paramDependencies.toSet)
        
        val unpackedTask = UnpackedGraph.Task(task.name, task.code, realization, specs, dependencies, task.astNode)
        
        values.put(key, unpackedTask)
        
        return unpackedTask
      }
      
    }
  }
  
  private def recursivelyProcessInputs(inputs:Seq[packed.Spec], realization:Realization, inputDependencies:HashSet[UnpackedGraph.Task]): Seq[UnpackedGraph.Input] = {
    
    val results = Seq.newBuilder[UnpackedGraph.Input]
    
    for (input <- inputs) {
            
      val name = input.name
      val value = recursivelyProcess(input.value, goals.getRealization(input, realization), Some(inputDependencies))
      
      results += new UnpackedGraph.Input(name, value, input.astNode)
      
    }
    
    return results.result()
  }
  
   
  private def recursivelyProcessParams(params:Seq[packed.Spec], realization:Realization, paramDependencies:HashSet[UnpackedGraph.Task]): Seq[UnpackedGraph.Param] = {
    
    val results = params.map { param =>      
      val name = param.name
      val value = recursivelyProcess(param.value, goals.getRealization(param, realization), Some(paramDependencies))      
      new UnpackedGraph.Param(name, value, param.dotVariable, param.astNode)     
    }
    
    return results
  }

  private def recursivelyProcessOutputs(outputs:Seq[packed.Spec], realization:Realization, taskName:String): Seq[UnpackedGraph.Output] = {
    
    val results = outputs.map { output =>      
      val name = output.name
      val value = recursivelyProcess(output.value, goals.getRealization(output, realization))  
      value match {
        case literal:UnpackedGraph.Literal => new UnpackedGraph.Output(name, literal, output.astNode)
        case UnpackedGraph.Reference(variableName, task, astNode) => throw new ducttape.syntax.FileFormatException(s"A reference was made in output variable $$${name}@${taskName} to $$${variableName}@${task.name}, but output variables aren't allowed to reference other variables", astNode)
        case UnpackedGraph.GlobReference(variableName, _, astNode) => throw new ducttape.syntax.FileFormatException(s"A reference was made in output variable $$${name}@${taskName} to $$${variableName}, but output variables aren't allowed to reference other variables", astNode)
      }
         
    }
    
    return results
  }
  
    private def recursivelyProcessPackages(packages:Seq[packed.Spec], realization:Realization): Seq[UnpackedGraph.Literal] = {
    
    val results = packages.map { spec => new UnpackedGraph.Literal(spec.name, spec.astNode) }   
    
    return results
  }
  
  
  private def recursivelyProcessVariableReference(task:PackedGraph.Task, realization:Realization, graft:Option[Realization], variableName:String): UnpackedGraph.Task = {
    
    if (task.containsVariable(variableName)) {

      val realizationAfterGrafting:Realization = graft match {
        
        case None                   => realization
        
        case Some(graftRealization) => {
          val branchSet = new HashSet[Branch]
          graftRealization.branches.foreach{ branch => branchSet.add(branch) }
          realization.branches.foreach{ branch => if (! graftRealization.explicitlyRefersTo(branch.branchPoint)) branchSet.add(branch) }
          Realization.fromUnsorted(branchSet.toSeq)
        }
        
      }
      
      val parentTaskRealization = goals.getParentRealization(task.name, realizationAfterGrafting) 
      
      return recursivelyProcess(task, parentTaskRealization)
      
    } else {
      
      throw new RuntimeException(s"A reference to ${variableName}@${task.name} was found, but no variable with that name is defined for task ${task.name}")
      
    }
  }
  
  private def recursivelyProcess(node:packed.ValueBearingNode, realization:Realization, possibleDependencySet:Option[HashSet[UnpackedGraph.Task]] = None): UnpackedGraph.ValueBearingNode = {
    
    node match {
      
      case packed.Literal(value, astNode) => return UnpackedGraph.Literal(value, astNode)
      
      case packed.BranchPointNode(branchPoint, branchNodes, astNode) => {
        
        for (branchNode <- branchNodes) {          
          if (realization.explicitlyRefersTo(branchNode.branch)) {
            return recursivelyProcess(branchNode, realization)
          }          
        }
        
        throw new RuntimeException(s"While unpacking the workflow graph, a branchpoint (${branchPoint.name}) was encountered that is incompatible with the realization ${realization.toFullString(false)}")
      }
      
      case packed.BranchNode(branch, value, astNode) => return recursivelyProcess(value, realization)
      
      case packed.Reference(variableName, possibleTaskName, graftsList, astNode) => {
        possibleTaskName match {
          
          // Look up a task
          case Some(taskName) => {
            
            goals.packedGraph.task(taskName) match {
            
              case Some(task) => {
            	  
                if (graftsList.isEmpty) {
                		
                	val unpackedTask = recursivelyProcessVariableReference(task, realization, graft=None, variableName)
                	
                	possibleDependencySet match {
                	  case None      => /* Do nothing */
                	  case Some(set) => set.add(unpackedTask)
                	}
                	
                	return UnpackedGraph.Reference(variableName, unpackedTask, astNode)
                		
                } else {
                  
                  val unpackedTasks = graftsList.map{ graftRealization => 
                		val newRealization = Realization.applyGraft(graft=graftRealization, original=realization) //; println(graftRealization.toFullString(false))
                		val unpackedTask = recursivelyProcessVariableReference(task, newRealization, graft=Some(graftRealization), variableName)
                		
                	  possibleDependencySet match {
                	    case None      => /* Do nothing */
                	    case Some(set) => set.add(unpackedTask)
                	  }
                		
                		unpackedTask
                	}
                	
                  if (unpackedTasks.size == 1) {
                    return UnpackedGraph.Reference(variableName, unpackedTasks.head, astNode)
                  } else {
                    return UnpackedGraph.GlobReference(variableName, unpackedTasks, astNode)
                  }
                	  
                }
                
              }
              
              case None       => {                
                throw new RuntimeException(s"A reference to task ${variableName} was found, but no task with that name exists")
              }
              
            }
            
          }
          
          // Look up a global variable
          case None           => {
          
            goals.packedGraph.global(variableName) match {
              
              case Some(globalVariable) => {
                val spec = globalVariable.value
                return recursivelyProcess(spec.value, realization)
              }
              
              case None                 => {
                throw new RuntimeException(s"A reference to global variable ${variableName} was found, but no global variable with that name exists")
              }
              
            }
            
          }
          
        }
        
      
        
      }
      
    }
    
  }
  
  
  
  
}



object UnpackedGraph extends Logging {

  sealed trait Node
  
  sealed trait Spec extends Node
  final case class Input(name:String, value:ValueBearingNode, astNode:ast.ASTType) extends Spec
  final case class Param(name:String, value:ValueBearingNode, dotVariable:Boolean, astNode:ast.ASTType) extends Spec
  final case class Output(name:String, value:Literal, astNode:ast.ASTType) extends Spec
  
  final case class Specs(inputs:Seq[Input], params:Seq[Param], outputs:Seq[Output], packages:Seq[Literal]) extends Node
  final case class Dependencies(temporal:Set[Task], nontemporal:Set[Task]) extends Node
  
  final case class Task(name:String, code:BashCode, realization:Realization, specs:Specs, directDependencies:Dependencies, astNode:ast.ASTType) extends Node with Comparable[Task] {
    override def toString(): String = {
      return s"${name}_${realization}"
    }
    
    override def compareTo(other:Task):Int = {
      return this.toString().compareTo(other.toString())  
    }
    
    def temporalDependencies:Iterable[Task] = directDependencies.temporal
    
    def get(variableName:String): ValueBearingNode = {
      for (input  <- specs.inputs)  { if (input.name  == variableName) return input.value  }
      for (param  <- specs.params)  { if (param.name  == variableName) return param.value  }
      for (output <- specs.outputs) { if (output.name == variableName) return output.value }
      throw new RuntimeException(s"A reference was made to $$${variableName}@${name} with realization ${realization}, but no such variable was found at that task.")
    }
    
    def allDotParams:Seq[Param] = specs.params.filter{ param => param.dotVariable==true }
  }
  
  final case class Root(values:Seq[Task])
  
  
  sealed trait ValueBearingNode extends Node
  final case class Literal(value:String, astNode:ast.ASTType) extends ValueBearingNode
  final case class Reference(variableName:String, value:Task, astNode:ast.ASTType) extends ValueBearingNode
  final case class GlobReference(variableName:String, values:Seq[Task], astNode:ast.ASTType) extends ValueBearingNode
  
  //def toGraphviz(tasks:Iterable[UnpackedGraph.Task]): String = {
  def toGraphviz(tasks:HashMap[packed.Task, HashSet[UnpackedGraph.Task]]): String = {
	  val s = new StringBuilder(capacity=1000)
    s ++= "digraph G {\n\n"
    s.append("\tnodesep=2\n\tranksep=2\n")
    
    
    def id(task:Task):String = return s"""task ${task} realization ${task.realization}"""
    
    def processLiterals(targetID:String, rhs:ValueBearingNode, s:StringBuilder) : Unit = {
      
      rhs match {
        case Literal(value, astNode) => {
          val id = targetID + " value " + value
          s.append("\t\t\"").append(id).append("""" [margin="0.01,0.01", height=0.0, width=0.0, fontcolor=black, fillcolor=darkseagreen2, color=black, shape=box, style="filled", label="""").append(value).append("\"]")
          astNode.positionString(s, prefix=" // Declared in ", suffix="\n")
          s.append("\t\t\"").append(id).append("\" -> \"").append(targetID).append("\"\n")
        }
        
        case _ => {}
      }
      
    }
    
    def processReferences(targetID:String, rhs:ValueBearingNode, s:StringBuilder) : Unit = {
      rhs match {
        case Reference(variableName, task, astNode) => {
          val variableID = variableName + "@" + id(task)
          val intermediateID = "from " + variableID + " to " + targetID
          s.append("\t\"").append(intermediateID).append("""" [margin="0.0,0.0", height=0.0, width=0.0, fillcolor=black, color=black, style="filled", label=""]""")
          astNode.positionString(s, prefix=" // Declared in ", suffix="\n")
          s.append("\t\t\"").append(variableID).append("\" -> \"").append(intermediateID).append("\" [arrowhead=\"none\"]\n")
          s.append("\t\t\"").append(intermediateID).append("\" -> \"").append(targetID).append("\"\n")
        }
        
        case GlobReference(variableName, tasks, astNode) => {
        	for (task <- tasks) {
            val variableID = variableName + "@" + id(task)
            val intermediateID = "from " + variableID + " to " + targetID
            s.append("\t\"").append(intermediateID).append("""" [margin="0.0,0.0", height=0.0, width=0.0, fillcolor=black, color=black, style="filled", label=""]""")
            astNode.positionString(s, prefix=" // Declared in ", suffix="\n")
            s.append("\t\t\"").append(variableID).append("\" -> \"").append(intermediateID).append("\" [arrowhead=\"none\"]\n")
            s.append("\t\t\"").append(intermediateID).append("\" -> \"").append(targetID).append("\"\n")
        	}
        }
        
        case _ => {}
      }
    } 
    
    for (packedTask <- tasks.keySet) {
    	s.append("\tsubgraph {\n")

    	for (task <- tasks(packedTask)) {      
    		s.append("\tsubgraph \"cluster_").append(task).append("\" {\n")
    		s.append("\tnodesep=1\n\tranksep=1\n")
    		
    		val taskID = id(task)

    		s.append("\t\t\"").append(taskID).append("""" [margin="0.01,0.01", height=0.0, width=0.0, fontcolor=white, fillcolor=orange, color=orange, style="filled,rounded", label="""").append(task.name).append("\"]\n")

    		for (input <- task.specs.inputs) {
    			val inputID = input.name + "@" + taskID 
    					s.append("\t\t\"").append(inputID).append("""" [margin="0.01,0.01", height=0.0, width=0.0, fontcolor=black, fillcolor=white, color=black, style=filled, label="""").append(input.name).append("\"]\n")
    					s.append("\t\t\"").append(inputID).append("\" -> \"").append(taskID).append("\"\n")
    					processLiterals(inputID, input.value, s)
    		}

    		for (param <- task.specs.params) {
    			val paramID = param.name + "@" + taskID
    					s.append("\t\t\"").append(paramID).append("""" [margin="0.01,0.01", height=0.0, width=0.0, fontcolor=black, fillcolor=white, color=black, style=filled, shape="box", label="""").append(param.name).append("\"]\n")
    					s.append("\t\t\"").append(paramID).append("\" -> \"").append(taskID).append("\"\n")
    					processLiterals(paramID, param.value, s)
    		}

    		for (output <- task.specs.outputs) {
    			val outputID = output.name + "@" + taskID
    					s.append("\t\t\"").append(outputID).append("""" [margin="0.01,0.01", height=0.0, width=0.0, fontcolor=black, fillcolor=white, color=black, style=filled, label="""").append(output.name).append("\"]\n")
    					s.append("\t\t\"").append(taskID).append("\" -> \"").append(outputID).append("\"\n")
    					//recursivelyBuildTask(outputID, output.value, s)
    		}

    		s.append("\t\t").append("""label=< <TABLE BORDER="0">""").append("\n")
    		for (branch <- task.realization.branches) {
    			s.append("\t\t\t<TR>\n")
    			s.append("\t\t\t\t").append("""<TD BGCOLOR="red" COLOR="red"><FONT COLOR="white">""").append(branch.branchPoint.name).append("</FONT></TD>\n")
    			s.append("\t\t\t\t").append("""<TD BGCOLOR="blue" COLOR="blue"><FONT COLOR="white">""").append(branch.name).append("</FONT></TD>\n")
    			s.append("\t\t\t</TR>\n")
    		}
    		s.append("\t\t").append("</TABLE> >\n\n")

    		s.append("\t\tcolor=black\n\t\tfillcolor=white\n\t\tstyle=filled\n")
    		s.append("\t}\n\n") 

    	}
    	s.append("\t}\n")
    }

    for (packedTask <- tasks.keySet) {
    	for (task <- tasks(packedTask)) {  
    	  val taskID = id(task)
    	  
    		for (input <- task.specs.inputs) {
    			val inputID = input.name + "@" + taskID 
    					processReferences(inputID, input.value, s)
    		}

    		for (param <- task.specs.params) {
    			val paramID = param.name + "@" + taskID
    					processReferences(paramID, param.value, s)
    		}        
      }
    }

    
    s ++= "}\n"
    return s.result()
  }
  
  def unpack(goals:Goals): UnpackedGraph = {
    
    val unpackedGraph = new UnpackedGraph(goals)
    
    val rootTasks = goals.map{ case (packedTask, realization) =>       
      unpackedGraph.recursivelyProcess(packedTask, realization)       
     }

    unpackedGraph.root = Root(rootTasks.toSeq)
    
    return unpackedGraph
    
  }
  
}

