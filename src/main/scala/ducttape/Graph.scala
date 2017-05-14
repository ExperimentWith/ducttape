package ducttape

import ducttape.syntax.{AbstractSyntaxTree => ast}
import ducttape.syntax.BashCode
import ducttape.workflow.Branch
import ducttape.workflow.BranchFactory
import ducttape.workflow.BranchPoint


object Graph {
  
  sealed trait Node
  final case class CrossProduct(value:Set[Branch]) extends Node
  final case class Via(reach:Task, via:CrossProduct) extends Node
  final case class Plan(goals:Seq[Via]) extends Node
  final case class Task(name:String, code:BashCode, inputs:Seq[Spec], params:Seq[Spec], outputs:Seq[Spec], packages:Seq[Spec]) extends Node
  final case class ShellScript(code: String, vars: Set[String]) extends Node
  final case class Packages(value:Seq[Package]) extends Node

  sealed trait ValueBearingNode extends Node
  final case class Literal(value:String) extends ValueBearingNode
  final case class BranchNode(branch:Branch, value:ValueBearingNode) extends ValueBearingNode
  final case class BranchPointNode(branchPoint:BranchPoint, branches:Seq[BranchNode]) extends ValueBearingNode
  final case class Spec(name:String, value:ValueBearingNode) //extends ValueBearingNode
  final case class Reference(variableName:String, taskName:Option[String], grafts:Option[Seq[Branch]]) extends ValueBearingNode
  final case class Glob(nodes:Seq[ValueBearingNode]) extends ValueBearingNode


  
  def toGraphvizID(taskName:String) : String = {
    return s"""task ${taskName}"""
  }
  
  def toGraphvizID(task:Task) : String = toGraphvizID(task.name)
  
  def toGraphviz(tasks:scala.collection.Map[String,Task]) : String = {
    val s = new StringBuilder(capacity=1000)
    s ++= "digraph G {\n\n"
    
    def recursivelyBuildTask(targetID:String, rhs:ValueBearingNode, s:StringBuilder) : Unit = {
      rhs match {
        case Literal(value) => {
          val id = targetID + " value " + value
          s.append("\t\t\"").append(id).append("""" [margin="0.01,0.01", height=0.0, width=0.0, fontcolor=white, fillcolor=darkgreen, color=black, shape=box, style="filled", label="""").append(value).append("\"]\n")
          s.append("\t\t\"").append(id).append("\" -> \"").append(targetID).append("\"\n")
        }
        
        case BranchNode(branch, value) => {
          val id = targetID + " branch " + branch.name
          s.append("\t\t\"").append(id).append("""" [margin="0.01,0.01", height=0.0, width=0.0, fontcolor=white, fillcolor=blue, color=blue, style="filled", label="""").append(branch.name).append("\"]\n")
          recursivelyBuildTask(id, value, s)
          s.append("\t\t\"").append(id).append("\" -> \"").append(targetID).append("\"\n")
        }
        
        case BranchPointNode(branchPoint, branches) => {
          val id = targetID + " branchPoint " + branchPoint.name
          s.append("\t\t\"").append(id).append("""" [margin="0.01,0.01", height=0.0, width=0.0, fontcolor=white, fillcolor=red, color=red, shape=box, style="filled", label="""").append(branchPoint.name).append("\"]\n")
          for (branch <- branches) {
            recursivelyBuildTask(id, branch, s)
          }
          s.append("\t\t\"").append(id).append("\" -> \"").append(targetID).append("\"\n")
        }
        
        case _ => {}
      }
      
      
    }  

    
    def recursivelyConnect(targetID:String, rhs:ValueBearingNode, s:StringBuilder) : Unit = {
      rhs match {
        
        case BranchNode(branch, value) => {
          val id = targetID + " branch " + branch.name
          recursivelyConnect(id, value, s)
        }
        
        case BranchPointNode(branchPoint, branches) => {
          val id = targetID + " branchPoint " + branchPoint.name
          for (branch <- branches) {
            recursivelyConnect(id, branch, s)
          }
        }
        
        case Reference(variable, taskName, grafts) => {
          val id = taskName match {
            case Some(task) => variable + "@" + toGraphvizID(task)
            case None       => "global " + variable
          }
          val label = grafts match {
            case Some(grafts) if grafts.size > 0 => grafts.map{ graft => graft.toString}.mkString("[ label=\"", ",", "\" ]")
            case _ => ""
          }
          s.append("\t\"").append(id).append("\" -> \"").append(targetID).append("\" ").append(label).append("\n")
        }
        
       
        case _ => {}
      }
      
      
    }      
      
    for (task <- tasks.values) {
      val taskID = toGraphvizID(task)
      
      s.append("\tsubgraph cluster_").append(task.name).append(" {\n")
      s.append("\t\t\"").append(taskID).append("""" [margin="0.01,0.01", height=0.0, width=0.0, fontcolor=white, fillcolor=orange, color=orange, style="filled,rounded", label="""").append(task.name).append("\"]\n")
      
      for (input <- task.inputs) {
        val inputID = input.name + "@" + taskID 
        s.append("\t\t\"").append(inputID).append("""" [margin="0.01,0.01", height=0.0, width=0.0, fontcolor=black, fillcolor=white, color=black, label="""").append(input.name).append("\"]\n")
        s.append("\t\t\"").append(inputID).append("\" -> \"").append(taskID).append("\"\n")
        recursivelyBuildTask(inputID, input.value, s)
      }

      for (param <- task.params) {
        val paramID = param.name + "@" + taskID
        s.append("\t\t\"").append(paramID).append("""" [margin="0.01,0.01", height=0.0, width=0.0, fontcolor=black, fillcolor=white, color=black, shape="box", label="""").append(param.name).append("\"]\n")
        s.append("\t\t\"").append(paramID).append("\" -> \"").append(taskID).append("\"\n")
        recursivelyBuildTask(paramID, param.value, s)
      }

      for (output <- task.outputs) {
        val outputID = output.name + "@" + taskID
        s.append("\t\t\"").append(outputID).append("""" [margin="0.01,0.01", height=0.0, width=0.0, fontcolor=black, fillcolor=white, color=black, label="""").append(output.name).append("\"]\n")
        s.append("\t\t\"").append(taskID).append("\" -> \"").append(outputID).append("\"\n")
      }
      
      s.append("\t\tcolor=black\n")
      s.append("\t}\n\n")
    }  
    
    for (task <- tasks.values) {
      val taskID = toGraphvizID(task)
      
      for (input <- task.inputs) {
        val inputID = input.name + "@" + taskID 
        recursivelyConnect(inputID, input.value, s)
      }

      for (param <- task.params) {
        val paramID = param.name + "@" + taskID
        recursivelyConnect(paramID, param.value, s)
      }     
    }
    
    
    s ++= "}\n"
    
    return s.result
  }
  
  def build(workflow:ast.WorkflowDefinition, branchFactory:BranchFactory) : scala.collection.Map[String,Task] = {
    
    val taskList:Seq[ast.TaskDef] = 
      (workflow.tasks ++ workflow.functionCallTasks)                  // gather all TaskDef objects from the workflow definition
    
    val taskNameTuples:Seq[(String, Task)] = taskList.map{ taskDef => // for each TaskDef in taskList:
      val task:Task   = build(taskDef, branchFactory)                 //   build a sub-graph (of type Task)
      val name:String = task.name                                     //   get the name of the TaskDef; for the moment we ignore namespaces (XXX: ideally fix this) 
      (name, task)                                                    //   return a tuple of type (String, Task) 
    }
    
    val taskMap:Map[String, Task] = taskNameTuples.toMap              // convert the sequence of tuples into a map, where the keys are task names and the values are Task graph objects
    
    return taskMap
  }
  
  def build(taskDef:ast.TaskDef, branchFactory:BranchFactory) : Task = {
    
    val inputs   = taskDef.inputs.map  { spec => recursivelyProcessSpec(spec, branchFactory) }
    val params   = taskDef.params.map  { spec => recursivelyProcessSpec(spec, branchFactory) }
    val outputs  = taskDef.outputs.map { spec => recursivelyProcessSpec(spec, branchFactory) }
    val packages = taskDef.packages.map{ spec => recursivelyProcessSpec(spec, branchFactory) }
    val code     = taskDef.commands
    val name     = taskDef.name.name                                  //   get the name of the TaskDef; for the moment we ignore namespaces (XXX: ideally fix this) 
    
    return new Task(name, code, inputs, params, outputs, packages)
  }
  
  def build(specs:Seq[ast.Spec], branchFactory:BranchFactory) : Seq[Spec] = {
    return specs.map{ spec => recursivelyProcessSpec(spec, branchFactory) }
  }
  

  def recursivelyProcessSpec(spec:ast.Spec, branchFactory:BranchFactory) : Spec = {
    val name = spec.name
    
    val value:ValueBearingNode = spec.rval match {
      case astNode:ast.Unbound => Literal(name)
      case astNode:ast.Literal => Literal(astNode.value)
      case astNode:ast.ConfigVariable          => Reference(astNode.value, None,                   None)
      case astNode:ast.ShorthandConfigVariable => Reference(name,          None,                   None)
      case astNode:ast.TaskVariable            => Reference(astNode.value, Some(astNode.taskName), None)
      case astNode:ast.ShorthandTaskVariable   => Reference(name,          Some(astNode.taskName), None)
      case astNode:ast.BranchGraft             => { 
        
        val branches = astNode.branchGraftElements.flatMap { graftElement => 
          if (graftElement.branchName == "*") {
            branchFactory.getAll(graftElement.branchPointName)
          } else {
            Seq(branchFactory(graftElement.branchName, graftElement.branchPointName))
          }
        }    
        
        val reference = astNode.taskName match {
          case Some(taskName) => Reference(astNode.variableName, Some(taskName), Some(branches))
          case None           => Reference(astNode.variableName, None,           Some(branches))
        }
        
        reference
      }
      case astNode:ast.ShorthandBranchGraft             => { 
        
        val branches = astNode.branchGraftElements.map {
          graftElement => branchFactory(graftElement.branchName, graftElement.branchPointName)
        }
        
        val reference = Reference(name, Some(astNode.taskName), Some(branches))
        
        reference
      }
      case astNode:ast.SequentialBranchPoint   => { 
      
        val branchPoint:BranchPoint = astNode.branchPointName match {
          case Some(bp) => branchFactory.getBranchPoint(bp)
          case None     => throw new ducttape.syntax.FileFormatException("Branch point name is required", astNode)
        }
        
        val branches = Seq.newBuilder[BranchNode]
        val sequence = astNode.sequence
        for ( number <- sequence.start to sequence.end by sequence.increment) {
          val value = number.toString
          val literal = Literal(value)
          val branch = branchFactory(value, branchPoint)
          val branchNode = BranchNode(branch, literal)
          
          branches += branchNode
        }
        
       BranchPointNode(branchPoint, branches.result)
        
      }
      case astNode:ast.BranchPointDef          => {
        
        val branchPoint:BranchPoint = astNode.name match {
          case Some(bp) => branchFactory.getBranchPoint(bp)
          case None     => throw new ducttape.syntax.FileFormatException("Branch point name is required", astNode)
        }
        
        val branches = astNode.specs.map{ spec => 
          
          val specNode = recursivelyProcessSpec(spec, branchFactory)
          
          val branchName = spec.name
          val branch = branchFactory(branchName, branchPoint)
          
          BranchNode(branch, specNode.value)
          
        }
        
        BranchPointNode(branchPoint, branches)
      }

    }
    
    return Spec(name, value)
  }
 
  
}

