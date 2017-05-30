/*
package ducttape

import ducttape.{PackedGraph => packed}
import ducttape.syntax.BashCode

import ducttape.workflow.Realization
import scala.collection.Map


class UnpackedGraph {

}



object UnpackedGraph {

  sealed trait Node
  final case class Task(name:String, code:BashCode, realization:Realization) extends Node
  
  
  def recursivelyProcess(task:packed.Task, goals:Goals): Option[Task] = {
    
    
    
    return None
  }
  
  def unpack(packedGraph:PackedGraph, goals:Goals): Unit = {
    
    for (taskName <- goals.tasks) {
      
      val packedTask = packedGraph.task(taskName)

      recursivelyProcess(packedTask, goals) match {
        case Some(unpackedTask) => {}
        case None               => {}
      }
      
    }
    
    
    
  }
  
}
*/
