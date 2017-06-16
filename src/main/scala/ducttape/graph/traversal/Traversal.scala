// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
package ducttape.graph.traversal

import ducttape.graph.UnpackedGraph.Task
import java.util.Comparator
import scala.collection.mutable.HashMap
import ducttape.graph.UnpackedGraph


sealed trait Traversal {
  def comparator(graph:UnpackedGraph): Comparator[Option[Task]]  
}

case object Arbitrary extends Traversal {
	override def comparator(graph:UnpackedGraph): Comparator[Option[Task]] = new Comparator[Option[Task]]() {
	  override def compare(a:Option[Task], b:Option[Task]): Int = 0
	}
}

case object BreadthFirst extends Traversal {
	override def comparator(graph:UnpackedGraph): Comparator[Option[Task]] = new BreadthFirstComparator(graph)
}

case object DepthFirst extends Traversal {
  override def comparator(graph:UnpackedGraph): Comparator[Option[Task]] = new DepthFirstComparator(graph)
}





private class BreadthFirstComparator(graph:UnpackedGraph) extends Comparator[Option[Task]] {

	private var next = graph.size + 1 

	private def nextID():Int = {
	  next -= 1
	  return next
	}

	private val id = new HashMap[Task, Int]

	private val root = graph.getRoot

	private def recursivelyProcess(task:Task): Unit = {
	  id.put(task, nextID())
	  for (dependency <- task.temporalDependencies) {
		  recursivelyProcess(dependency)
	  }
	}

	for (task <- root.values) {
		recursivelyProcess(task)
	}

	override def compare(a:Option[Task], b:Option[Task]): Int = {
			
	  val aID = a match {
	    case Some(task) => id.getOrElse(task, Int.MaxValue-1)
	    case None       => Int.MaxValue
	  }
	  
		val bID = b match {
	    case Some(task) => id.getOrElse(task, Int.MaxValue-1)
	    case None       => Int.MaxValue
	  }

		return aID.compare(bID)

	}

}  


private class DepthFirstComparator(graph:UnpackedGraph) extends Comparator[Option[Task]] {

	private var next = 0 

	private def nextID():Int = {
	  next += 1
		return next
	}

	private val id = new HashMap[Task, Int]

	private val root = graph.getRoot

	private def recursivelyProcess(task:Task): Unit = {
	  for (dependency <- task.temporalDependencies) {
		  recursivelyProcess(dependency)
	  }
	  id.put(task, nextID())
	}

	for (task <- root.values) {
		recursivelyProcess(task)
	}

	override def compare(a:Option[Task], b:Option[Task]): Int = {
			
	  val aID = a match {
	    case Some(task) => id.getOrElse(task, Int.MaxValue-1)
	    case None       => Int.MaxValue
	  }
	  
		val bID = b match {
	    case Some(task) => id.getOrElse(task, Int.MaxValue-1)
	    case None       => Int.MaxValue
	  }

		return aID.compare(bID)

	}

}
