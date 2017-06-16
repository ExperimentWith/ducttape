// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.graph.traversal

import ducttape.graph.UnpackedGraph.Task
import grizzled.slf4j.Logging
import java.util.concurrent.ConcurrentSkipListSet
import java.util.concurrent.PriorityBlockingQueue
import ducttape.graph.UnpackedGraph

class UnpackedGraphWalker(val graph:UnpackedGraph, val traversal:Traversal) extends Walker[Task] with Logging {

  override val size = graph.size
  
  /** 
   * Priority queue of tasks that can be run.
   * 
   * If a task is in this priority queue, that means that every task it depends on has already completed.
   */
  val queue:PriorityBlockingQueue[Option[Task]] = UnpackedGraphWalker.initializeQueue(graph, traversal)
  
  /** Given a task, returns the set of tasks that depend on it. */
  val dependencies:Map[Task, Set[Task]] = UnpackedGraphWalker.initializeDependencyMap(graph)
  
  val completedTasks = new ConcurrentSkipListSet[Task]
  
  
  /** Get the next traversable item. Returns None when there are no more elements */
  protected def take(): Option[Task] = {
    return queue.take()
  }

  /** Callers must use this method to notify walker that caller is done with each
   *  item so that walker can traverse its dependends
   *  continue: False indicates that no dependents of the specified item should be traversed */
  protected def complete(item: Task, continue: Boolean = true) : Unit = {
    completedTasks.add(item)
    
    if (continue) {
      val tasks = dependencies.getOrElse(item, Set())
      for (dependentTask <- tasks) {
        var success = true
        for (dependency <- dependentTask.temporalDependencies) {
          success = success || completedTasks.contains(dependency)
        }
        if (success) {
          queue.add(Some(dependentTask))
        }
      }
    }
    
    if (completedTasks.size() == size) {
      queue.add(None)
    }
    
  }
  
}




object UnpackedGraphWalker {

  def initializeQueue(graph:UnpackedGraph, traversal:Traversal): PriorityBlockingQueue[Option[Task]] = {
    
    val comparator = traversal.comparator(graph)
    val queue:PriorityBlockingQueue[Option[Task]] = new PriorityBlockingQueue[Option[Task]](graph.size, comparator)
    
    def recursivelyProcess(task:Task): Unit = {
      val dependencies = task.temporalDependencies
      if (dependencies.isEmpty) {
        queue.add(Some(task))
      } else {
        for (dependency <- dependencies) {
          recursivelyProcess(dependency)
        }
      }
    }
    
    for (task <- graph.getRoot.values) {
      recursivelyProcess(task)
    }
    
    return queue
  }
 
  
  def initializeDependencyMap(graph:UnpackedGraph): Map[Task, Set[Task]] = {
    
    import scala.collection.mutable.HashMap
    import scala.collection.mutable.HashSet
    
    val data = new HashMap[Task, HashSet[Task]]
    
    def recursivelyProcess(task:Task): Unit = {
      for (dependency <- task.temporalDependencies) {
        val set = data.getOrElseUpdate(dependency, new HashSet[Task])
        set.add(task)
    	  recursivelyProcess(dependency)
      }
    }

    for (task <- graph.getRoot.values) {
      recursivelyProcess(task)
    }
    
    val map:HashMap[Task, Set[Task]] = data.map{ case(key,value) => key -> value.toSet}
    val result:Map[Task, Set[Task]] = map.toMap
    return result
  }
  
  
  
}