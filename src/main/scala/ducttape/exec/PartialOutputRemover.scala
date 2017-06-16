// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.exec

import java.io.File

import ducttape.graph.UnpackedGraph.Task
import ducttape.graph.traversal.Visitor
import ducttape.workflow.Realization
import ducttape.util.Files

import grizzled.slf4j.Logging

/**
 * Moves incomplete or invalidated tasks to the attic before re-running them.
 * The CompletionChecker has already discovered which tasks are broken or partially completed.
 * 
 * partial is a list of the task/realizations that have partial output that needs to be moved
 *         (and are guaranteed to have version numbers)
 * broken is a list of the task/realizations that are partial but are missing information
 *        such as a version number and so are considered broken. these will be deleted
 */
class PartialOutputRemover(dirs: DirectoryArchitect,
                         partial: Set[(String,Realization)],
                         broken: Set[(String,Realization)],
                         locker: LockManager) extends Visitor with Logging {
  
  override def visit(task: Task) {
    
    debug("Considering %s".format(task))
    
    val taskEnv = new TaskEnvironment(dirs, task)
    val gotLock = locker.maybeAcquireLock(taskEnv)
    
    if (gotLock) {
      if (broken( (task.name, task.realization) )) {
        System.err.println("Removing broken partial output for %s".format(task))
        val origDir = dirs.assignDir(task)
        Files.deleteDir(origDir)
        
      } else if (partial( (task.name, task.realization) )) {
        System.err.println("Removing partial output for %s".format(task))
        val origDir = dirs.assignDir(task)
        Files.deleteDir(origDir)
      }
    } else {
      debug("Couldn't immediately get a lock for %s. We'll let the other process keep using it for now and try again later".format(task))
    }
  }
}
