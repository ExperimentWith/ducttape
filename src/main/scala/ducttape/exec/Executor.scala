// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.exec

import collection._
import ducttape.graph.UnpackedGraph.Task
import ducttape.graph.traversal.Visitor
import ducttape.util.Shell
import ducttape.util.Files
import ducttape.workflow.Realization
//import ducttape.workflow.VersionedTask
//import ducttape.workflow.HyperWorkflow
//import ducttape.workflow.PlanPolicy
import ducttape.util.BashException
import grizzled.slf4j.Logging

// workflow used for viz
class Executor(val dirs: DirectoryArchitect,
               val packageVersioner: PackageVersioner,
//               val planPolicy: PlanPolicy,
               val locker: LockManager,
//               val workflow: HyperWorkflow,
               val alreadyDone: Set[(String,Realization)],
               val todo: Set[(String,Realization)],
               val submitter:Submitter,
               observers: Seq[ExecutionObserver] = Nil) extends Visitor with Logging {
  
//  val submitter = new Submitter(workflow.submitters)

  observers.foreach(_.init(this))

  override def visit(task: Task) {
    if (todo( (task.name, task.realization) )) {
      
      val taskEnv = new FullTaskEnvironment(dirs, packageVersioner, task)
      // first, acquire a lock
      System.err.println(s"Acquiring lock for ${task}")
      locker.acquireLock(taskEnv)
      
      taskEnv.fullSymlink match {
        case None => ;
        case Some(link) => {
          Files.symlink(taskEnv.where, link)
        }
      }

      // Note: If we just acquired the lock,
      // the LockManager will have just written ducttape_version.txt for us as well.
      // and moved any previous partial output
      
      try {
        // this task could have been completed by another ducttape process
        // while we were waiting on the lock
        if (!CompletionChecker.isComplete(taskEnv)) {
          
          System.err.println(s"Running ${task} in ${taskEnv.where.getAbsolutePath}")
          observers.foreach(_.begin(this, taskEnv))

          Files.mkdirs(taskEnv.where)          
          debug(s"Environment for ${task} is ${taskEnv.env}")
    
          // the "run" action of the submitter will throw if the exit code is non-zero
          submitter.run(taskEnv)
          
          def incompleteCallback(task: Task, msg: String) {
            System.err.println(s"${task}: ${msg}")
          }
          if (!CompletionChecker.isComplete(taskEnv, incompleteCallback)) {
            throw new BashException(s"${task}: Task completed, but did not satisfy post-conditions. Check output: ${taskEnv.where.getAbsolutePath}")
          }
        }
      } catch {
        case t: Throwable => {
          System.err.println(s"Failed ${task}: ${t.getMessage}")
          observers.foreach(_.fail(this, taskEnv))
          throw t
        }
      } finally {
        locker.releaseLock(taskEnv)
      }
      System.err.println(s"Completed ${task}")
      observers.foreach(_.succeed(this, taskEnv))
    } else {
      val taskEnv = new TaskEnvironment(dirs, task)
      observers.foreach(_.skip(this, taskEnv))
    }
  }
}
