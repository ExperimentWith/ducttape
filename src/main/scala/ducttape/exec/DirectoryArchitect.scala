// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.exec

import java.io.File
import ducttape.workflow.Realization
import ducttape.syntax.Namespace
//import ducttape.syntax.AbstractSyntaxTree.TaskDef
//import ducttape.syntax.AbstractSyntaxTree.BranchPointDef
//import ducttape.syntax.AbstractSyntaxTree.Unbound
//import ducttape.syntax.AbstractSyntaxTree.Literal
//import ducttape.syntax.AbstractSyntaxTree.LiteralSpec
//import ducttape.syntax.AbstractSyntaxTree.ConfigVariable
//import ducttape.syntax.AbstractSyntaxTree.Spec
//import ducttape.syntax.FileFormatException
import ducttape.util.Environment
import ducttape.util.Files
import ducttape.workflow.Task.NO_REALIZATION
//import ducttape.workflow.RealTask
//import ducttape.workflow.VersionedTask
import ducttape.graph.UnpackedGraph.Task
import ducttape.graph.UnpackedGraph.Input
import ducttape.graph.UnpackedGraph.Output
import ducttape.graph.UnpackedGraph.Param
import ducttape.graph.UnpackedGraph.Literal
import ducttape.graph.UnpackedGraph.Reference
import ducttape.graph.UnpackedGraph.GlobReference
import ducttape.graph.UnpackedGraph.ValueBearingNode

import grizzled.slf4j.Logging

class DirectoryArchitect(val flat: Boolean,
                         //val versionedTasks: Boolean,
                         val confName: Option[String],
                         val workflowBaseDir: File) extends Logging { //,
  
  val confBaseDir = Files.normalize(workflowBaseDir)
  
  val versionHistoryDir = new File(confBaseDir, ".versions")
  def assignVersionDir(workflowVersion: Int) = new File(versionHistoryDir, workflowVersion.toString)
  
  val xdotFile = new File(confBaseDir, ".xdot")
  val dbFile = new File(confBaseDir, ".db")

  //TODO: Change taskName from String to Namespace
  def assignPackedDir(taskName: String, relativeTo: File = confBaseDir): File = {
    // note: taskName will include various group names delimited by / such that they will create subdirectories
    new File(relativeTo, taskName.toString).getAbsoluteFile
  }
  
  // assign a version and realization-specific task directory (convenience method)
  def assignDir(task: Task): File = assignDir(task, relativeTo=confBaseDir)

  // assign a version and realization-specific task directory (convenience method)
  def assignDir(task: Task, relativeTo: File): File
    = assignDir(task, task.realization, relativeTo, task.realization.toCanonicalString(true))
  
  // assign a version and realization-specific task directory (convenience method)
  def assignDir(task: Task, realization: Realization, //workflowVersion: Int,
                relativeTo: File = confBaseDir): File
    = assignDir(task, realization, relativeTo, realization.toCanonicalString(true))
  
  // assign a version and realization-specific task directory
  private def assignDir(task: Task,
                        realization: Realization,
                        //workflowVersion: Int,
                        relativeTo: File,
                        realName: String): File = {

    val packedDir = assignPackedDir(task.name, relativeTo)
    if (flat) {
      if (realization != NO_REALIZATION) { // TODO: Statically check this elsewhere, too?
        throw new RuntimeException("workflow may not contain any branchpoints if flat structure is being used")
      }
      packedDir
    } else { // using hyper structure
      val realizationDir = new File(packedDir, realName).getAbsoluteFile
//      if (versionedTasks) {
//        new File(realizationDir, workflowVersion.toString).getAbsoluteFile
//      } else {
        realizationDir
//      }
    }
  }

  // this symlink includes *all* branch points in the current workflow, even
  //   if they're baseline branches. this symlink may include more components
  //   as the workflow evolves, but should be used ONLY by the user, not
  //   internally by ducttape
  // will return None if we are using flat structure since we will never
  //   have multiple realizations there OR if the symlink would be the same as the original dir
  def assignLongSymlink(task: Task): Option[File] = {
    if (flat) {
      None
    } else {
      val orig = assignDir(task, task.realization, confBaseDir, task.realization.toString)
      val link = assignDir(task, task.realization, confBaseDir, task.realization.toFullString())
      if (orig.getAbsolutePath == link.getAbsolutePath || task.realization.hasSingleBranchBaseline) {
        None
      } else {
        Some(link)
      }
    }  
  }
  
//  val atticDir = new File(confBaseDir, ".attic")
//  def assignAtticDir(task: Task)
//    = assignDir(task, relativeTo=new File(atticDir, task.version.toString))

  def assignPackagesDir() = new File(confBaseDir, ".packages")

  // the directory where various versions of a software package will get built
  def assignBuildPackageDir(packageName: String): File = {
    new File(assignPackagesDir(), packageName.toString)
  }
  
  def assignBuildHeadFile(packageName: String) = new File(assignBuildPackageDir(packageName), "HEAD")

  // the directory where a specific version of a software package will get built
  def assignBuildDir(packageName: String, packageVersion: String): File = {
    val packageDir = assignBuildPackageDir(packageName)
    new File(packageDir, packageVersion)
//    return packageDir
  }

//  def assignOutFile(spec: Spec, task: Task, realization: Realization): File = {
//    debug(s"Assigning outfile for ${spec}")
//    val taskDir = assignDir(task, realization)
//    spec.rval match {
//      case Unbound() => { // user didn't specify a name for this output file
//        new File(taskDir, spec.name) // will never collide with stdout.txt since it can't contain dots
//      }
//      case Literal(filename) => { // the user told us what name to use for the file
//        if (Files.isAbsolute(filename)) {
//          // TODO: Warn user about this unusual pattern...
//          new File(Files.normalize(filename))
//        } else {
//          new File(taskDir, filename)
//        }
//      }
//    }
//  }
//
//  def resolveLiteralPath(path:String): File = {
//    val result = Files.isAbsolute(path) match {
//      case true => new File(Files.normalize(path))
//      // relative paths are resolved relative to the directory
//      // **of the file in which this literal was declared**
//      // this could be the workflow file or the config file
//      case false => new File(spec.declaringFile.getParentFile, path)
//    }
//    
//    return result
//  }
//  
//  // resolve a literal *input* path
//  def resolveLiteralPath(spec: LiteralSpec): File = {
//    val path = spec.rval.value
//    val result = resolveLiteralPath(path)
//    return result
//  }
  
  def getFile(node:ValueBearingNode, task:Task): Seq[File] = {
    val parentDir = assignDir(task)
    val result = getFile(node, task.realization, parentDir)
    
    return result    
  }
  
  def getFile(variableName:String, task:Task): Seq[File] = {
    debug(s"Assigning $$${variableName}@${task.name}[${task.realization}]")
    
    val node = task.get(variableName)
    val result = getFile(node, task)
    
    return result
  }
  
  
  def getFile(node:ValueBearingNode, realization:Realization, parentDir:File): Seq[File] = {
   val result:Seq[File] = node match {
      case Literal(filename) => { // the user told us what name to use for the file
        if (Files.isAbsolute(filename)) {
          // TODO: Warn user about this unusual pattern...
          Seq(new File(Files.normalize(filename)))
        } else {
          Seq(new File(parentDir, filename))
        }
      }
      case Reference(variableName, task) => getFile(variableName, task)
      case GlobReference(variableName, tasks) => tasks.flatMap{ task => getFile(variableName, task) }
    }
    
    return result
  }
  
  def getFile(input:Input, realization:Realization, parentDir:File): Seq[File] = {
    return getFile(input.value, realization, parentDir)
  }

  def getFile(param:Param, realization:Realization, parentDir:File): Seq[File] = {
    return getFile(param.value, realization, parentDir)
  }
  
  def getFile(output:Output, realization:Realization, parentDir:File): Seq[File] = {
    return getFile(output.value, realization, parentDir)
  }
  
  
  
//  def getInFile(mySpec: Spec,
//                realization: Realization,
//                srcSpec: Spec,
//                srcTaskDefOpt: Option[TaskDef],
//                srcRealization: Realization
//                //srcVersion: Int
//                ): File = {
//
//    srcTaskDefOpt match {
//      // no source task? this better be a literal
//      case None => srcSpec.rval match {
//        // gah, erasure!
//        case Literal(path) => resolveLiteralPath(srcSpec.asInstanceOf[LiteralSpec])
//        case _ => throw new RuntimeException(s"No source task found for spec ${mySpec} with source ${srcSpec}")
//      }
//      
//      // has a source task? just recover the output path in the same way as when we originally produced it
//      case Some(srcTaskDef) => assignOutFile(srcSpec, srcTaskDef, srcRealization)
//    }
//  }
  
  def getTempActionDir(actionName: String) = {
    val f = File.createTempFile("ducttape", actionName)
    f.delete() // delete file
    Files.mkdirs(f) // and make it a directory instead
    f
  }
}

object DirectoryArchitect {
  val builtinsDir = new File(Environment.InstallDir, "builtins")
}