// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.exec

import java.io.File

import collection._

import ducttape.graph.UnpackedGraph
import ducttape.graph.UnpackedGraph.Literal
import ducttape.graph.UnpackedGraph.Task
import ducttape.graph.traversal.Visitor
import ducttape.syntax.FileFormatException
import ducttape.syntax.AbstractSyntaxTree._
import ducttape.workflow.Branch
import ducttape.workflow.Realization
//import ducttape.workflow.VersionedTask
//import ducttape.workflow.SpecTypes.ResolvedSpec
import ducttape.util.Files

import grizzled.slf4j.Logging

class InputChecker(dirs: DirectoryArchitect, graph:UnpackedGraph) extends Visitor with Logging {

  val errors = new mutable.ArrayBuffer[Exception]

  override def visit(task: Task) {
    for (input <- task.specs.inputs) {
      input.value match {
        case node:Literal => {
          val files = dirs.getFile(node, task)
          for (file <- files) {
        	  debug("Checking for file %s".format(file))
        	  if (!file.exists) {
        		  // TODO: Not FileFormatException
        		  errors += new Exception(s"Input file not found: ${file.getAbsolutePath} required by $$${input.name}@${task.name} [${task.realization}].")
        	  }            
          }
        }
        case _ => ; // input will be generated during workflow execution
      }
//    for (inSpec: ResolvedSpec <- task.inputVals) {
//      inSpec.srcTask match {
//        case Some(_) => ; // input will be generated during workflow execution
//        case None =>
//          inSpec.srcSpec.rval match {
//            case Literal(path) => {
//              // detect and handle globs
//              val literalSpec = inSpec.srcSpec.asInstanceOf[LiteralSpec]
//              val fileGlob: File = dirs.resolveLiteralPath(literalSpec)
//              val globbedFiles: Seq[File] = Files.glob(fileGlob.getAbsolutePath)
//              for (file <- globbedFiles) {
//                debug("Checking for file %s".format(file))
//                if (!file.exists) {
//                  // TODO: Not FileFormatException
//                  errors += new FileFormatException("Input file not found: %s required at %s:%d, defined at %s:%d".
//                              format(
//                                file.getAbsolutePath,
//                                inSpec.origSpec.declaringFile, inSpec.origSpec.pos.line,
//                                inSpec.srcSpec.declaringFile, inSpec.srcSpec.pos.line),
//                              List(inSpec.srcSpec, inSpec.origSpec))
//                }
//              }
//            }
//            case _ => throw new RuntimeException("Expected source file to be a literal")
//        }
//        case _ => ;
//      }
    }
  }
}
