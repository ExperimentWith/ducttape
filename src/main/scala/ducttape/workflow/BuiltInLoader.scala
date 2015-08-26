// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.workflow

import java.io.File

import ducttape.syntax.AbstractSyntaxTree.Block
import ducttape.syntax.GrammarParser
import ducttape.syntax.AbstractSyntaxTree.WorkflowDefinition
import ducttape.util.Files

import grizzled.slf4j.Logging

object BuiltInLoader extends Logging {
  
  def load(builtInsDir: File): Seq[WorkflowDefinition] = {
    for (file <- findBuiltIns(builtInsDir)) yield {
      debug("Loading builtin: %s".format(file.getAbsolutePath))
      try {
        val builtin = GrammarParser.readWorkflow(file)
        debug("Loaded submitters: %s".format(builtin.submitters.map(_.name).mkString(" ")))
        debug("Loaded versioners: %s".format(builtin.versioners.map(_.name).mkString(" ")))
        builtin
      } catch {
        case e: Exception => throw new Exception(s"Error while loading builtin: ${file.getAbsolutePath}", e)
      }
    }
  }
  
  def findBuiltIns(builtInsDir: File): Seq[File] = {
    Files.ls(builtInsDir).filter { file => file.getName.endsWith(".tape") }
  }
}
