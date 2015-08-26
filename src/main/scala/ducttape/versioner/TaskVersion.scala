// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.versioner

import collection._
import java.io.File
import ducttape.util.Files

object TaskVersion {
  def read(file: File): Int = Files.read(file) match {
    case Seq(line) => line.toInt
    // TODO: More resilient error handling?
    case _ => throw new RuntimeException("Invalid version file: %s".format(file.getAbsolutePath))
  }
}

// TODO: Diff the workflow files themselves
trait TaskVersion {
  def ==(that: TaskVersion) = this.digest() == that.digest()
  def digest(): Int;
  def toString(): String;
}
/*
// do config files first because...
// This must also include modifications specified in external config files!!!
class TaskVersion(val task: TaskDef,
                  val taskVer: Version,
                  val inVers: Seq[(Spec,Version)]) extends Version {

  def digest() = {
    // how do we factor out choice of digest algorithm? config allows different ones for workflow, files, aggregation, etc?
  }

  def print(out: PrintStream) = {
    out.println("%s = %s".format(task.name, taskVer.toString))
    for( (inFileSpec, v) <- inVers) {
      out.println("%s = %s".format(inFileSpec.name, v.toString))
    }
  }
}

class WorkflowVersion(val when: Date) extends Version {
  // contains a version hash or revision (Integer? Special DataType?)
  // for every taskdef, every input file, (output file?), 
  override def toString() = {
    ""
  }
}

// TODO: Use java.security.DigestInputStream?


class WorkflowVersioner {

  val versions = List[WorkflowVersion]

  def toString() = {
    ""
  }
}
*/
