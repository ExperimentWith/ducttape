// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.cli
import ducttape.graph.UnpackedGraph.Task
import ducttape.exec.DirectoryArchitect
import ducttape.workflow.Realization

object ColorUtils {
  def colorizeDir(taskName: String, real: Realization,
                 dirs: DirectoryArchitect): String = {
    val x = s"${dirs.confBaseDir.getAbsolutePath}/${Config.taskNameColor}${taskName}${Config.resetColor}"
    if (dirs.flat) {
      x
    } else {
      s"${x}/${Config.realNameColor}${real.toCanonicalString()}${Config.resetColor} (${Config.realFullNameColor}${real.toFullString(hashLongNames=false)}${Config.resetColor})"
    }
  }
  
  def colorizeDirs(list: Iterable[Task],
                  dirs: DirectoryArchitect): Seq[String] = {
    list.toSeq.map{ task => colorizeDir(task.name, task.realization, dirs) }
  }
}
