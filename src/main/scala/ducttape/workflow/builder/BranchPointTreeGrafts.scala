// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.workflow.builder

import ducttape.workflow.Branch

/**
 * TODO: Explain to Lane what this class is
 */
private[builder] class BranchPointTreeGrafts(
    val tree: BranchPointTree,
    val grafts: Seq[Branch]) {

  override def toString() : String = {
    val treeStr = tree.toString()
    val graftStr = "\n\tgrafts=[" + grafts.mkString(",") + "]"
    return treeStr.replaceAll("\n","\n\t") + graftStr//.replaceAll("ArrayBuffer","") + graftStr
  }
}
