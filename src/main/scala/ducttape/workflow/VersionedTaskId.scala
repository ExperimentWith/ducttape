// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.workflow

import ducttape.syntax.Namespace

// holds the information necessary to uniquely identify a task's directory
//
// "realization" should be the *canonical* string representation
// as we might need to compare against realization strings from the version history
// We don't store realization as the proper Realization type since parsing a realization
// may result in BranchPoints or Branches that no longer exist in the current version's AST.
// (and are therefore invalid under the current definition of those types). 
//
// (see also docs for RealTaskId)
class VersionedTaskId(val name: Namespace, val realization: String, val version: Int) {
  lazy val realTaskId = new RealTaskId(name, realization)
  def toRealTaskId(): RealTaskId = realTaskId

  // TODO: Smear hash code better
  override def hashCode() = name.hashCode ^ realization.hashCode ^ version
  override def equals(obj: Any) = obj match {
    case that: VersionedTaskId => {
      // use hash code to weed out non-matches faster
      this.name == that.name &&
      this.realization.hashCode == that.realization.hashCode &&
      this.version == that.version &&
      this.realization == that.realization
    }
  }
  override def toString() = s"${name}/${realization}/${version}"
}
