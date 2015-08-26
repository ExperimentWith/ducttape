// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.cli

import ducttape.workflow.Branch
import ducttape.workflow.Realization
import ducttape.util.Globs

import java.util.regex.Pattern

import org.apache.commons.lang3.StringUtils

import grizzled.slf4j.Logging

class RealizationGlob(glob: String) extends Logging {
  // the pattern contains one tuple per branch point specified within the glob
  // the tuple consists of a branch point name glob and a branch glob

  val globElems: Seq[String] = {
    val splitter = Pattern.compile(Realization.delimiter, Pattern.LITERAL)
    splitter.split(glob).toSeq
  }

  // branch point name -> pattern
  val patterns: Map[String,Pattern] = {
    globElems.filter(_ != "*").map { elem: String =>
      // Note: Branch POINTS may NOT contain a dot. However, BRANCHES CAN contain a dot.
      // limit to 2 elements to include branch point and branch only
      val Array(branchPoint, branchGlob) = StringUtils.split(elem, ".", 2)

      // we explicitly disallow globbing within branch names to keep the semantics of
      // branch globbing easy to understand

      val branchPat = Pattern.compile(Globs.globToRegex(branchGlob))
      (branchPoint, branchPat)
    }
  }.toMap

  // if the user specifies the "global glob", it indicates that ducttape
  // should glob *all* branches of branch points that were not otherwise specified.
  // without the "global glob", ducttape will take only the baseline realization of
  // branch points that are not explicitly mentioned
  val globalGlob: Boolean = globElems.contains("*")

  def matches(real: Realization): Boolean = {
    debug("REAL is %s".format(real))
    // require this realization to contain all of the branch points mentioned in the glob
    val containsBPs = patterns.keys.forall { bpName: String =>
      val ok = real.branches.exists(_.branchPoint.name == bpName)
      if (!ok) debug("Realization '%s' does not mention required branch point '%s'".format(real.branches.mkString("+"), bpName))
      ok
    }

    // and that all of the branches in the realization match the glob's requirements
    val matchesGlob = real.branches.forall { branch: Branch =>
      val branchOk = patterns.get(branch.branchPoint.name) match {
        // didn't match any explicitly mentioned branch points
        case None => globalGlob || branch.baseline
        // matched a branch point... what about the branch?
        case Some(branchPat) => branchPat.matcher(branch.name).matches
      }
      if (branchOk)
        debug("Glob '%s' DID match branch '%s'".format(glob, branch))
      else
        debug("Glob '%s' did NOT match branch '%s'".format(glob, branch))
      branchOk
    }

    debug("Glob '%s': Contains required branch points? %b Matches glob? %b".format(glob, containsBPs, matchesGlob))
    containsBPs && matchesGlob
  }
}
