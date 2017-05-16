// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.workflow

import ducttape.util.HashUtils

import collection._

import grizzled.slf4j.Logging

// Note: branches are now guaranteed to be sorted by the HyperDAG
// see RealizationOrdering
class Realization(val branches: Seq[Branch]) extends Logging {
 // TODO: Keep string branch point names?

  /** sort by branch *point* names to keep ordering consistent, then join branch names using dashes
   *   and don't include our default branch "baseline"
   * by default we "shorten" realization names by removing branch points using baseline branches
   *   such that new branch points can easily be added since the entire directory structure doesn't
   *   suddenly change */
  private def realizationName(hashLongNames: Boolean = true): String = {
    
    // sort by branch point name and remove references to the baseline branch
    // (removing all instances of baseline for any branch allows easier extensibility)
    val filteredBranches: Seq[Branch] = branches.filter { !_.baseline } match {
      // make sure we have at least baseline, if nothing else
      case Seq() => Seq(Task.NO_BRANCH)
      case myBranches => myBranches 
    }
                                     
    // write baseline branch to disk as "baseline" to allow for easier extensibility
    // while maintaining semantics
    val names = filteredBranches.map { branch =>
      val displayName = if (branch.baseline) "baseline" else branch.name
      s"${branch.branchPoint.name}.${displayName}"
    }
    val result = names.mkString(Realization.delimiter)
    
    if (result.length > Realization.MaxNameLength && hashLongNames) {
      warn(s"Very long filename is being hashed: ${result}")
      HashUtils.md5(result)
    } else {
      result
    }
  }
    
  private def fullRealizationName(hashLongNames: Boolean = true): String = {
    // TODO: Prohibit the branch point name "Baseline"?
    val filteredBranches: Seq[Branch] = branches.filter { _.branchPoint != Task.NO_BRANCH_POINT } match {
      // make sure we have at least baseline, if nothing else
      case Seq() => Seq(Task.NO_BRANCH)
      case myBranches => myBranches 
    }
    val names = filteredBranches.map { branch => s"${branch.branchPoint.name}.${branch.name}" }
    val result = names.mkString(Realization.delimiter)
    if (result.length > Realization.MaxNameLength && hashLongNames) {
      warn(s"Very long filename is being hashed: ${result}")
      HashUtils.md5(result)
    } else {
      result
    }
  }

  lazy val str = realizationName(false)

  override def hashCode() = str.hashCode // TODO: More efficient?
  override def equals(obj: Any) = obj match { case that: Realization => this.str == that.str } // TODO: More efficient?
  // a canonical representation that should hold constant even when new branch points are added
  // (assuming users follow our best practices for creating baseline branches)
  def toCanonicalString(hashLongNames: Boolean = false) = {
    if (hashLongNames)
      realizationName(true)
    else
      str
  }
  override def toString() = toCanonicalString(false)
  
  // unshortened realization name
  def toFullString(hashLongNames: Boolean = true): String = fullRealizationName(hashLongNames)
  
  // returns true if this realization has only one branch and it is Baseline.baseline
  def hasSingleBranchBaseline(): Boolean = {
    branches == Seq(Task.NO_BRANCH) || branches.isEmpty
  }
}

object Realization {
  var delimiter: String = "+"
  
  val MaxNameLength = 255 - 5; // leave room for ".LOCK"

    
  def crossProduct(branchMap:Map[BranchPoint, Iterable[Branch]], withOmissions:Boolean=false) : Seq[Realization] = {
    
    val branchPoints = branchMap.keys.toSeq.sortBy{ branchPoint => branchPoint.toString }
    
    val solutions = Seq.newBuilder[Realization]
    
    def recursivelyConstructSolution(bpIndex:Int, partialSolution:Seq[Branch]): Unit = {
      if (bpIndex < branchPoints.size) {
        val branchPoint = branchPoints(bpIndex)
        val branches = branchMap(branchPoint)
        for (branch <- branches) {
          if (bpIndex == branchPoints.size - 1) {
            val realization = new Realization(partialSolution++Seq(branch))
            solutions += realization
          } else {
            recursivelyConstructSolution(bpIndex+1, partialSolution++Seq(branch))
          }
        }
        if (withOmissions) {
          if (bpIndex == branchPoints.size - 1) {
            val realization = new Realization(partialSolution)
            solutions += realization
          } else {
            recursivelyConstructSolution(bpIndex+1, partialSolution)
          }
        }
      }
    }
    
    recursivelyConstructSolution(0, Seq())
    
    return solutions.result
  }
}
