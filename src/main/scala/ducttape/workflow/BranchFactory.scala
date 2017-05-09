// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.workflow

import collection._
import grizzled.slf4j.Logging

import ducttape.syntax.AbstractSyntaxTree.ASTType
import ducttape.syntax.AbstractSyntaxTree.BranchPointDef
import ducttape.syntax.AbstractSyntaxTree.Sequence
import ducttape.syntax.AbstractSyntaxTree.SequentialBranchPoint
import ducttape.syntax.AbstractSyntaxTree.Spec
import ducttape.syntax.FileFormatException

class NoSuchBranchException(val msg: String) extends Exception(msg)

// pool branches to make comparison easier
class BranchFactory() extends Logging {
  
  private val branchPointFactory = new BranchPointFactory
  
  private val pool = new mutable.HashMap[(String,BranchPoint),Branch]
  pool += (Task.NO_BRANCH.name, Task.NO_BRANCH_POINT) -> Task.NO_BRANCH

  // creates new if branch isn't found
  private[workflow] def get(myName: String, myBranchPoint: BranchPoint, isBaseline: Boolean): Branch = {
    assert(myName != null)
    if (isBaseline) debug("New baseline branch: %s:%s".format(myBranchPoint, myName))
    pool.getOrElseUpdate( (myName, myBranchPoint), new Branch {
      override val name = myName
      override val baseline = isBaseline
      override val branchPoint = myBranchPoint
    } )
  }
  // creates new if branch isn't found
  private[workflow] def get(name: String, branchPoint: String, isBaseline: Boolean): Branch = {
    assert(name != null)
    get(name, branchPointFactory.get(branchPoint), isBaseline)
  }
  
  // throws if branch isn't found
  def apply(name: String, branchPoint: BranchPoint): Branch = try {
    pool( (name, branchPoint) )
  } catch {
    case e: NoSuchElementException => throw new NoSuchBranchException("%s@%s".format(name, branchPoint))
  }
  
  // throws if branch isn't found
  def apply(name: String, branchPoint: String): Branch = {
    apply(name, branchPointFactory(branchPoint))
  }
  
  /**
   * Gets a map containing all branches for each branch point
   */
  def getAll() : Map[BranchPoint,Iterable[Branch]] = { 
    return pool.values.groupBy{ branch: Branch => branch.branchPoint }
  }
  
  /**
   * Gets all branches for a particular branch point
   */
  def getAll(branchPointName: String) : Iterable[Branch] = { 
    val branchPoint = branchPointFactory.get(branchPointName)
    val map = getAll()
    return map(branchPoint)
  }
  
  def getBranchPoint(branchPointName: String) : BranchPoint = {
    return branchPointFactory.get(branchPointName)
  }
  
  def size() : (Int,Int) = {
    val branchPoints = getAll().keys.size
    val branches = pool.size
    return (branchPoints, branches)
  }
  
  // first identify all branch points that are present in the workflow so that
  // we can identify and store which elements are branch points
  def findBranchPoints(element: ASTType): Unit = {
    element match {
      case BranchPointDef(nameOpt: Option[String], branchSpecs: Seq[Spec]) => {
        nameOpt match {
          case Some(branchPointName) => {
            // the get() method of BranchPointFactory and BranchFactory
            // cause these factories to globally remember the set of 
            // branches and branch points
            val branchPoint = this.getBranchPoint(branchPointName)
            for ( (branchSpec, idx) <- branchSpecs.zipWithIndex) {
              val isBaseline = (idx == 0)
              val branch = this.get(branchSpec.name, branchPoint, isBaseline)
            }
          }
          case None => {
            throw new FileFormatException("Anonymous branch points are not yet supported", element)
          }
        }
      }
      case SequentialBranchPoint(nameOpt: Option[String], sequence: Sequence) => {
        nameOpt match {
          case Some(branchPointName) => {
            // the get() method of BranchPointFactory and BranchFactory
            // cause these factories to globally remember the set of 
            // branches and branch points
            val branchPoint = this.getBranchPoint(branchPointName)
            for ( value <- sequence.start to sequence.end by sequence.increment) {
              val isBaseline = (value == sequence.start)
              val branch = this.get(value.toString, branchPoint, isBaseline)
            }
          }
          case None => {
            throw new FileFormatException("Anonymous branch points are not yet supported", element)
          }
        }     
      }
      
      case _ => ;
    }

    for (child <- element.children) {
      this.findBranchPoints(child)
    }
    
  }
  
   override def toString() : String = {
     val s = new StringBuilder()
     
     for ((bp,branches) <- this.getAll()) {
       s.append(bp.toString)
       s.append("\n")
       for (branch <- branches) {
         s.append("\t")
         s.append(branch.toString)
         s.append("\n")
       }
     }
     
     return s.toString
   }
  
}
