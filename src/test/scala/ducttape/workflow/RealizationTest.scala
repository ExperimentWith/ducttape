// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.workflow

import ducttape.workflow.Task.NO_BRANCH
import org.scalatest.WordSpec
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class RealizationTest extends WordSpec {


  private def createBranches(numBranchPoints:Int, branchesPerBranchPoint:Int): BranchFactory = {
    val map = Map[Int, Int]().withDefaultValue(branchesPerBranchPoint)
    return createBranches(numBranchPoints, map)
  }

  private def createBranches(numBranchPoints:Int, branchesPerBranchPoint:Map[Int,Int]): BranchFactory = {
    val branchFactory = new BranchFactory()
  
    Range(1, numBranchPoints+1).foreach{ branchPointIndex =>
      val branchPoint = ('a' + branchPointIndex - 1).toChar.toString
      Range(1, branchesPerBranchPoint(branchPointIndex)+1).foreach{ branchIndex =>
        branchFactory.get(s"${branchPoint}${branchIndex}",branchPoint.toUpperCase(),isBaseline=(branchIndex==1))
      }
    }

    return branchFactory
  }



  "A cross product with 1 branch points, each of which contain 1 branch" should {

    val branchFactory = createBranches(numBranchPoints=1, branchesPerBranchPoint=1)
    val branchMap = branchFactory.getAll(includeBaseline=false)
    
    val crossProduct = Realization.crossProduct(branchMap, withOmissions=false)
    
    "have a cross-product consisting of expected size" in {      
      expectResult(1)(crossProduct.size)
    }
    
    "have a single realization with expected value" in {
      expectResult("A.a1")(crossProduct.head.toFullString())
    }

  }

  "A cross product with 1 branch points, each of which contain 1 branch, with omissions" should {

    val branchFactory = createBranches(numBranchPoints=1, branchesPerBranchPoint=1)
    val branchMap = branchFactory.getAll(includeBaseline=false)
    
    val crossProduct = Realization.crossProduct(branchMap, withOmissions=true)

    "have a cross-product of expected size" in {      
      expectResult(2)(crossProduct.size)
    }

    "have realizations with expected value" in {
      val expected = new scala.collection.mutable.ArrayBuffer[String]
      expected ++= crossProduct.map{ realization => realization.toFullString() }.sorted
      
      expectResult("A.a1")(expected.remove(0))
      expectResult(NO_BRANCH.toString)(expected.remove(0))
    }
  }  

  "A cross product with 2 branch points, each of which contain 2 branches" should {

    val branchFactory = createBranches(numBranchPoints=2, branchesPerBranchPoint=2)
    val branchMap = branchFactory.getAll(includeBaseline=false)
    
    val crossProduct = Realization.crossProduct(branchMap, withOmissions=false)
    
    "have a cross-product consisting of expected size" in {      
      expectResult(4)(crossProduct.size)
    }
    
    "have realizations with expected values" in {
      val expected = new scala.collection.mutable.ArrayBuffer[String]
      expected ++= crossProduct.map{ realization => realization.toFullString() }.sorted
      
      expectResult("A.a1+B.b1")(expected.remove(0))
      expectResult("A.a1+B.b2")(expected.remove(0))
      expectResult("A.a2+B.b1")(expected.remove(0))
      expectResult("A.a2+B.b2")(expected.remove(0))      
    }
    
  }
  

  "A cross product with 2 branch points, each of which contain 2 branches, with omissions" should {

    val branchFactory = createBranches(numBranchPoints=2, branchesPerBranchPoint=2)
    val branchMap = branchFactory.getAll(includeBaseline=false)
    
    val crossProduct = Realization.crossProduct(branchMap, withOmissions=true)
    
    "have a cross-product consisting of expected size" in {      
      expectResult(9)(crossProduct.size)
    }
    
    "have realizations with expected values" in {
      val expected = new scala.collection.mutable.ArrayBuffer[String]
      expected ++= crossProduct.map{ realization => realization.toFullString() }.sorted
      
      expectResult("A.a1")(expected.remove(0))
      expectResult("A.a1+B.b1")(expected.remove(0))
      expectResult("A.a1+B.b2")(expected.remove(0))
      expectResult("A.a2")(expected.remove(0))
      expectResult("A.a2+B.b1")(expected.remove(0))
      expectResult("A.a2+B.b2")(expected.remove(0))
      expectResult("B.b1")(expected.remove(0))
      expectResult("B.b2")(expected.remove(0))
      expectResult(NO_BRANCH.toString)(expected.remove(0))
    }
    
  }  
  
  "A cross product with 3 branch points, each of which contain 2 branches" should {

    val branchFactory = createBranches(numBranchPoints=3, branchesPerBranchPoint=2)
    val branchMap = branchFactory.getAll(includeBaseline=false)
    
    val crossProduct = Realization.crossProduct(branchMap, withOmissions=false)
    
    "have a cross-product consisting of expected size" in {      
      expectResult(8)(crossProduct.size)
    }
    
    "have realizations with expected values" in {
      val expected = new scala.collection.mutable.ArrayBuffer[String]
      expected ++= crossProduct.map{ realization => realization.toFullString() }.sorted
      
      expectResult("A.a1+B.b1+C.c1")(expected.remove(0))
      expectResult("A.a1+B.b1+C.c2")(expected.remove(0))
      expectResult("A.a1+B.b2+C.c1")(expected.remove(0))
      expectResult("A.a1+B.b2+C.c2")(expected.remove(0))
      expectResult("A.a2+B.b1+C.c1")(expected.remove(0))
      expectResult("A.a2+B.b1+C.c2")(expected.remove(0))
      expectResult("A.a2+B.b2+C.c1")(expected.remove(0)) 
      expectResult("A.a2+B.b2+C.c2")(expected.remove(0)) 
    }
    
  }
  
  "A cross product with 3 branch points, each of which contain 2 branches, with omissions" should {

    val branchFactory = createBranches(numBranchPoints=3, branchesPerBranchPoint=2)
    val branchMap = branchFactory.getAll(includeBaseline=false)
    val crossProduct = Realization.crossProduct(branchMap, withOmissions=true)
    
    "have a cross-product consisting of expected size" in {      
      expectResult(27)(crossProduct.size)
    }
    
    "have realizations with expected values" in {
      val expected = new scala.collection.mutable.ArrayBuffer[String]
      expected ++= crossProduct.map{ realization => realization.toFullString() }.sorted
      
      expectResult("A.a1")(expected.remove(0))
      expectResult("A.a1+B.b1")(expected.remove(0))
      expectResult("A.a1+B.b1+C.c1")(expected.remove(0))
      expectResult("A.a1+B.b1+C.c2")(expected.remove(0))
      expectResult("A.a1+B.b2")(expected.remove(0))
      expectResult("A.a1+B.b2+C.c1")(expected.remove(0))
      expectResult("A.a1+B.b2+C.c2")(expected.remove(0))
      expectResult("A.a1+C.c1")(expected.remove(0))
      expectResult("A.a1+C.c2")(expected.remove(0))
      expectResult("A.a2")(expected.remove(0))
      expectResult("A.a2+B.b1")(expected.remove(0))
      expectResult("A.a2+B.b1+C.c1")(expected.remove(0))
      expectResult("A.a2+B.b1+C.c2")(expected.remove(0))
      expectResult("A.a2+B.b2")(expected.remove(0))
      expectResult("A.a2+B.b2+C.c1")(expected.remove(0)) 
      expectResult("A.a2+B.b2+C.c2")(expected.remove(0))
      expectResult("A.a2+C.c1")(expected.remove(0))
      expectResult("A.a2+C.c2")(expected.remove(0))
      expectResult("B.b1")(expected.remove(0))
      expectResult("B.b1+C.c1")(expected.remove(0))
      expectResult("B.b1+C.c2")(expected.remove(0))
      expectResult("B.b2")(expected.remove(0))
      expectResult("B.b2+C.c1")(expected.remove(0))
      expectResult("B.b2+C.c2")(expected.remove(0))
      expectResult(NO_BRANCH.toString)(expected.remove(0))
      expectResult("C.c1")(expected.remove(0))
      expectResult("C.c2")(expected.remove(0))
    }
    
  }
}