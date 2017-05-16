package ducttape

import ducttape.{PackedGraph => packed}
import ducttape.syntax.{AbstractSyntaxTree => ast}
import ducttape.syntax.BashCode
import ducttape.workflow.Branch
import ducttape.workflow.BranchFactory
import ducttape.workflow.BranchPoint
import scala.collection.Map

class UnpackedGraph {

}



object UnpackedGraph {
/*  
  def crossProduct(branchMap:Map[BranchPoint, Seq[Branch]]) : Seq[Seq[Branch]] = {
    
    val branchPoints = branchMap.keys.toSeq.sortBy{ branchPoint => branchPoint.toString }
    
    val solutions = Seq.newBuilder[Seq[Branch]]
    
    def recursivelyConstructSolution(bpIndex:Int, partialSolution:Seq[Branch]): Unit = {
      if (bpIndex < branchPoints.size) {
        val branchPoint = branchPoints(bpIndex)
        val branches = branchMap(branchPoint)
        for (branch <- branches) {
          if (bpIndex == branchPoints.size - 1) {
            solutions += (partialSolution++Seq(branch))
          } else {
            recursivelyConstructSolution(bpIndex+1, partialSolution++Seq(branch))
          }
        }
      }
    }
    
    recursivelyConstructSolution(0, Seq())
    
    return solutions.result
  }
*/
  
  def planRealizations(plan:ast.PlanDefinition, branchFactory:BranchFactory): Unit = {
    
    for (crossProduct <- plan.crossProducts) {
    
      val branchTuples:Seq[(BranchPoint, Seq[Branch])] = crossProduct.value.map { branchPointRef => branchPointRef.getBranches(branchFactory) }
      val branchMap:   Map[ BranchPoint, Seq[Branch] ] = branchTuples.toMap
      
      crossProduct.value.map{ branchPointRef => }
      
    }
    
  }
  
  def unpack(packedGraph:PackedGraph, plan:ast.PlanDefinition): Unit = {
    
    
    
    
    
  }
  
}