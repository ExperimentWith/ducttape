// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape

import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import ducttape.syntax.GrammarParser
import ducttape.syntax.AbstractSyntaxTree.ConfigAssignment

@RunWith(classOf[JUnitRunner])
class GoalsTest extends WordSpec {

  val confSpecs:Seq[ConfigAssignment] = Seq()

  
  "The packed graph for an empty workflow" should {

    val string = ""
    
    val workflow = GrammarParser.readWorkflow(string)
    
    val packedGraph = new PackedGraph(workflow, confSpecs)
    
    val goals = packedGraph.goals
    
    "contain zero tasks" in {      
      assertResult(0)(packedGraph.numTasks)
    }
    
    "contain zero goals" in {
      assertResult(0)(packedGraph.goals.size)
    }

  }
  

  "The packed graph for a workflow with 1 task, no branchpoints, and no plans" should {

    val string = """
      task foo {
        echo "hello"
      }
      """
    
    val workflow = GrammarParser.readWorkflow(string)
    
    val packedGraph = new PackedGraph(workflow, confSpecs)
        
    "contain 1 task" in {      
      assertResult(1)(packedGraph.numTasks)
    }
    
    "contain zero goals" in {
      assertResult(0)(packedGraph.goals.size)
    }

  }
  
}