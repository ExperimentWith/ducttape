// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.graph

import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import ducttape.syntax.GrammarParser
import ducttape.syntax.AbstractSyntaxTree.ConfigAssignment
import ducttape.workflow.Task

@RunWith(classOf[JUnitRunner])
class PackedGraphTest extends WordSpec {

    
  "The packed graph for an empty workflow" should {

    val string = ""
    
    val workflow = GrammarParser.readWorkflow(string)
    
    val packedGraph = new PackedGraph(workflow)
        
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
    
    val packedGraph = new PackedGraph(workflow)
        
    "contain 1 task" in {      
      assertResult(1)(packedGraph.numTasks)
    }
    
    s"contain a single goal of ${Task.NO_REALIZATION}" in {
      assertResult(1)(packedGraph.goals.size)
      assertResult(Task.NO_REALIZATION)(packedGraph.goals.values.values.flatten.head)
    }

  }

  
  "The packed graph for a workflow with 1 task, 1 branchpoint, and no plans" should {

    val string = """
      task foo 
        :: greeting=(Language: en="hello" de="Hallo" es="Hola")
      {
        echo "${greeting}"
      }
      """
    
    val workflow = GrammarParser.readWorkflow(string)
    
    val packedGraph = new PackedGraph(workflow)
        
    "contain 1 task" in {      
      assertResult(1)(packedGraph.numTasks)
    }
    
    s"contain a single goal of ${Task.NO_REALIZATION}" in {
      assertResult(1)(packedGraph.goals.size)
      assertResult(Task.NO_REALIZATION)(packedGraph.goals.values.values.flatten.head)
    }

  }

 
  "The packed graph for a workflow with 1 task, 1 branchpoint, and plans" should {

    val string = """
      task foo 
        :: greeting=(Language: en="hello" de="Hallo" es="Hola")
      {
        echo "${greeting}"
      }
      
      plan {
        reach foo via (Language: en)
        reach foo via (Language: de)
      }
      """
    
    val workflow = GrammarParser.readWorkflow(string)
    
    val packedGraph = new PackedGraph(workflow)
        
    val goals = packedGraph.goals
    
    val x = 1
    
    "contain 1 task" in {      
      assertResult(1)(packedGraph.numTasks)
    }
    
    "contain 2 goals" in {
      
      assertResult(2)(goals.size)
    }

  }

  "The packed graph for a workflow with 2 tasks, no branchpoints, and no plans" should {

    val string = """
      task foo {
        echo "hello"
      }
      
      task bar {
        echo "yo"
      }
      """
    
    val workflow = GrammarParser.readWorkflow(string)
    
    val packedGraph = new PackedGraph(workflow)
        
    "contain 2 tasks" in {      
      assertResult(2)(packedGraph.numTasks)
    }
    
    s"contain a goal of ${Task.NO_REALIZATION} for each task" in {
      assertResult(2)(packedGraph.goals.size)
      assertResult(Task.NO_REALIZATION)(packedGraph.goals.values.values.flatten.head)
    }

  }



}
