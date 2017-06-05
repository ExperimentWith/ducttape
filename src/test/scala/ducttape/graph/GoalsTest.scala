// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.graph

import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import ducttape.syntax.GrammarParser
import ducttape.syntax.AbstractSyntaxTree.ConfigAssignment
import ducttape.workflow.Realization
import ducttape.workflow.Task

@RunWith(classOf[JUnitRunner])
class GoalsTest extends WordSpec {

  val confSpecs:Seq[ConfigAssignment] = Seq()
  
  "The packed graph for an empty workflow" should {

    val string = ""
    
    val workflow = GrammarParser.readWorkflow(string)
    
    val packedGraph = new PackedGraph(workflow, confSpecs)
    
    val goals = packedGraph.goals
    
    "contain zero goals" in {
      assertResult(0)(goals.size)
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
    
    val goals = packedGraph.goals
    
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
    
    val packedGraph = new PackedGraph(workflow, confSpecs)

    val goals = packedGraph.goals
    
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
    
    val packedGraph = new PackedGraph(workflow, confSpecs)
        
    val goals = packedGraph.goals
       
    "contain 2 goals" in {   
      assertResult(2)(goals.size)
    }
    
    "contain realization foo(Language: en)" in {
      test(goals, packedGraph, branchPointName="Language", branchName="en", taskName="foo", expectedResult=true)
    }
    
    "contain realization foo(Language: de)" in {
      test(goals, packedGraph, branchPointName="Language", branchName="de", taskName="foo", expectedResult=true)
    }
    
    "not contain realization foo(Language: es)" in {
      test(goals, packedGraph, branchPointName="Language", branchName="es", taskName="foo", expectedResult=false)
    }
        
    "not contain realization (Language: sv)" in {
      assertThrows[ducttape.workflow.NoSuchBranchException] { 
        test(goals, packedGraph, branchPointName="Language", branchName="sv", taskName="foo", expectedResult=false)
      }
    }
  }

  "The packed graph for a workflow with 1 task, 1 branchpoint, and a plan with no vias" should {

    val string = """
      task foo 
        :: greeting=(Language: en="hello" de="Hallo" es="Hola")
      {
        echo "${greeting}"
      }
      
      plan {
        reach foo
      }
      """
    
    val workflow = GrammarParser.readWorkflow(string)
    
    val packedGraph = new PackedGraph(workflow, confSpecs)
        
    val goals = packedGraph.goals
       
    "contain 1 goals" in {   
      assertResult(1)(goals.size)
    }
    
    "contain realization foo(Language: en)" in {
      test(goals, packedGraph, branchPointName="Language", branchName="en", taskName="foo", expectedResult=true)
    }
    
    "contain realization foo(Language: de)" in {
      test(goals, packedGraph, branchPointName="Language", branchName="de", taskName="foo", expectedResult=false)
    }
    
    "not contain realization foo(Language: es)" in {
      test(goals, packedGraph, branchPointName="Language", branchName="es", taskName="foo", expectedResult=false)
    }
        
    "not contain realization (Language: sv)" in {
      assertThrows[ducttape.workflow.NoSuchBranchException] { 
        test(goals, packedGraph, branchPointName="Language", branchName="sv", taskName="foo", expectedResult=false)
      }
    }
  }

  "The packed graph for a workflow with 2 tasks, 1 branchpoint, and plans" should {

    val string = """
      task foo 
        :: greeting=(Language: en="hello" de="Hallo" es="Hola")
         > out
      {
        echo "${greeting}" > ${out}
      }
      
      task bar
        < in=$out@foo
        > out
      {
        cat ${in} > ${out}
      }
      
      plan {
        reach bar via (Language: en)
        reach foo via (Language: de)
      }
      """
    
    val workflow = GrammarParser.readWorkflow(string)
    
    val packedGraph = new PackedGraph(workflow, confSpecs)
        
    val goals = packedGraph.goals
       
    "contain 3 goals" in {   
      assertResult(3)(goals.size)
    }
    
    "contain realization foo[Language: en]" in {
      test(goals, packedGraph, branchPointName="Language", branchName="en", taskName="foo", expectedResult=true)
    }

    "contain realization foo[Language: de]" in {
      test(goals, packedGraph, branchPointName="Language", branchName="de", taskName="foo", expectedResult=true)
    }
 
    "not contain realization foo[Language: es]" in {
      test(goals, packedGraph, branchPointName="Language", branchName="es", taskName="foo", expectedResult=false)
    }
    
    "contain realization bar[Language: en]" in {
      test(goals, packedGraph, branchPointName="Language", branchName="en", taskName="bar", expectedResult=true)
    }

    "not contain realization bar[Language: de]" in {
      test(goals, packedGraph, branchPointName="Language", branchName="de", taskName="bar", expectedResult=false)
    }
    
    "not contain realization bar[Language: es]" in {
      test(goals, packedGraph, branchPointName="Language", branchName="es", taskName="bar", expectedResult=false)
    }
  
  }
 
  
  "The packed graph for a workflow with 2 tasks, 1 branchpoint, 1 graft, and plans" should {

    val string = """
      task foo 
        :: greeting=(Language: en="hello" de="Hallo" es="Hola")
         > out
      {
        echo "${greeting}" > ${out}
      }
      
      task bar
        < in=(Language: en=$out@foo[Language:es] de=$out@foo[Language:de] es=$out@foo[Language:en])
        > out
      {
        cat ${in} > ${out}
      }
      
      plan {
        reach bar via (Language: en)
        reach foo via (Language: de)
      }
      """
    
    val workflow = GrammarParser.readWorkflow(string)
    
    val packedGraph = new PackedGraph(workflow, confSpecs)
        
    val goals = packedGraph.goals
       
    "contain 3 goals" in {   
      assertResult(3)(goals.size)
    }
    
    "not contain realization foo[Language: en]" in {
      test(goals, packedGraph, branchPointName="Language", branchName="en", taskName="foo", expectedResult=false)
    }

    "contain realization foo[Language: de]" in {
      test(goals, packedGraph, branchPointName="Language", branchName="de", taskName="foo", expectedResult=true)
    }
 
    "not contain realization foo[Language: es]" in {
      test(goals, packedGraph, branchPointName="Language", branchName="es", taskName="foo", expectedResult=true)
    }
    
    "contain realization bar[Language: en]" in {
      test(goals, packedGraph, branchPointName="Language", branchName="en", taskName="bar", expectedResult=true)
    }

    "not contain realization bar[Language: de]" in {
      test(goals, packedGraph, branchPointName="Language", branchName="de", taskName="bar", expectedResult=false)
    }
    
    "not contain realization bar[Language: es]" in {
      test(goals, packedGraph, branchPointName="Language", branchName="es", taskName="bar", expectedResult=false)
    }
  
  }  
  
  
  def test(goals:Goals, packedGraph:PackedGraph, branchPointName:String, branchName:String, taskName:String, expectedResult:Boolean): Unit = {
    val branch = packedGraph.branchFactory(name=branchName, branchPoint=branchPointName)
    val branches = Seq(branch)
    val realization = new Realization(branches)
      
    goals.values.get(taskName) match {
      case Some(result) => assertResult(expectedResult)(result.contains(realization))
      case None         => fail()
    }    
  }
}