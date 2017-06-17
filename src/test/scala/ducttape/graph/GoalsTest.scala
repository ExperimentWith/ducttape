// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.graph

import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import ducttape.syntax.GrammarParser
import ducttape.syntax.AbstractSyntaxTree.ConfigAssignment
import ducttape.workflow.Branch
import ducttape.workflow.Realization
import ducttape.workflow.Task

@RunWith(classOf[JUnitRunner])
class GoalsTest extends WordSpec {
   
  "The packed graph for an empty workflow" should {

    val string = ""
    
    val workflow = GrammarParser.readWorkflow(string)
    
    val packedGraph = new PackedGraph(workflow)
    
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
    
    val packedGraph = new PackedGraph(workflow)
    
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
    
    val packedGraph = new PackedGraph(workflow)

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
    
    val packedGraph = new PackedGraph(workflow)
        
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
    
    val packedGraph = new PackedGraph(workflow)
        
    val goals = packedGraph.goals
       
    "contain 1 goals" in {   
      assertResult(1)(goals.size)
    }
    
    "contain realization foo(Language: en)" in {
      test(goals, packedGraph, branchPointName="Language", branchName="en", taskName="foo", expectedResult=true)
    }
    
    "not contain realization foo(Language: de)" in {
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
  
  "The packed graph for a workflow with 2 tasks, 1 branchpoint, and a plan with 3 vias" should {

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
        reach bar via (Language: de)
        reach foo via (Language: de)
        reach foo via (Language: en)
      }
      """
    
    val workflow = GrammarParser.readWorkflow(string)
    
    val packedGraph = new PackedGraph(workflow)
        
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
    
    "not contain realization bar[Language: en]" in {
      test(goals, packedGraph, branchPointName="Language", branchName="en", taskName="bar", expectedResult=false)
    }

    "contain realization bar[Language: de]" in {
      test(goals, packedGraph, branchPointName="Language", branchName="de", taskName="bar", expectedResult=true)
    }
    
    "not contain realization bar[Language: es]" in {
      test(goals, packedGraph, branchPointName="Language", branchName="es", taskName="bar", expectedResult=false)
    }
  
  }
  
  "The packed graph for a workflow with 2 tasks, 1 branchpoint, and a plan with two vias" should {

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
        reach bar via (Language: de)
        reach foo via (Language: en)
      }
      """
    
    val workflow = GrammarParser.readWorkflow(string)
    
    val packedGraph = new PackedGraph(workflow)
        
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
    
    "not contain realization bar[Language: en]" in {
      test(goals, packedGraph, branchPointName="Language", branchName="en", taskName="bar", expectedResult=false)
    }

    "contain realization bar[Language: de]" in {
      test(goals, packedGraph, branchPointName="Language", branchName="de", taskName="bar", expectedResult=true)
    }
    
    "not contain realization bar[Language: es]" in {
      test(goals, packedGraph, branchPointName="Language", branchName="es", taskName="bar", expectedResult=false)
    }
  
  }
  
  "The packed graph for a workflow with 2 tasks, 1 branchpoint, and a plan with one via and one no via" should {

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
        reach bar via (Language: de)
        reach foo
      }
      """
    
    val workflow = GrammarParser.readWorkflow(string)
    
    val packedGraph = new PackedGraph(workflow)
        
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
    
    "not contain realization bar[Language: en]" in {
      test(goals, packedGraph, branchPointName="Language", branchName="en", taskName="bar", expectedResult=false)
    }

    "contain realization bar[Language: de]" in {
      test(goals, packedGraph, branchPointName="Language", branchName="de", taskName="bar", expectedResult=true)
    }
    
    "not contain realization bar[Language: es]" in {
      test(goals, packedGraph, branchPointName="Language", branchName="es", taskName="bar", expectedResult=false)
    }
  
  }
  
  "The packed graph for a workflow with 2 tasks, 1 branchpoint, and a plan with one no via and one via" should {

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
        reach bar
        reach foo via (Language: de)
      }
      """
    
    val workflow = GrammarParser.readWorkflow(string)
    
    val packedGraph = new PackedGraph(workflow)
        
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
  
  "The packed graph for a workflow with 2 tasks, 1 branchpoint, and a plan with no vias" should {

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
        reach bar
        reach foo
      }
      """
    
    val workflow = GrammarParser.readWorkflow(string)
    
    val packedGraph = new PackedGraph(workflow)
        
    val goals = packedGraph.goals
       
    "contain 2 goals" in {   
      assertResult(2)(goals.size)
    }
    
    "contain realization foo[Language: en]" in {
      test(goals, packedGraph, branchPointName="Language", branchName="en", taskName="foo", expectedResult=true)
    }

    "contain realization foo[Language: de]" in {
      test(goals, packedGraph, branchPointName="Language", branchName="de", taskName="foo", expectedResult=false)
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
    
    val packedGraph = new PackedGraph(workflow)
        
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
    
    val packedGraph = new PackedGraph(workflow)
        
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
 
    "contain realization foo[Language: es]" in {
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
  
  "The packed graph for a workflow with 2 tasks, 1 branchpoint, 1 graft, and plans (including no via)" should {

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
        reach foo
      }
      """
    
    val workflow = GrammarParser.readWorkflow(string)
    
    val packedGraph = new PackedGraph(workflow)
        
    val goals = packedGraph.goals
       
    "contain 3 goals" in {   
      assertResult(4)(goals.size)
    }
    
    "not contain realization foo[Language: en]" in {
      test(goals, packedGraph, branchPointName="Language", branchName="en", taskName="foo", expectedResult=true)
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
  
  "The packed graph for acid.tape workflow with plan to reach nothing with no via" should {
  
    val plan = """
      plan {
        reach nothing
      }
      """
    
    val workflow = GrammarParser.readWorkflow(Acid.tape + plan)
    val packedGraph = new PackedGraph(workflow)
    val goals = packedGraph.goals
       
    "contain goals" in {   
      assertResult(1)(goals.size)
    }
    
    "contain realization nothing" in {
      test(goals, packedGraph, branchPointName="Foo", branchName="bar", taskName="nothing", expectedResult=true)
    }
    
  }

  "The packed graph for acid.tape workflow with plan to reach nothing with a via" should {
  
    val plan = """
      plan {
        reach nothing via (Foo: bar)
      }
      """
    
    val workflow = GrammarParser.readWorkflow(Acid.tape + plan)
    val packedGraph = new PackedGraph(workflow)
    val goals = packedGraph.goals
       
    "contain goals" in {   
      assertResult(1)(goals.size)
    }
    
    "contain realization nothing" in {
      test(goals, packedGraph, branchPointName="Foo", branchName="bar", taskName="nothing", expectedResult=true)
    }
    
  }  

  "The packed graph for acid.tape workflow with plan to reach nothing with an incompatible via" should {
  
    val plan = """
      plan {
        reach nothing via (DataSet: test)
      }
      """
    
    val workflow = GrammarParser.readWorkflow(Acid.tape + plan)
    val packedGraph = new PackedGraph(workflow)
    val goals = packedGraph.goals
       
    "contain goals" in {   
      assertResult(1)(goals.size)
    }
    
    "contain realization nothing[Baseline.baseline]" in {
      test(goals, packedGraph, branchPointName="Baseline", branchName="baseline", taskName="nothing", expectedResult=true)
    }
    
    "contain realization nothing[Foo.bar]" in {
      test(goals, packedGraph, branchPointName="Foo", branchName="bar", taskName="nothing", expectedResult=true)
    }

    "not contain realization nothing[DataSet.test]" in {
      test(goals, packedGraph, branchPointName="DataSet", branchName="test", taskName="nothing", expectedResult=false)
    }
  }    


  "The packed graph for acid.tape workflow with plan to reach extract_dictionary using a plan with no vias" should {
  
    val plan = """
      plan {
        reach extract_dictionary
      }
      """
    
    val workflow = GrammarParser.readWorkflow(Acid.tape + plan)
    val packedGraph = new PackedGraph(workflow)
    val goals = packedGraph.goals
    
    "contain goals" in {   
      assertResult(1)(goals.size)
    }
    
    "contain realization extract_dictionary" in {
      test(goals, packedGraph, branchPointName="Baseline", branchName="baseline", taskName="extract_dictionary", expectedResult=true)
    }
    
  }   

  "The packed graph for acid.tape workflow with plan to reach extract_dictionary using a plan with incompatible vias" should {
  
    val plan = """
      plan {
        reach extract_dictionary via (Foo:bar)
      }
      """
    
    val workflow = GrammarParser.readWorkflow(Acid.tape + plan)
    val packedGraph = new PackedGraph(workflow)
    val goals = packedGraph.goals
    
    "contain goals" in {   
      assertResult(1)(goals.size)
    }
    
    "contain realization (Baseline:baseline)" in {
      test(goals, packedGraph, branchPointName="Baseline", branchName="baseline", taskName="extract_dictionary", expectedResult=true)
    }
    
  }    

  "The packed graph for acid.tape workflow with plan to reach extract_dictionary using a plan via (DataSet: test) * (TestSplit: standard)" should {
  
    val plan = """
      plan {
        reach extract_dictionary via (DataSet: test) * (TestSplit: standard)
      }
      """
    
    val workflow = GrammarParser.readWorkflow(Acid.tape + plan)
    val packedGraph = new PackedGraph(workflow)
    val goals = packedGraph.goals
    
    "contain goals" in {   
      assertResult(1)(goals.size)
    }
    
    "contain realization (DataSet: test) * (TestSplit: standard)" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("TestSplit","standard")), taskName="extract_dictionary", expectedResult=true)
    }
    
    "not contain realization (Baseline:baseline)" in {
      test(goals, packedGraph, branchPointName="Baseline", branchName="baseline", taskName="extract_dictionary", expectedResult=false)
    }
  }    
    
  "The packed graph for acid.tape workflow with plan to reach extract_dictionary using a plan via (DataSet: train) * (TestSplit: standard)" should {
  
    val plan = """
      plan {
        reach extract_dictionary via (DataSet: train) * (TestSplit: standard)
      }
      """
    
    val workflow = GrammarParser.readWorkflow(Acid.tape + plan)
    val packedGraph = new PackedGraph(workflow)
    val goals = packedGraph.goals
    
    "contain goals" in {   
      assertResult(1)(goals.size)
    }
    
    "contain realization (DataSet: train) * (TestSplit: standard)" in {
      test(goals, packedGraph, Seq(("DataSet","train"),("TrainCorpus","small")), taskName="extract_dictionary", expectedResult=true)
    }
    
    "contain realization (Baseline:baseline)" in {
      test(goals, packedGraph, branchPointName="Baseline", branchName="baseline", taskName="extract_dictionary", expectedResult=true)
    }
  }  

  "The packed graph for acid.tape workflow with plan to reach extract_dictionary using a plan via (DataSet: train) * (TrainCorpus: small)" should {
  
    val plan = """
      plan {
        reach extract_dictionary via (DataSet: train) * (TrainCorpus: small)
      }
      """
    
    val workflow = GrammarParser.readWorkflow(Acid.tape + plan)
    val packedGraph = new PackedGraph(workflow)
    val goals = packedGraph.goals
    
    "contain goals" in {   
      assertResult(1)(goals.size)
    }
    
    "contain realization (DataSet: train) * (TrainCorpus: standard)" in {
      test(goals, packedGraph, Seq(("DataSet","train"),("TrainCorpus","small")), taskName="extract_dictionary", expectedResult=true)
    }

    "not contain realization (DataSet: train) * (TrainCorpus: large)" in {
      test(goals, packedGraph, Seq(("DataSet","train"),("TrainCorpus","large")), taskName="extract_dictionary", expectedResult=false)
    }    
    
    "contain realization (Baseline:baseline)" in {
      test(goals, packedGraph, branchPointName="Baseline", branchName="baseline", taskName="extract_dictionary", expectedResult=true)
    }
  }    

  "The packed graph for acid.tape workflow with plan to reach extract_dictionary using a plan via (DataSet: train) * (TrainCorpus: large)" should {
  
    val plan = """
      plan {
        reach extract_dictionary via (DataSet: train) * (TrainCorpus: large)
      }
      """
    
    val workflow = GrammarParser.readWorkflow(Acid.tape + plan)
    val packedGraph = new PackedGraph(workflow)
    val goals = packedGraph.goals
    
    "contain goals" in {   
      assertResult(1)(goals.size)
    }
    
    "contain realization (DataSet: train) * (TestSplit: standard)" in {
      test(goals, packedGraph, Seq(("DataSet","train"),("TrainCorpus","large")), taskName="extract_dictionary", expectedResult=true)
    }
    
    "not contain realization (DataSet: train) * (TrainCorpus: standard)" in {
      test(goals, packedGraph, Seq(("DataSet","train"),("TrainCorpus","small")), taskName="extract_dictionary", expectedResult=false)
    }
    
    "not contain realization (Baseline:baseline)" in {
      test(goals, packedGraph, branchPointName="Baseline", branchName="baseline", taskName="extract_dictionary", expectedResult=false)
    }
  }   

  "The packed graph for acid.tape workflow with plan to reach corpus_counts using a plan with no via" should {
  
    val plan = """
      plan {
        reach corpus_counts
      }
      """
    
    val workflow = GrammarParser.readWorkflow(Acid.tape + plan)
    val packedGraph = new PackedGraph(workflow)
    val goals = packedGraph.goals
    //print(goals)
    
    "contain goals" in {   
      assertResult(5)(goals.size)
    }
    
    "contain realization corpus_counts[Baseline:baseline]" in {
      test(goals, packedGraph, Seq(("Baseline","baseline")),                                    taskName="corpus_counts",      expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","train"),("Foo","bar"),("TrainCorpus","small")),  taskName="corpus_counts",      expectedResult=true)
    }
    
    "contain realization nothing[Baseline:baseline]" in {
      test(goals, packedGraph, Seq(("Baseline","baseline")),                                    taskName="nothing",            expectedResult=true)
      test(goals, packedGraph, Seq(                    ("Foo","bar")                        ),  taskName="nothing",            expectedResult=true)
    }

    "contain realization extract_dictionary[Baseline:baseline]" in {
      test(goals, packedGraph, Seq(("Baseline","baseline")),                                    taskName="extract_dictionary", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","train"),              ("TrainCorpus","small")),  taskName="extract_dictionary", expectedResult=true)
    }

    "contain realization preproc[Baseline:baseline]" in {
      test(goals, packedGraph, Seq(("Baseline","baseline")),                                    taskName="preproc",            expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","train"),("Side","src"),("TrainCorpus","small")), taskName="preproc",            expectedResult=true)
    }

    "contain realization preproc[Side:tgt]" in {
      test(goals, packedGraph, Seq(("DataSet","train"),("Side","tgt"),("TrainCorpus","small")), taskName="preproc",            expectedResult=true)
    }
    
  }   



  
  "The packed graph for build_model of acid.tape workflow with the complete plan" should {
  
    val plan = """
plan {
	reach build_model via (DataSet: *)     * 
	                                    (TrainCorpus: *) * 
	                                    (TestSplit: *)   * 
	                                    (UseDict: *)     * 
	                                    (OnlyOne: *)     * 
	                                    (Foo: *)         * 
	                                    (OptimizerSeed: *)
}
      """
    
    val workflow = GrammarParser.readWorkflow(Acid.tape + plan)
    val packedGraph = new PackedGraph(workflow)
    val goals = packedGraph.goals

    print(goals)
    
    "contain goals" in {   
      assertResult(20)(goals.size)
    }



    "contain task build_model with realization Baseline.baseline" in {
      test(goals, packedGraph, Seq(("Baseline","baseline")), taskName="build_model", expectedResult=true)
      test(goals, packedGraph, Seq(("TrainCorpus","small"),("UseDict","no")), taskName="build_model", expectedResult=true)
    }

    "contain task build_model with realization DataSet.test+TestSplit.random+TrainCorpus.large+UseDict.yes" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","yes")), taskName="build_model", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("OnlyOne","one"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","yes")), taskName="build_model", expectedResult=true)
    }

    "contain task build_model with realization DataSet.test+TestSplit.random+UseDict.yes" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("TestSplit","random"),("UseDict","yes")), taskName="build_model", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("OnlyOne","one"),("TestSplit","random"),("TrainCorpus","small"),("UseDict","yes")), taskName="build_model", expectedResult=true)
    }

    "contain task build_model with realization DataSet.test+TrainCorpus.large+UseDict.yes" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("TrainCorpus","large"),("UseDict","yes")), taskName="build_model", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("OnlyOne","one"),("TestSplit","standard"),("TrainCorpus","large"),("UseDict","yes")), taskName="build_model", expectedResult=true)
    }

    "contain task build_model with realization DataSet.test+UseDict.yes" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("UseDict","yes")), taskName="build_model", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("OnlyOne","one"),("TestSplit","standard"),("TrainCorpus","small"),("UseDict","yes")), taskName="build_model", expectedResult=true)
    }

    "contain task build_model with realization TrainCorpus.large" in {
      test(goals, packedGraph, Seq(("TrainCorpus","large")), taskName="build_model", expectedResult=true)
      test(goals, packedGraph, Seq(("TrainCorpus","large"),("UseDict","no")), taskName="build_model", expectedResult=true)
    }

    "contain task build_model with realization TrainCorpus.large+UseDict.yes" in {
      test(goals, packedGraph, Seq(("TrainCorpus","large"),("UseDict","yes")), taskName="build_model", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","train"),("OnlyOne","one"),("TrainCorpus","large"),("UseDict","yes")), taskName="build_model", expectedResult=true)
    }

    "contain task build_model with realization UseDict.yes" in {
      test(goals, packedGraph, Seq(("UseDict","yes")), taskName="build_model", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","train"),("OnlyOne","one"),("TrainCorpus","small"),("UseDict","yes")), taskName="build_model", expectedResult=true)
    }

    "contain task extract_dictionary with realization Baseline.baseline" in {
      test(goals, packedGraph, Seq(("Baseline","baseline")), taskName="extract_dictionary", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","train"),("TrainCorpus","small")), taskName="extract_dictionary", expectedResult=true)
    }

    "contain task extract_dictionary with realization DataSet.test" in {
      test(goals, packedGraph, Seq(("DataSet","test")), taskName="extract_dictionary", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("TestSplit","standard")), taskName="extract_dictionary", expectedResult=true)
    }

    "contain task extract_dictionary with realization DataSet.test+TestSplit.random" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("TestSplit","random")), taskName="extract_dictionary", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("TestSplit","random")), taskName="extract_dictionary", expectedResult=true)
    }

    "contain task extract_dictionary with realization TrainCorpus.large" in {
      test(goals, packedGraph, Seq(("TrainCorpus","large")), taskName="extract_dictionary", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","train"),("TrainCorpus","large")), taskName="extract_dictionary", expectedResult=true)
    }

    "contain task preproc with realization Baseline.baseline" in {
      test(goals, packedGraph, Seq(("Baseline","baseline")), taskName="preproc", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","train"),("Side","src"),("TrainCorpus","small")), taskName="preproc", expectedResult=true)
    }

    "contain task preproc with realization Side.tgt" in {
      test(goals, packedGraph, Seq(("Side","tgt")), taskName="preproc", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","train"),("Side","tgt"),("TrainCorpus","small")), taskName="preproc", expectedResult=true)
    }

    "contain task preproc with realization Side.tgt+TrainCorpus.large" in {
      test(goals, packedGraph, Seq(("Side","tgt"),("TrainCorpus","large")), taskName="preproc", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","train"),("Side","tgt"),("TrainCorpus","large")), taskName="preproc", expectedResult=true)
    }

    "contain task preproc with realization TrainCorpus.large" in {
      test(goals, packedGraph, Seq(("TrainCorpus","large")), taskName="preproc", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","train"),("Side","src"),("TrainCorpus","large")), taskName="preproc", expectedResult=true)
    }

    "contain task process_dict with realization Baseline.baseline" in {
      test(goals, packedGraph, Seq(("Baseline","baseline")), taskName="process_dict", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","train"),("OnlyOne","one"),("TrainCorpus","small")), taskName="process_dict", expectedResult=true)
    }

    "contain task process_dict with realization DataSet.test" in {
      test(goals, packedGraph, Seq(("DataSet","test")), taskName="process_dict", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("OnlyOne","one"),("TestSplit","standard")), taskName="process_dict", expectedResult=true)
    }

    "contain task process_dict with realization DataSet.test+TestSplit.random" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("TestSplit","random")), taskName="process_dict", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("OnlyOne","one"),("TestSplit","random")), taskName="process_dict", expectedResult=true)
    }

    "contain task process_dict with realization TrainCorpus.large" in {
      test(goals, packedGraph, Seq(("TrainCorpus","large")), taskName="process_dict", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","train"),("OnlyOne","one"),("TrainCorpus","large")), taskName="process_dict", expectedResult=true)
    }
  }    

  "The packed graph for process_dict of acid.tape workflow with the complete plan" should {
  
    val plan = """
plan {
	reach process_dict via (DataSet: *)     * 
	                                    (TrainCorpus: *) * 
	                                    (TestSplit: *)   * 
	                                    (UseDict: *)     * 
	                                    (OnlyOne: *)     * 
	                                    (Foo: *)         * 
	                                    (OptimizerSeed: *)
}
      """
    
    val workflow = GrammarParser.readWorkflow(Acid.tape + plan)
    val packedGraph = new PackedGraph(workflow)
    val goals = packedGraph.goals

    //print(goals)
    
    "contain goals" in {   
      assertResult(8)(goals.size)
    }

     "contain task extract_dictionary with realization Baseline.baseline" in {
      test(goals, packedGraph, Seq(("Baseline","baseline")), taskName="extract_dictionary", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","train"),("TrainCorpus","small")), taskName="extract_dictionary", expectedResult=true)
    }

    "contain task extract_dictionary with realization DataSet.test" in {
      test(goals, packedGraph, Seq(("DataSet","test")), taskName="extract_dictionary", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("TestSplit","standard")), taskName="extract_dictionary", expectedResult=true)
    }

    "contain task extract_dictionary with realization DataSet.test+TestSplit.random" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("TestSplit","random")), taskName="extract_dictionary", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("TestSplit","random")), taskName="extract_dictionary", expectedResult=true)
    }

    "contain task extract_dictionary with realization TrainCorpus.large" in {
      test(goals, packedGraph, Seq(("TrainCorpus","large")), taskName="extract_dictionary", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","train"),("TrainCorpus","large")), taskName="extract_dictionary", expectedResult=true)
    }

    "contain task process_dict with realization Baseline.baseline" in {
      test(goals, packedGraph, Seq(("Baseline","baseline")), taskName="process_dict", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","train"),("OnlyOne","one"),("TrainCorpus","small")), taskName="process_dict", expectedResult=true)
    }

    "contain task process_dict with realization DataSet.test" in {
      test(goals, packedGraph, Seq(("DataSet","test")), taskName="process_dict", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("OnlyOne","one"),("TestSplit","standard")), taskName="process_dict", expectedResult=true)
    }

    "contain task process_dict with realization DataSet.test+TestSplit.random" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("TestSplit","random")), taskName="process_dict", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("OnlyOne","one"),("TestSplit","random")), taskName="process_dict", expectedResult=true)
    }

    "contain task process_dict with realization TrainCorpus.large" in {
      test(goals, packedGraph, Seq(("TrainCorpus","large")), taskName="process_dict", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","train"),("OnlyOne","one"),("TrainCorpus","large")), taskName="process_dict", expectedResult=true)
    }
  }   
  
  "The packed graph for acid.tape workflow with the complete plan" should {
  
    val plan = """
plan {
	reach evaluate_one,evaluate_all via (DataSet: *)     * 
	                                    (TrainCorpus: *) * 
	                                    (TestSplit: *)   * 
	                                    (UseDict: *)     * 
	                                    (OnlyOne: *)     * 
	                                    (Foo: *)         * 
	                                    (OptimizerSeed: *)
}
      """
    
    val workflow = GrammarParser.readWorkflow(Acid.tape + plan)
    val packedGraph = new PackedGraph(workflow)
    
    "contain a global variable" in {
      packedGraph.global("null") match {
        case None => fail()
        case Some(_) => /* succeed */ 
      }
    }
    
    val goals = packedGraph.goals

    //print(goals)
    
    "contain goals" in {   
      assertResult(101)(goals.size)
    }

    """contain realization build_model[Seq(("TrainCorpus","small"),("UseDict","no"))]""" in {
      test(goals, packedGraph, Seq(("Baseline","baseline")), taskName="build_model", expectedResult=true)
      test(goals, packedGraph, Seq(("TrainCorpus","small"),("UseDict","no")), taskName="build_model", expectedResult=true)
    }

    """contain realization build_model[Seq(("DataSet","test"),("OnlyOne","one"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","yes"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","yes")), taskName="build_model", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("OnlyOne","one"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","yes")), taskName="build_model", expectedResult=true)
    }

    """contain realization build_model[Seq(("DataSet","test"),("OnlyOne","one"),("TestSplit","random"),("TrainCorpus","small"),("UseDict","yes"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("TestSplit","random"),("UseDict","yes")), taskName="build_model", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("OnlyOne","one"),("TestSplit","random"),("TrainCorpus","small"),("UseDict","yes")), taskName="build_model", expectedResult=true)
    }

    """contain realization build_model[Seq(("DataSet","test"),("OnlyOne","one"),("TestSplit","standard"),("TrainCorpus","large"),("UseDict","yes"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("TrainCorpus","large"),("UseDict","yes")), taskName="build_model", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("OnlyOne","one"),("TestSplit","standard"),("TrainCorpus","large"),("UseDict","yes")), taskName="build_model", expectedResult=true)
    }

    """contain realization build_model[Seq(("DataSet","test"),("OnlyOne","one"),("TestSplit","standard"),("TrainCorpus","small"),("UseDict","yes"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("UseDict","yes")), taskName="build_model", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("OnlyOne","one"),("TestSplit","standard"),("TrainCorpus","small"),("UseDict","yes")), taskName="build_model", expectedResult=true)
    }

    """contain realization build_model[Seq(("TrainCorpus","large"),("UseDict","no"))]""" in {
      test(goals, packedGraph, Seq(("TrainCorpus","large")), taskName="build_model", expectedResult=true)
      test(goals, packedGraph, Seq(("TrainCorpus","large"),("UseDict","no")), taskName="build_model", expectedResult=true)
    }

    """contain realization build_model[Seq(("DataSet","train"),("OnlyOne","one"),("TrainCorpus","large"),("UseDict","yes"))]""" in {
      test(goals, packedGraph, Seq(("TrainCorpus","large"),("UseDict","yes")), taskName="build_model", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","train"),("OnlyOne","one"),("TrainCorpus","large"),("UseDict","yes")), taskName="build_model", expectedResult=true)
    }

    """contain realization build_model[Seq(("DataSet","train"),("OnlyOne","one"),("TrainCorpus","small"),("UseDict","yes"))]""" in {
      test(goals, packedGraph, Seq(("UseDict","yes")), taskName="build_model", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","train"),("OnlyOne","one"),("TrainCorpus","small"),("UseDict","yes")), taskName="build_model", expectedResult=true)
    }

    """contain realization corpus_counts[Seq(("DataSet","train"),("Foo","bar"),("TrainCorpus","small"))]""" in {
      test(goals, packedGraph, Seq(("Baseline","baseline")), taskName="corpus_counts", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","train"),("Foo","bar"),("TrainCorpus","small")), taskName="corpus_counts", expectedResult=true)
    }

    """contain realization corpus_counts[Seq(("DataSet","test"),("Foo","bar"),("TestSplit","standard"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test")), taskName="corpus_counts", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("Foo","bar"),("TestSplit","standard")), taskName="corpus_counts", expectedResult=true)
    }

    """contain realization corpus_counts[Seq(("DataSet","test"),("Foo","bar"),("TestSplit","random"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("TestSplit","random")), taskName="corpus_counts", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("Foo","bar"),("TestSplit","random")), taskName="corpus_counts", expectedResult=true)
    }

    """contain realization corpus_counts[Seq(("DataSet","train"),("Foo","bar"),("TrainCorpus","large"))]""" in {
      test(goals, packedGraph, Seq(("TrainCorpus","large")), taskName="corpus_counts", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","train"),("Foo","bar"),("TrainCorpus","large")), taskName="corpus_counts", expectedResult=true)
    }

    """contain realization evaluate_all[Seq(("DataSet","train"),("Foo","bar"),("TrainCorpus","small"),("UseDict","no"))]""" in {
      test(goals, packedGraph, Seq(("Baseline","baseline")), taskName="evaluate_all", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","train"),("Foo","bar"),("TrainCorpus","small"),("UseDict","no")), taskName="evaluate_all", expectedResult=true)
    }

    """contain realization evaluate_all[Seq(("DataSet","test"),("Foo","bar"),("TestSplit","standard"),("TrainCorpus","small"),("UseDict","no"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test")), taskName="evaluate_all", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("Foo","bar"),("TestSplit","standard"),("TrainCorpus","small"),("UseDict","no")), taskName="evaluate_all", expectedResult=true)
    }

    """contain realization evaluate_all[Seq(("DataSet","test"),("Foo","bar"),("TestSplit","random"),("TrainCorpus","small"),("UseDict","no"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("TestSplit","random")), taskName="evaluate_all", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("Foo","bar"),("TestSplit","random"),("TrainCorpus","small"),("UseDict","no")), taskName="evaluate_all", expectedResult=true)
    }

    """contain realization evaluate_all[Seq(("DataSet","test"),("Foo","bar"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","no"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("TestSplit","random"),("TrainCorpus","large")), taskName="evaluate_all", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("Foo","bar"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","no")), taskName="evaluate_all", expectedResult=true)
    }

    """contain realization evaluate_all[Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","yes"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","yes")), taskName="evaluate_all", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","yes")), taskName="evaluate_all", expectedResult=true)
    }

    """contain realization evaluate_all[Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("TestSplit","random"),("TrainCorpus","small"),("UseDict","yes"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("TestSplit","random"),("UseDict","yes")), taskName="evaluate_all", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("TestSplit","random"),("TrainCorpus","small"),("UseDict","yes")), taskName="evaluate_all", expectedResult=true)
    }

    """contain realization evaluate_all[Seq(("DataSet","test"),("Foo","bar"),("TestSplit","standard"),("TrainCorpus","large"),("UseDict","no"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("TrainCorpus","large")), taskName="evaluate_all", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("Foo","bar"),("TestSplit","standard"),("TrainCorpus","large"),("UseDict","no")), taskName="evaluate_all", expectedResult=true)
    }

    """contain realization evaluate_all[Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("TestSplit","standard"),("TrainCorpus","large"),("UseDict","yes"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("TrainCorpus","large"),("UseDict","yes")), taskName="evaluate_all", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("TestSplit","standard"),("TrainCorpus","large"),("UseDict","yes")), taskName="evaluate_all", expectedResult=true)
    }

    """contain realization evaluate_all[Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("TestSplit","standard"),("TrainCorpus","small"),("UseDict","yes"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("UseDict","yes")), taskName="evaluate_all", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("TestSplit","standard"),("TrainCorpus","small"),("UseDict","yes")), taskName="evaluate_all", expectedResult=true)
    }

    """contain realization evaluate_all[Seq(("DataSet","train"),("Foo","bar"),("TrainCorpus","large"),("UseDict","no"))]""" in {
      test(goals, packedGraph, Seq(("TrainCorpus","large")), taskName="evaluate_all", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","train"),("Foo","bar"),("TrainCorpus","large"),("UseDict","no")), taskName="evaluate_all", expectedResult=true)
    }

    """contain realization evaluate_all[Seq(("DataSet","train"),("Foo","bar"),("OnlyOne","one"),("TrainCorpus","large"),("UseDict","yes"))]""" in {
      test(goals, packedGraph, Seq(("TrainCorpus","large"),("UseDict","yes")), taskName="evaluate_all", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","train"),("Foo","bar"),("OnlyOne","one"),("TrainCorpus","large"),("UseDict","yes")), taskName="evaluate_all", expectedResult=true)
    }

    """contain realization evaluate_all[Seq(("DataSet","train"),("Foo","bar"),("OnlyOne","one"),("TrainCorpus","small"),("UseDict","yes"))]""" in {
      test(goals, packedGraph, Seq(("UseDict","yes")), taskName="evaluate_all", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","train"),("Foo","bar"),("OnlyOne","one"),("TrainCorpus","small"),("UseDict","yes")), taskName="evaluate_all", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","train"),("Foo","bar"),("OptimizerSeed","1"),("TrainCorpus","small"),("UseDict","no"))]""" in {
      test(goals, packedGraph, Seq(("Baseline","baseline")), taskName="evaluate_one", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","train"),("Foo","bar"),("OptimizerSeed","1"),("TrainCorpus","small"),("UseDict","no")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","test"),("Foo","bar"),("OptimizerSeed","1"),("TestSplit","standard"),("TrainCorpus","small"),("UseDict","no"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test")), taskName="evaluate_one", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("Foo","bar"),("OptimizerSeed","1"),("TestSplit","standard"),("TrainCorpus","small"),("UseDict","no")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","test"),("Foo","bar"),("OptimizerSeed","2"),("TestSplit","standard"),("TrainCorpus","small"),("UseDict","no"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("OptimizerSeed","2")), taskName="evaluate_one", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("Foo","bar"),("OptimizerSeed","2"),("TestSplit","standard"),("TrainCorpus","small"),("UseDict","no")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","test"),("Foo","bar"),("OptimizerSeed","2"),("TestSplit","random"),("TrainCorpus","small"),("UseDict","no"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("OptimizerSeed","2"),("TestSplit","random")), taskName="evaluate_one", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("Foo","bar"),("OptimizerSeed","2"),("TestSplit","random"),("TrainCorpus","small"),("UseDict","no")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","test"),("Foo","bar"),("OptimizerSeed","2"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","no"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("OptimizerSeed","2"),("TestSplit","random"),("TrainCorpus","large")), taskName="evaluate_one", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("Foo","bar"),("OptimizerSeed","2"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","no")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","2"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","yes"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("OptimizerSeed","2"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","2"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","2"),("TestSplit","random"),("TrainCorpus","small"),("UseDict","yes"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("OptimizerSeed","2"),("TestSplit","random"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","2"),("TestSplit","random"),("TrainCorpus","small"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","test"),("Foo","bar"),("OptimizerSeed","2"),("TestSplit","standard"),("TrainCorpus","large"),("UseDict","no"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("OptimizerSeed","2"),("TrainCorpus","large")), taskName="evaluate_one", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("Foo","bar"),("OptimizerSeed","2"),("TestSplit","standard"),("TrainCorpus","large"),("UseDict","no")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","2"),("TestSplit","standard"),("TrainCorpus","large"),("UseDict","yes"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("OptimizerSeed","2"),("TrainCorpus","large"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","2"),("TestSplit","standard"),("TrainCorpus","large"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","2"),("TestSplit","standard"),("TrainCorpus","small"),("UseDict","yes"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("OptimizerSeed","2"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","2"),("TestSplit","standard"),("TrainCorpus","small"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","test"),("Foo","bar"),("OptimizerSeed","3"),("TestSplit","standard"),("TrainCorpus","small"),("UseDict","no"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("OptimizerSeed","3")), taskName="evaluate_one", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("Foo","bar"),("OptimizerSeed","3"),("TestSplit","standard"),("TrainCorpus","small"),("UseDict","no")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","test"),("Foo","bar"),("OptimizerSeed","3"),("TestSplit","random"),("TrainCorpus","small"),("UseDict","no"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("OptimizerSeed","3"),("TestSplit","random")), taskName="evaluate_one", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("Foo","bar"),("OptimizerSeed","3"),("TestSplit","random"),("TrainCorpus","small"),("UseDict","no")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","test"),("Foo","bar"),("OptimizerSeed","3"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","no"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("OptimizerSeed","3"),("TestSplit","random"),("TrainCorpus","large")), taskName="evaluate_one", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("Foo","bar"),("OptimizerSeed","3"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","no")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","3"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","yes"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("OptimizerSeed","3"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","3"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","3"),("TestSplit","random"),("TrainCorpus","small"),("UseDict","yes"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("OptimizerSeed","3"),("TestSplit","random"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","3"),("TestSplit","random"),("TrainCorpus","small"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","test"),("Foo","bar"),("OptimizerSeed","3"),("TestSplit","standard"),("TrainCorpus","large"),("UseDict","no"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("OptimizerSeed","3"),("TrainCorpus","large")), taskName="evaluate_one", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("Foo","bar"),("OptimizerSeed","3"),("TestSplit","standard"),("TrainCorpus","large"),("UseDict","no")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","3"),("TestSplit","standard"),("TrainCorpus","large"),("UseDict","yes"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("OptimizerSeed","3"),("TrainCorpus","large"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","3"),("TestSplit","standard"),("TrainCorpus","large"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","3"),("TestSplit","standard"),("TrainCorpus","small"),("UseDict","yes"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("OptimizerSeed","3"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","3"),("TestSplit","standard"),("TrainCorpus","small"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","test"),("Foo","bar"),("OptimizerSeed","1"),("TestSplit","random"),("TrainCorpus","small"),("UseDict","no"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("TestSplit","random")), taskName="evaluate_one", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("Foo","bar"),("OptimizerSeed","1"),("TestSplit","random"),("TrainCorpus","small"),("UseDict","no")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","test"),("Foo","bar"),("OptimizerSeed","1"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","no"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("TestSplit","random"),("TrainCorpus","large")), taskName="evaluate_one", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("Foo","bar"),("OptimizerSeed","1"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","no")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","1"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","yes"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","1"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","1"),("TestSplit","random"),("TrainCorpus","small"),("UseDict","yes"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("TestSplit","random"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","1"),("TestSplit","random"),("TrainCorpus","small"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","test"),("Foo","bar"),("OptimizerSeed","1"),("TestSplit","standard"),("TrainCorpus","large"),("UseDict","no"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("TrainCorpus","large")), taskName="evaluate_one", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("Foo","bar"),("OptimizerSeed","1"),("TestSplit","standard"),("TrainCorpus","large"),("UseDict","no")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","1"),("TestSplit","standard"),("TrainCorpus","large"),("UseDict","yes"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("TrainCorpus","large"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","1"),("TestSplit","standard"),("TrainCorpus","large"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","1"),("TestSplit","standard"),("TrainCorpus","small"),("UseDict","yes"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","1"),("TestSplit","standard"),("TrainCorpus","small"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","train"),("Foo","bar"),("OptimizerSeed","2"),("TrainCorpus","small"),("UseDict","no"))]""" in {
      test(goals, packedGraph, Seq(("OptimizerSeed","2")), taskName="evaluate_one", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","train"),("Foo","bar"),("OptimizerSeed","2"),("TrainCorpus","small"),("UseDict","no")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","train"),("Foo","bar"),("OptimizerSeed","2"),("TrainCorpus","large"),("UseDict","no"))]""" in {
      test(goals, packedGraph, Seq(("OptimizerSeed","2"),("TrainCorpus","large")), taskName="evaluate_one", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","train"),("Foo","bar"),("OptimizerSeed","2"),("TrainCorpus","large"),("UseDict","no")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","train"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","2"),("TrainCorpus","large"),("UseDict","yes"))]""" in {
      test(goals, packedGraph, Seq(("OptimizerSeed","2"),("TrainCorpus","large"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","train"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","2"),("TrainCorpus","large"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","train"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","2"),("TrainCorpus","small"),("UseDict","yes"))]""" in {
      test(goals, packedGraph, Seq(("OptimizerSeed","2"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","train"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","2"),("TrainCorpus","small"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","train"),("Foo","bar"),("OptimizerSeed","3"),("TrainCorpus","small"),("UseDict","no"))]""" in {
      test(goals, packedGraph, Seq(("OptimizerSeed","3")), taskName="evaluate_one", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","train"),("Foo","bar"),("OptimizerSeed","3"),("TrainCorpus","small"),("UseDict","no")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","train"),("Foo","bar"),("OptimizerSeed","3"),("TrainCorpus","large"),("UseDict","no"))]""" in {
      test(goals, packedGraph, Seq(("OptimizerSeed","3"),("TrainCorpus","large")), taskName="evaluate_one", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","train"),("Foo","bar"),("OptimizerSeed","3"),("TrainCorpus","large"),("UseDict","no")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","train"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","3"),("TrainCorpus","large"),("UseDict","yes"))]""" in {
      test(goals, packedGraph, Seq(("OptimizerSeed","3"),("TrainCorpus","large"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","train"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","3"),("TrainCorpus","large"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","train"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","3"),("TrainCorpus","small"),("UseDict","yes"))]""" in {
      test(goals, packedGraph, Seq(("OptimizerSeed","3"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","train"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","3"),("TrainCorpus","small"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","train"),("Foo","bar"),("OptimizerSeed","1"),("TrainCorpus","large"),("UseDict","no"))]""" in {
      test(goals, packedGraph, Seq(("TrainCorpus","large")), taskName="evaluate_one", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","train"),("Foo","bar"),("OptimizerSeed","1"),("TrainCorpus","large"),("UseDict","no")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","train"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","1"),("TrainCorpus","large"),("UseDict","yes"))]""" in {
      test(goals, packedGraph, Seq(("TrainCorpus","large"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","train"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","1"),("TrainCorpus","large"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","train"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","1"),("TrainCorpus","small"),("UseDict","yes"))]""" in {
      test(goals, packedGraph, Seq(("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","train"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","1"),("TrainCorpus","small"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization extract_dictionary[Seq(("DataSet","train"),("TrainCorpus","small"))]""" in {
      test(goals, packedGraph, Seq(("Baseline","baseline")), taskName="extract_dictionary", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","train"),("TrainCorpus","small")), taskName="extract_dictionary", expectedResult=true)
    }

    """contain realization extract_dictionary[Seq(("DataSet","test"),("TestSplit","standard"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test")), taskName="extract_dictionary", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("TestSplit","standard")), taskName="extract_dictionary", expectedResult=true)
    }

    """contain realization extract_dictionary[Seq(("DataSet","test"),("TestSplit","random"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("TestSplit","random")), taskName="extract_dictionary", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("TestSplit","random")), taskName="extract_dictionary", expectedResult=true)
    }

    """contain realization extract_dictionary[Seq(("DataSet","train"),("TrainCorpus","large"))]""" in {
      test(goals, packedGraph, Seq(("TrainCorpus","large")), taskName="extract_dictionary", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","train"),("TrainCorpus","large")), taskName="extract_dictionary", expectedResult=true)
    }

    """contain realization nothing[Seq(("Foo","bar"))]""" in {
      test(goals, packedGraph, Seq(("Baseline","baseline")), taskName="nothing", expectedResult=true)
      test(goals, packedGraph, Seq(("Foo","bar")), taskName="nothing", expectedResult=true)
    }

    """contain realization optimize[Seq(("OptimizerSeed","1"),("TrainCorpus","small"),("UseDict","no"))]""" in {
      test(goals, packedGraph, Seq(("Baseline","baseline")), taskName="optimize", expectedResult=true)
      test(goals, packedGraph, Seq(("OptimizerSeed","1"),("TrainCorpus","small"),("UseDict","no")), taskName="optimize", expectedResult=true)
    }

    """contain realization optimize[Seq(("DataSet","test"),("OnlyOne","one"),("OptimizerSeed","2"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","yes"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("OptimizerSeed","2"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","yes")), taskName="optimize", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("OnlyOne","one"),("OptimizerSeed","2"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","yes")), taskName="optimize", expectedResult=true)
    }

    """contain realization optimize[Seq(("DataSet","test"),("OnlyOne","one"),("OptimizerSeed","2"),("TestSplit","random"),("TrainCorpus","small"),("UseDict","yes"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("OptimizerSeed","2"),("TestSplit","random"),("UseDict","yes")), taskName="optimize", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("OnlyOne","one"),("OptimizerSeed","2"),("TestSplit","random"),("TrainCorpus","small"),("UseDict","yes")), taskName="optimize", expectedResult=true)
    }

    """contain realization optimize[Seq(("DataSet","test"),("OnlyOne","one"),("OptimizerSeed","2"),("TestSplit","standard"),("TrainCorpus","large"),("UseDict","yes"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("OptimizerSeed","2"),("TrainCorpus","large"),("UseDict","yes")), taskName="optimize", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("OnlyOne","one"),("OptimizerSeed","2"),("TestSplit","standard"),("TrainCorpus","large"),("UseDict","yes")), taskName="optimize", expectedResult=true)
    }

    """contain realization optimize[Seq(("DataSet","test"),("OnlyOne","one"),("OptimizerSeed","2"),("TestSplit","standard"),("TrainCorpus","small"),("UseDict","yes"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("OptimizerSeed","2"),("UseDict","yes")), taskName="optimize", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("OnlyOne","one"),("OptimizerSeed","2"),("TestSplit","standard"),("TrainCorpus","small"),("UseDict","yes")), taskName="optimize", expectedResult=true)
    }

    """contain realization optimize[Seq(("DataSet","test"),("OnlyOne","one"),("OptimizerSeed","3"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","yes"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("OptimizerSeed","3"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","yes")), taskName="optimize", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("OnlyOne","one"),("OptimizerSeed","3"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","yes")), taskName="optimize", expectedResult=true)
    }

    """contain realization optimize[Seq(("DataSet","test"),("OnlyOne","one"),("OptimizerSeed","3"),("TestSplit","random"),("TrainCorpus","small"),("UseDict","yes"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("OptimizerSeed","3"),("TestSplit","random"),("UseDict","yes")), taskName="optimize", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("OnlyOne","one"),("OptimizerSeed","3"),("TestSplit","random"),("TrainCorpus","small"),("UseDict","yes")), taskName="optimize", expectedResult=true)
    }

    """contain realization optimize[Seq(("DataSet","test"),("OnlyOne","one"),("OptimizerSeed","3"),("TestSplit","standard"),("TrainCorpus","large"),("UseDict","yes"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("OptimizerSeed","3"),("TrainCorpus","large"),("UseDict","yes")), taskName="optimize", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("OnlyOne","one"),("OptimizerSeed","3"),("TestSplit","standard"),("TrainCorpus","large"),("UseDict","yes")), taskName="optimize", expectedResult=true)
    }

    """contain realization optimize[Seq(("DataSet","test"),("OnlyOne","one"),("OptimizerSeed","3"),("TestSplit","standard"),("TrainCorpus","small"),("UseDict","yes"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("OptimizerSeed","3"),("UseDict","yes")), taskName="optimize", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("OnlyOne","one"),("OptimizerSeed","3"),("TestSplit","standard"),("TrainCorpus","small"),("UseDict","yes")), taskName="optimize", expectedResult=true)
    }

    """contain realization optimize[Seq(("DataSet","test"),("OnlyOne","one"),("OptimizerSeed","1"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","yes"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","yes")), taskName="optimize", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("OnlyOne","one"),("OptimizerSeed","1"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","yes")), taskName="optimize", expectedResult=true)
    }

    """contain realization optimize[Seq(("DataSet","test"),("OnlyOne","one"),("OptimizerSeed","1"),("TestSplit","random"),("TrainCorpus","small"),("UseDict","yes"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("TestSplit","random"),("UseDict","yes")), taskName="optimize", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("OnlyOne","one"),("OptimizerSeed","1"),("TestSplit","random"),("TrainCorpus","small"),("UseDict","yes")), taskName="optimize", expectedResult=true)
    }

    """contain realization optimize[Seq(("DataSet","test"),("OnlyOne","one"),("OptimizerSeed","1"),("TestSplit","standard"),("TrainCorpus","large"),("UseDict","yes"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("TrainCorpus","large"),("UseDict","yes")), taskName="optimize", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("OnlyOne","one"),("OptimizerSeed","1"),("TestSplit","standard"),("TrainCorpus","large"),("UseDict","yes")), taskName="optimize", expectedResult=true)
    }

    """contain realization optimize[Seq(("DataSet","test"),("OnlyOne","one"),("OptimizerSeed","1"),("TestSplit","standard"),("TrainCorpus","small"),("UseDict","yes"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("UseDict","yes")), taskName="optimize", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("OnlyOne","one"),("OptimizerSeed","1"),("TestSplit","standard"),("TrainCorpus","small"),("UseDict","yes")), taskName="optimize", expectedResult=true)
    }

    """contain realization optimize[Seq(("OptimizerSeed","2"),("TrainCorpus","small"),("UseDict","no"))]""" in {
      test(goals, packedGraph, Seq(("OptimizerSeed","2")), taskName="optimize", expectedResult=true)
      test(goals, packedGraph, Seq(("OptimizerSeed","2"),("TrainCorpus","small"),("UseDict","no")), taskName="optimize", expectedResult=true)
    }

    """contain realization optimize[Seq(("OptimizerSeed","2"),("TrainCorpus","large"),("UseDict","no"))]""" in {
      test(goals, packedGraph, Seq(("OptimizerSeed","2"),("TrainCorpus","large")), taskName="optimize", expectedResult=true)
      test(goals, packedGraph, Seq(("OptimizerSeed","2"),("TrainCorpus","large"),("UseDict","no")), taskName="optimize", expectedResult=true)
    }

    """contain realization optimize[Seq(("DataSet","train"),("OnlyOne","one"),("OptimizerSeed","2"),("TrainCorpus","large"),("UseDict","yes"))]""" in {
      test(goals, packedGraph, Seq(("OptimizerSeed","2"),("TrainCorpus","large"),("UseDict","yes")), taskName="optimize", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","train"),("OnlyOne","one"),("OptimizerSeed","2"),("TrainCorpus","large"),("UseDict","yes")), taskName="optimize", expectedResult=true)
    }

    """contain realization optimize[Seq(("DataSet","train"),("OnlyOne","one"),("OptimizerSeed","2"),("TrainCorpus","small"),("UseDict","yes"))]""" in {
      test(goals, packedGraph, Seq(("OptimizerSeed","2"),("UseDict","yes")), taskName="optimize", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","train"),("OnlyOne","one"),("OptimizerSeed","2"),("TrainCorpus","small"),("UseDict","yes")), taskName="optimize", expectedResult=true)
    }

    """contain realization optimize[Seq(("OptimizerSeed","3"),("TrainCorpus","small"),("UseDict","no"))]""" in {
      test(goals, packedGraph, Seq(("OptimizerSeed","3")), taskName="optimize", expectedResult=true)
      test(goals, packedGraph, Seq(("OptimizerSeed","3"),("TrainCorpus","small"),("UseDict","no")), taskName="optimize", expectedResult=true)
    }

    """contain realization optimize[Seq(("OptimizerSeed","3"),("TrainCorpus","large"),("UseDict","no"))]""" in {
      test(goals, packedGraph, Seq(("OptimizerSeed","3"),("TrainCorpus","large")), taskName="optimize", expectedResult=true)
      test(goals, packedGraph, Seq(("OptimizerSeed","3"),("TrainCorpus","large"),("UseDict","no")), taskName="optimize", expectedResult=true)
    }

    """contain realization optimize[Seq(("DataSet","train"),("OnlyOne","one"),("OptimizerSeed","3"),("TrainCorpus","large"),("UseDict","yes"))]""" in {
      test(goals, packedGraph, Seq(("OptimizerSeed","3"),("TrainCorpus","large"),("UseDict","yes")), taskName="optimize", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","train"),("OnlyOne","one"),("OptimizerSeed","3"),("TrainCorpus","large"),("UseDict","yes")), taskName="optimize", expectedResult=true)
    }

    """contain realization optimize[Seq(("DataSet","train"),("OnlyOne","one"),("OptimizerSeed","3"),("TrainCorpus","small"),("UseDict","yes"))]""" in {
      test(goals, packedGraph, Seq(("OptimizerSeed","3"),("UseDict","yes")), taskName="optimize", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","train"),("OnlyOne","one"),("OptimizerSeed","3"),("TrainCorpus","small"),("UseDict","yes")), taskName="optimize", expectedResult=true)
    }

    """contain realization optimize[Seq(("OptimizerSeed","1"),("TrainCorpus","large"),("UseDict","no"))]""" in {
      test(goals, packedGraph, Seq(("TrainCorpus","large")), taskName="optimize", expectedResult=true)
      test(goals, packedGraph, Seq(("OptimizerSeed","1"),("TrainCorpus","large"),("UseDict","no")), taskName="optimize", expectedResult=true)
    }

    """contain realization optimize[Seq(("DataSet","train"),("OnlyOne","one"),("OptimizerSeed","1"),("TrainCorpus","large"),("UseDict","yes"))]""" in {
      test(goals, packedGraph, Seq(("TrainCorpus","large"),("UseDict","yes")), taskName="optimize", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","train"),("OnlyOne","one"),("OptimizerSeed","1"),("TrainCorpus","large"),("UseDict","yes")), taskName="optimize", expectedResult=true)
    }

    """contain realization optimize[Seq(("DataSet","train"),("OnlyOne","one"),("OptimizerSeed","1"),("TrainCorpus","small"),("UseDict","yes"))]""" in {
      test(goals, packedGraph, Seq(("UseDict","yes")), taskName="optimize", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","train"),("OnlyOne","one"),("OptimizerSeed","1"),("TrainCorpus","small"),("UseDict","yes")), taskName="optimize", expectedResult=true)
    }

    """contain realization preproc[Seq(("DataSet","train"),("Side","src"),("TrainCorpus","small"))]""" in {
      test(goals, packedGraph, Seq(("Baseline","baseline")), taskName="preproc", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","train"),("Side","src"),("TrainCorpus","small")), taskName="preproc", expectedResult=true)
    }

    """contain realization preproc[Seq(("DataSet","test"),("Side","src"),("TestSplit","standard"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test")), taskName="preproc", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("Side","src"),("TestSplit","standard")), taskName="preproc", expectedResult=true)
    }

    """contain realization preproc[Seq(("DataSet","test"),("Side","tgt"),("TestSplit","standard"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("Side","tgt")), taskName="preproc", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("Side","tgt"),("TestSplit","standard")), taskName="preproc", expectedResult=true)
    }

    """contain realization preproc[Seq(("DataSet","test"),("Side","tgt"),("TestSplit","random"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("Side","tgt"),("TestSplit","random")), taskName="preproc", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("Side","tgt"),("TestSplit","random")), taskName="preproc", expectedResult=true)
    }

    """contain realization preproc[Seq(("DataSet","test"),("Side","src"),("TestSplit","random"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("TestSplit","random")), taskName="preproc", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("Side","src"),("TestSplit","random")), taskName="preproc", expectedResult=true)
    }

    """contain realization preproc[Seq(("DataSet","train"),("Side","tgt"),("TrainCorpus","small"))]""" in {
      test(goals, packedGraph, Seq(("Side","tgt")), taskName="preproc", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","train"),("Side","tgt"),("TrainCorpus","small")), taskName="preproc", expectedResult=true)
    }

    """contain realization preproc[Seq(("DataSet","train"),("Side","tgt"),("TrainCorpus","large"))]""" in {
      test(goals, packedGraph, Seq(("Side","tgt"),("TrainCorpus","large")), taskName="preproc", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","train"),("Side","tgt"),("TrainCorpus","large")), taskName="preproc", expectedResult=true)
    }

    """contain realization preproc[Seq(("DataSet","train"),("Side","src"),("TrainCorpus","large"))]""" in {
      test(goals, packedGraph, Seq(("TrainCorpus","large")), taskName="preproc", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","train"),("Side","src"),("TrainCorpus","large")), taskName="preproc", expectedResult=true)
    }

    """contain realization process_dict[Seq(("DataSet","train"),("OnlyOne","one"),("TrainCorpus","small"))]""" in {
      test(goals, packedGraph, Seq(("Baseline","baseline")), taskName="process_dict", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","train"),("OnlyOne","one"),("TrainCorpus","small")), taskName="process_dict", expectedResult=true)
    }

    """contain realization process_dict[Seq(("DataSet","test"),("OnlyOne","one"),("TestSplit","standard"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test")), taskName="process_dict", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("OnlyOne","one"),("TestSplit","standard")), taskName="process_dict", expectedResult=true)
    }

    """contain realization process_dict[Seq(("DataSet","test"),("OnlyOne","one"),("TestSplit","random"))]""" in {
      test(goals, packedGraph, Seq(("DataSet","test"),("TestSplit","random")), taskName="process_dict", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","test"),("OnlyOne","one"),("TestSplit","random")), taskName="process_dict", expectedResult=true)
    }

    """contain realization process_dict[Seq(("DataSet","train"),("OnlyOne","one"),("TrainCorpus","large"))]""" in {
      test(goals, packedGraph, Seq(("TrainCorpus","large")), taskName="process_dict", expectedResult=true)
      test(goals, packedGraph, Seq(("DataSet","train"),("OnlyOne","one"),("TrainCorpus","large")), taskName="process_dict", expectedResult=true)
    }

  }    
  
  def test(goals:Goals, packedGraph:PackedGraph, tuples:Seq[(String,String)], taskName:String, expectedResult:Boolean): Unit = {
    
    val branches = Seq.newBuilder[Branch]
    tuples.foreach{ case (branchPointName, branchName) => 
      val branch = packedGraph.branchFactory(name=branchName, branchPoint=branchPointName)
      branches += branch
    }
    //val sortedBranches = branches.result().sortWith{(a,b) => a.toString() < b.toString()}
    val realization = Realization.fromUnsorted(branches.result())
      
    goals.values.get(taskName) match {
      case Some(result) => assertResult(expectedResult)(result.contains(realization))
      case None         => fail()
    }    
  }
  
  def test(goals:Goals, packedGraph:PackedGraph, branchPointName:String, branchName:String, taskName:String, expectedResult:Boolean): Unit = {
    val tuple = (branchPointName, branchName)
    test(goals, packedGraph, Seq(tuple), taskName, expectedResult)  
  }
}