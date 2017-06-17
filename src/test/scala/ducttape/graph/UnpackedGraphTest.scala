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
class UnpackedGraphTest extends WordSpec {
  
  import ducttape.graph.UnpackedGraphTest.unpack
  
  "The unpacked graph for an empty workflow" should {

    val string = ""
    
    val graph = unpack(string)
    
    "contain zero tasks" in {
      assertResult(0)(graph.tasks.size)
    }
    
  }
  

  "The unpacked graph for a workflow with 1 task, no branchpoints, and no plans" should {

    val string = """
      task foo {
        echo "hello"
      }
      """
    
    val graph = unpack(string)
    
    s"contain a single task with ${Task.NO_REALIZATION}" in {
      assertResult(1)(graph.tasks.size)
      assertResult(Task.NO_REALIZATION)(graph.tasks.toSeq.head.realization)
    }

  }

  "The unpacked graph for a workflow with 1 task, 1 branchpoint, and no plans" should {

    val string = """
      task foo 
        :: greeting=(Language: en="hello" de="Hallo" es="Hola")
      {
        echo "${greeting}"
      }
      """
    
    val graph = unpack(string)
    
    s"contain a single task with ${Task.NO_REALIZATION}" in {
      assertResult(1)(graph.tasks.size)
      assertResult(Task.NO_REALIZATION)(graph.tasks.toSeq.head.realization)
    }

  }

 
  "The unpacked graph for a workflow with 1 task, 1 branchpoint, and plans" should {

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
    
    val graph = unpack(string)
       
    "contain 2 tasks" in {   
      assertResult(2)(graph.tasks.size)
    }
    
    "contain realization foo(Language: en)" in {
      test(graph, branchPointName="Language", branchName="en", taskName="foo", expectedResult=true)
    }
    
    "contain realization foo(Language: de)" in {
      test(graph, branchPointName="Language", branchName="de", taskName="foo", expectedResult=true)
    }
    
    "not contain realization foo(Language: es)" in {
      test(graph, branchPointName="Language", branchName="es", taskName="foo", expectedResult=false)
    }
        
    "not contain realization (Language: sv)" in {
      assertThrows[ducttape.workflow.NoSuchBranchException] { 
        test(graph, branchPointName="Language", branchName="sv", taskName="foo", expectedResult=false)
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
    
    val graph = unpack(string)
       
    "contain 1 task" in {   
      assertResult(1)(graph.tasks.size)
    }
    
    "contain realization foo(Language: en)" in {
      test(graph, branchPointName="Language", branchName="en", taskName="foo", expectedResult=true)
    }
    
    "not contain realization foo(Language: de)" in {
      test(graph, branchPointName="Language", branchName="de", taskName="foo", expectedResult=false)
    }
    
    "not contain realization foo(Language: es)" in {
      test(graph, branchPointName="Language", branchName="es", taskName="foo", expectedResult=false)
    }
        
    "not contain realization (Language: sv)" in {
      assertThrows[ducttape.workflow.NoSuchBranchException] { 
        test(graph, branchPointName="Language", branchName="sv", taskName="foo", expectedResult=false)
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
    
    val graph = unpack(string)
       
    "contain 3 tasks" in {   
      assertResult(3)(graph.tasks.size)
    }
    
    "contain realization foo[Language: en]" in {
      test(graph, branchPointName="Language", branchName="en", taskName="foo", expectedResult=true)
    }

    "contain realization foo[Language: de]" in {
      test(graph, branchPointName="Language", branchName="de", taskName="foo", expectedResult=true)
    }
 
    "not contain realization foo[Language: es]" in {
      test(graph, branchPointName="Language", branchName="es", taskName="foo", expectedResult=false)
    }
    
    "not contain realization bar[Language: en]" in {
      test(graph, branchPointName="Language", branchName="en", taskName="bar", expectedResult=false)
    }

    "contain realization bar[Language: de]" in {
      test(graph, branchPointName="Language", branchName="de", taskName="bar", expectedResult=true)
    }
    
    "not contain realization bar[Language: es]" in {
      test(graph, branchPointName="Language", branchName="es", taskName="bar", expectedResult=false)
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
    
    val graph = unpack(string)
       
    "contain 3 tasks" in {   
      assertResult(3)(graph.tasks.size)
    }
    
    "contain realization foo[Language: en]" in {
      test(graph, branchPointName="Language", branchName="en", taskName="foo", expectedResult=true)
    }

    "contain realization foo[Language: de]" in {
      test(graph, branchPointName="Language", branchName="de", taskName="foo", expectedResult=true)
    }
 
    "not contain realization foo[Language: es]" in {
      test(graph, branchPointName="Language", branchName="es", taskName="foo", expectedResult=false)
    }
    
    "not contain realization bar[Language: en]" in {
      test(graph, branchPointName="Language", branchName="en", taskName="bar", expectedResult=false)
    }

    "contain realization bar[Language: de]" in {
      test(graph, branchPointName="Language", branchName="de", taskName="bar", expectedResult=true)
    }
    
    "not contain realization bar[Language: es]" in {
      test(graph, branchPointName="Language", branchName="es", taskName="bar", expectedResult=false)
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
    
    val graph = unpack(string)
       
    "contain 3 tasks" in {   
      assertResult(3)(graph.tasks.size)
    }
    
    "contain realization foo[Language: en]" in {
      test(graph, branchPointName="Language", branchName="en", taskName="foo", expectedResult=true)
    }

    "contain realization foo[Language: de]" in {
      test(graph, branchPointName="Language", branchName="de", taskName="foo", expectedResult=true)
    }
 
    "not contain realization foo[Language: es]" in {
      test(graph, branchPointName="Language", branchName="es", taskName="foo", expectedResult=false)
    }
    
    "not contain realization bar[Language: en]" in {
      test(graph, branchPointName="Language", branchName="en", taskName="bar", expectedResult=false)
    }

    "contain realization bar[Language: de]" in {
      test(graph, branchPointName="Language", branchName="de", taskName="bar", expectedResult=true)
    }
    
    "not contain realization bar[Language: es]" in {
      test(graph, branchPointName="Language", branchName="es", taskName="bar", expectedResult=false)
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
    
    val graph = unpack(string)
       
    "contain 3 tasks" in {   
      assertResult(3)(graph.tasks.size)
    }
    
    "contain realization foo[Language: en]" in {
      test(graph, branchPointName="Language", branchName="en", taskName="foo", expectedResult=true)
    }

    "contain realization foo[Language: de]" in {
      test(graph, branchPointName="Language", branchName="de", taskName="foo", expectedResult=true)
    }
 
    "not contain realization foo[Language: es]" in {
      test(graph, branchPointName="Language", branchName="es", taskName="foo", expectedResult=false)
    }
    
    "contain realization bar[Language: en]" in {
      test(graph, branchPointName="Language", branchName="en", taskName="bar", expectedResult=true)
    }

    "not contain realization bar[Language: de]" in {
      test(graph, branchPointName="Language", branchName="de", taskName="bar", expectedResult=false)
    }
    
    "not contain realization bar[Language: es]" in {
      test(graph, branchPointName="Language", branchName="es", taskName="bar", expectedResult=false)
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
    
    val graph = unpack(string)
       
    "contain 2 tasks" in {   
      assertResult(2)(graph.tasks.size)
    }
    
    "contain realization foo[Language: en]" in {
      test(graph, branchPointName="Language", branchName="en", taskName="foo", expectedResult=true)
    }

    "contain realization foo[Language: de]" in {
      test(graph, branchPointName="Language", branchName="de", taskName="foo", expectedResult=false)
    }
 
    "not contain realization foo[Language: es]" in {
      test(graph, branchPointName="Language", branchName="es", taskName="foo", expectedResult=false)
    }
    
    "contain realization bar[Language: en]" in {
      test(graph, branchPointName="Language", branchName="en", taskName="bar", expectedResult=true)
    }

    "not contain realization bar[Language: de]" in {
      test(graph, branchPointName="Language", branchName="de", taskName="bar", expectedResult=false)
    }
    
    "not contain realization bar[Language: es]" in {
      test(graph, branchPointName="Language", branchName="es", taskName="bar", expectedResult=false)
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
    
    val graph = unpack(string)
       
    "contain 3 tasks" in {   
      assertResult(3)(graph.tasks.size)
    }
    
    "contain realization foo[Language: en]" in {
      test(graph, branchPointName="Language", branchName="en", taskName="foo", expectedResult=true)
    }

    "contain realization foo[Language: de]" in {
      test(graph, branchPointName="Language", branchName="de", taskName="foo", expectedResult=true)
    }
 
    "not contain realization foo[Language: es]" in {
      test(graph, branchPointName="Language", branchName="es", taskName="foo", expectedResult=false)
    }
    
    "contain realization bar[Language: en]" in {
      test(graph, branchPointName="Language", branchName="en", taskName="bar", expectedResult=true)
    }

    "not contain realization bar[Language: de]" in {
      test(graph, branchPointName="Language", branchName="de", taskName="bar", expectedResult=false)
    }
    
    "not contain realization bar[Language: es]" in {
      test(graph, branchPointName="Language", branchName="es", taskName="bar", expectedResult=false)
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
    
    val graph = unpack(string)
       
    "contain 3 tasks" in {   
      assertResult(3)(graph.tasks.size)
    }
    
    "not contain realization foo[Language: en]" in {
      test(graph, branchPointName="Language", branchName="en", taskName="foo", expectedResult=false)
    }

    "contain realization foo[Language: de]" in {
      test(graph, branchPointName="Language", branchName="de", taskName="foo", expectedResult=true)
    }
 
    "contain realization foo[Language: es]" in {
      test(graph, branchPointName="Language", branchName="es", taskName="foo", expectedResult=true)
    }
    
    "contain realization bar[Language: en]" in {
      test(graph, branchPointName="Language", branchName="en", taskName="bar", expectedResult=true)
    }

    "not contain realization bar[Language: de]" in {
      test(graph, branchPointName="Language", branchName="de", taskName="bar", expectedResult=false)
    }
    
    "not contain realization bar[Language: es]" in {
      test(graph, branchPointName="Language", branchName="es", taskName="bar", expectedResult=false)
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
    
    val graph = unpack(string)
       
    "contain 4 tasks" in {   
      assertResult(4)(graph.tasks.size)
    }
    
    "not contain realization foo[Language: en]" in {
      test(graph, branchPointName="Language", branchName="en", taskName="foo", expectedResult=true)
    }

    "contain realization foo[Language: de]" in {
      test(graph, branchPointName="Language", branchName="de", taskName="foo", expectedResult=true)
    }
 
    "not contain realization foo[Language: es]" in {
      test(graph, branchPointName="Language", branchName="es", taskName="foo", expectedResult=true)
    }
    
    "contain realization bar[Language: en]" in {
      test(graph, branchPointName="Language", branchName="en", taskName="bar", expectedResult=true)
    }

    "not contain realization bar[Language: de]" in {
      test(graph, branchPointName="Language", branchName="de", taskName="bar", expectedResult=false)
    }
    
    "not contain realization bar[Language: es]" in {
      test(graph, branchPointName="Language", branchName="es", taskName="bar", expectedResult=false)
    }
  
  }  
  
  "The packed graph for acid.tape workflow with plan to reach nothing with no via" should {
  
    val plan = """
      plan {
        reach nothing
      }
      """
    
    val graph = unpack(Acid.tape + plan)
       
    "contain 1 task" in {   
      assertResult(1)(graph.tasks.size)
    }
    
    "contain realization nothing" in {
      test(graph, branchPointName="Foo", branchName="bar", taskName="nothing", expectedResult=true)
    }
    
  }

  "The packed graph for acid.tape workflow with plan to reach nothing with a via" should {
  
    val plan = """
      plan {
        reach nothing via (Foo: bar)
      }
      """
    
    val graph = unpack(Acid.tape + plan)
       
    "contain 1 task" in {   
      assertResult(1)(graph.tasks.size)
    }
    
    "contain realization nothing" in {
      test(graph, branchPointName="Foo", branchName="bar", taskName="nothing", expectedResult=true)
    }
    
  }  

  "The packed graph for acid.tape workflow with plan to reach nothing with an incompatible via" should {
  
    val plan = """
      plan {
        reach nothing via (DataSet: test)
      }
      """
    
    val graph = unpack(Acid.tape + plan)
       
    "contain 1 task" in {   
      assertResult(1)(graph.tasks.size)
    }
    
    "contain realization nothing[Baseline.baseline]" in {
      test(graph, branchPointName="Baseline", branchName="baseline", taskName="nothing", expectedResult=true)
    }
    
    "contain realization nothing[Foo.bar]" in {
      test(graph, branchPointName="Foo", branchName="bar", taskName="nothing", expectedResult=true)
    }

    "not contain realization nothing[DataSet.test]" in {
      test(graph, branchPointName="DataSet", branchName="test", taskName="nothing", expectedResult=false)
    }
  }    


  "The packed graph for acid.tape workflow with plan to reach extract_dictionary using a plan with no vias" should {
  
    val plan = """
      plan {
        reach extract_dictionary
      }
      """
    
    val graph = unpack(Acid.tape + plan)
    
    "contain 1 task" in {   
      assertResult(1)(graph.tasks.size)
    }
    
    "contain realization extract_dictionary" in {
      test(graph, branchPointName="Baseline", branchName="baseline", taskName="extract_dictionary", expectedResult=true)
    }
    
  }   

  "The packed graph for acid.tape workflow with plan to reach extract_dictionary using a plan with incompatible vias" should {
  
    val plan = """
      plan {
        reach extract_dictionary via (Foo:bar)
      }
      """
    
    val graph = unpack(Acid.tape + plan)
    
    "contain 1 task" in {   
      assertResult(1)(graph.tasks.size)
    }
    
    "contain realization (Baseline:baseline)" in {
      test(graph, branchPointName="Baseline", branchName="baseline", taskName="extract_dictionary", expectedResult=true)
    }
    
  }    

  "The packed graph for acid.tape workflow with plan to reach extract_dictionary using a plan via (DataSet: test) * (TestSplit: standard)" should {
  
    val plan = """
      plan {
        reach extract_dictionary via (DataSet: test) * (TestSplit: standard)
      }
      """
    
    val graph = unpack(Acid.tape + plan)
    
    "contain 1 task" in {   
      assertResult(1)(graph.tasks.size)
    }
    
    "contain realization (DataSet: test) * (TestSplit: standard)" in {
      test(graph, Seq(("DataSet","test"),("TestSplit","standard")), taskName="extract_dictionary", expectedResult=true)
    }
    
    "not contain realization (Baseline:baseline)" in {
      test(graph, branchPointName="Baseline", branchName="baseline", taskName="extract_dictionary", expectedResult=false)
    }
  }    
    
  "The packed graph for acid.tape workflow with plan to reach extract_dictionary using a plan via (DataSet: train) * (TestSplit: standard)" should {
  
    val plan = """
      plan {
        reach extract_dictionary via (DataSet: train) * (TestSplit: standard)
      }
      """
    
    val graph = unpack(Acid.tape + plan)
    
    "contain 1 task" in {   
      assertResult(1)(graph.tasks.size)
    }
    
    "contain realization (DataSet: train) * (TestSplit: standard)" in {
      test(graph, Seq(("DataSet","train"),("TrainCorpus","small")), taskName="extract_dictionary", expectedResult=true)
    }
    
    "contain realization (Baseline:baseline)" in {
      test(graph, branchPointName="Baseline", branchName="baseline", taskName="extract_dictionary", expectedResult=true)
    }
  }  

  "The packed graph for acid.tape workflow with plan to reach extract_dictionary using a plan via (DataSet: train) * (TrainCorpus: small)" should {
  
    val plan = """
      plan {
        reach extract_dictionary via (DataSet: train) * (TrainCorpus: small)
      }
      """
    
    val graph = unpack(Acid.tape + plan)
    
    "contain 1 task" in {   
      assertResult(1)(graph.tasks.size)
    }
    
    "contain realization (DataSet: train) * (TrainCorpus: standard)" in {
      test(graph, Seq(("DataSet","train"),("TrainCorpus","small")), taskName="extract_dictionary", expectedResult=true)
    }

    "not contain realization (DataSet: train) * (TrainCorpus: large)" in {
      test(graph, Seq(("DataSet","train"),("TrainCorpus","large")), taskName="extract_dictionary", expectedResult=false)
    }    
    
    "contain realization (Baseline:baseline)" in {
      test(graph, branchPointName="Baseline", branchName="baseline", taskName="extract_dictionary", expectedResult=true)
    }
  }    

  "The packed graph for acid.tape workflow with plan to reach extract_dictionary using a plan via (DataSet: train) * (TrainCorpus: large)" should {
  
    val plan = """
      plan {
        reach extract_dictionary via (DataSet: train) * (TrainCorpus: large)
      }
      """
    
    val graph = unpack(Acid.tape + plan)
    
    "contain 1 task" in {   
      assertResult(1)(graph.tasks.size)
    }
    
    "contain realization (DataSet: train) * (TestSplit: standard)" in {
      test(graph, Seq(("DataSet","train"),("TrainCorpus","large")), taskName="extract_dictionary", expectedResult=true)
    }
    
    "not contain realization (DataSet: train) * (TrainCorpus: standard)" in {
      test(graph, Seq(("DataSet","train"),("TrainCorpus","small")), taskName="extract_dictionary", expectedResult=false)
    }
    
    "not contain realization (Baseline:baseline)" in {
      test(graph, branchPointName="Baseline", branchName="baseline", taskName="extract_dictionary", expectedResult=false)
    }
  }   

  "The packed graph for acid.tape workflow with plan to reach corpus_counts using a plan with no via" should {
  
    val plan = """
      plan {
        reach corpus_counts
      }
      """
    
    val graph = unpack(Acid.tape + plan)
    
    "contain 5 tasks" in {   
      assertResult(5)(graph.tasks.size)
    }
    
    "contain realization corpus_counts[Baseline:baseline]" in {
      test(graph, Seq(("Baseline","baseline")),                                    taskName="corpus_counts",      expectedResult=true)
      test(graph, Seq(("DataSet","train"),("Foo","bar"),("TrainCorpus","small")),  taskName="corpus_counts",      expectedResult=true)
    }
    
    "contain realization nothing[Baseline:baseline]" in {
      test(graph, Seq(("Baseline","baseline")),                                    taskName="nothing",            expectedResult=true)
      test(graph, Seq(                    ("Foo","bar")                        ),  taskName="nothing",            expectedResult=true)
    }

    "contain realization extract_dictionary[Baseline:baseline]" in {
      test(graph, Seq(("Baseline","baseline")),                                    taskName="extract_dictionary", expectedResult=true)
      test(graph, Seq(("DataSet","train"),              ("TrainCorpus","small")),  taskName="extract_dictionary", expectedResult=true)
    }

    "contain realization preproc[Baseline:baseline]" in {
      test(graph, Seq(("Baseline","baseline")),                                    taskName="preproc",            expectedResult=true)
      test(graph, Seq(("DataSet","train"),("Side","src"),("TrainCorpus","small")), taskName="preproc",            expectedResult=true)
    }

    "contain realization preproc[Side:tgt]" in {
      test(graph, Seq(("DataSet","train"),("Side","tgt"),("TrainCorpus","small")), taskName="preproc",            expectedResult=true)
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
    
    val graph = unpack(Acid.tape + plan)

    "contain 20 tasks" in {   
      assertResult(20)(graph.tasks.size)
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
    
    val graph = unpack(Acid.tape + plan)
    
    "contain 8 tasks" in {   
      assertResult(8)(graph.tasks.size)
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
    
    val graph = unpack(Acid.tape + plan)
    
    "contain 101 tasks" in {   
      assertResult(101)(graph.tasks.size)
    }

    """contain realization build_model[Seq(("TrainCorpus","small"),("UseDict","no"))]""" in {
      test(graph, Seq(("Baseline","baseline")), taskName="build_model", expectedResult=true)
      test(graph, Seq(("TrainCorpus","small"),("UseDict","no")), taskName="build_model", expectedResult=true)
    }

    """contain realization build_model[Seq(("DataSet","test"),("OnlyOne","one"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","yes"))]""" in {
      test(graph, Seq(("DataSet","test"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","yes")), taskName="build_model", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("OnlyOne","one"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","yes")), taskName="build_model", expectedResult=true)
    }

    """contain realization build_model[Seq(("DataSet","test"),("OnlyOne","one"),("TestSplit","random"),("TrainCorpus","small"),("UseDict","yes"))]""" in {
      test(graph, Seq(("DataSet","test"),("TestSplit","random"),("UseDict","yes")), taskName="build_model", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("OnlyOne","one"),("TestSplit","random"),("TrainCorpus","small"),("UseDict","yes")), taskName="build_model", expectedResult=true)
    }

    """contain realization build_model[Seq(("DataSet","test"),("OnlyOne","one"),("TestSplit","standard"),("TrainCorpus","large"),("UseDict","yes"))]""" in {
      test(graph, Seq(("DataSet","test"),("TrainCorpus","large"),("UseDict","yes")), taskName="build_model", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("OnlyOne","one"),("TestSplit","standard"),("TrainCorpus","large"),("UseDict","yes")), taskName="build_model", expectedResult=true)
    }

    """contain realization build_model[Seq(("DataSet","test"),("OnlyOne","one"),("TestSplit","standard"),("TrainCorpus","small"),("UseDict","yes"))]""" in {
      test(graph, Seq(("DataSet","test"),("UseDict","yes")), taskName="build_model", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("OnlyOne","one"),("TestSplit","standard"),("TrainCorpus","small"),("UseDict","yes")), taskName="build_model", expectedResult=true)
    }

    """contain realization build_model[Seq(("TrainCorpus","large"),("UseDict","no"))]""" in {
      test(graph, Seq(("TrainCorpus","large")), taskName="build_model", expectedResult=true)
      test(graph, Seq(("TrainCorpus","large"),("UseDict","no")), taskName="build_model", expectedResult=true)
    }

    """contain realization build_model[Seq(("DataSet","train"),("OnlyOne","one"),("TrainCorpus","large"),("UseDict","yes"))]""" in {
      test(graph, Seq(("TrainCorpus","large"),("UseDict","yes")), taskName="build_model", expectedResult=true)
      test(graph, Seq(("DataSet","train"),("OnlyOne","one"),("TrainCorpus","large"),("UseDict","yes")), taskName="build_model", expectedResult=true)
    }

    """contain realization build_model[Seq(("DataSet","train"),("OnlyOne","one"),("TrainCorpus","small"),("UseDict","yes"))]""" in {
      test(graph, Seq(("UseDict","yes")), taskName="build_model", expectedResult=true)
      test(graph, Seq(("DataSet","train"),("OnlyOne","one"),("TrainCorpus","small"),("UseDict","yes")), taskName="build_model", expectedResult=true)
    }

    """contain realization corpus_counts[Seq(("DataSet","train"),("Foo","bar"),("TrainCorpus","small"))]""" in {
      test(graph, Seq(("Baseline","baseline")), taskName="corpus_counts", expectedResult=true)
      test(graph, Seq(("DataSet","train"),("Foo","bar"),("TrainCorpus","small")), taskName="corpus_counts", expectedResult=true)
    }

    """contain realization corpus_counts[Seq(("DataSet","test"),("Foo","bar"),("TestSplit","standard"))]""" in {
      test(graph, Seq(("DataSet","test")), taskName="corpus_counts", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("Foo","bar"),("TestSplit","standard")), taskName="corpus_counts", expectedResult=true)
    }

    """contain realization corpus_counts[Seq(("DataSet","test"),("Foo","bar"),("TestSplit","random"))]""" in {
      test(graph, Seq(("DataSet","test"),("TestSplit","random")), taskName="corpus_counts", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("Foo","bar"),("TestSplit","random")), taskName="corpus_counts", expectedResult=true)
    }

    """contain realization corpus_counts[Seq(("DataSet","train"),("Foo","bar"),("TrainCorpus","large"))]""" in {
      test(graph, Seq(("TrainCorpus","large")), taskName="corpus_counts", expectedResult=true)
      test(graph, Seq(("DataSet","train"),("Foo","bar"),("TrainCorpus","large")), taskName="corpus_counts", expectedResult=true)
    }

    """contain realization evaluate_all[Seq(("DataSet","train"),("Foo","bar"),("TrainCorpus","small"),("UseDict","no"))]""" in {
      test(graph, Seq(("Baseline","baseline")), taskName="evaluate_all", expectedResult=true)
      test(graph, Seq(("DataSet","train"),("Foo","bar"),("TrainCorpus","small"),("UseDict","no")), taskName="evaluate_all", expectedResult=true)
    }

    """contain realization evaluate_all[Seq(("DataSet","test"),("Foo","bar"),("TestSplit","standard"),("TrainCorpus","small"),("UseDict","no"))]""" in {
      test(graph, Seq(("DataSet","test")), taskName="evaluate_all", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("Foo","bar"),("TestSplit","standard"),("TrainCorpus","small"),("UseDict","no")), taskName="evaluate_all", expectedResult=true)
    }

    """contain realization evaluate_all[Seq(("DataSet","test"),("Foo","bar"),("TestSplit","random"),("TrainCorpus","small"),("UseDict","no"))]""" in {
      test(graph, Seq(("DataSet","test"),("TestSplit","random")), taskName="evaluate_all", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("Foo","bar"),("TestSplit","random"),("TrainCorpus","small"),("UseDict","no")), taskName="evaluate_all", expectedResult=true)
    }

    """contain realization evaluate_all[Seq(("DataSet","test"),("Foo","bar"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","no"))]""" in {
      test(graph, Seq(("DataSet","test"),("TestSplit","random"),("TrainCorpus","large")), taskName="evaluate_all", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("Foo","bar"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","no")), taskName="evaluate_all", expectedResult=true)
    }

    """contain realization evaluate_all[Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","yes"))]""" in {
      test(graph, Seq(("DataSet","test"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","yes")), taskName="evaluate_all", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","yes")), taskName="evaluate_all", expectedResult=true)
    }

    """contain realization evaluate_all[Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("TestSplit","random"),("TrainCorpus","small"),("UseDict","yes"))]""" in {
      test(graph, Seq(("DataSet","test"),("TestSplit","random"),("UseDict","yes")), taskName="evaluate_all", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("TestSplit","random"),("TrainCorpus","small"),("UseDict","yes")), taskName="evaluate_all", expectedResult=true)
    }

    """contain realization evaluate_all[Seq(("DataSet","test"),("Foo","bar"),("TestSplit","standard"),("TrainCorpus","large"),("UseDict","no"))]""" in {
      test(graph, Seq(("DataSet","test"),("TrainCorpus","large")), taskName="evaluate_all", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("Foo","bar"),("TestSplit","standard"),("TrainCorpus","large"),("UseDict","no")), taskName="evaluate_all", expectedResult=true)
    }

    """contain realization evaluate_all[Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("TestSplit","standard"),("TrainCorpus","large"),("UseDict","yes"))]""" in {
      test(graph, Seq(("DataSet","test"),("TrainCorpus","large"),("UseDict","yes")), taskName="evaluate_all", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("TestSplit","standard"),("TrainCorpus","large"),("UseDict","yes")), taskName="evaluate_all", expectedResult=true)
    }

    """contain realization evaluate_all[Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("TestSplit","standard"),("TrainCorpus","small"),("UseDict","yes"))]""" in {
      test(graph, Seq(("DataSet","test"),("UseDict","yes")), taskName="evaluate_all", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("TestSplit","standard"),("TrainCorpus","small"),("UseDict","yes")), taskName="evaluate_all", expectedResult=true)
    }

    """contain realization evaluate_all[Seq(("DataSet","train"),("Foo","bar"),("TrainCorpus","large"),("UseDict","no"))]""" in {
      test(graph, Seq(("TrainCorpus","large")), taskName="evaluate_all", expectedResult=true)
      test(graph, Seq(("DataSet","train"),("Foo","bar"),("TrainCorpus","large"),("UseDict","no")), taskName="evaluate_all", expectedResult=true)
    }

    """contain realization evaluate_all[Seq(("DataSet","train"),("Foo","bar"),("OnlyOne","one"),("TrainCorpus","large"),("UseDict","yes"))]""" in {
      test(graph, Seq(("TrainCorpus","large"),("UseDict","yes")), taskName="evaluate_all", expectedResult=true)
      test(graph, Seq(("DataSet","train"),("Foo","bar"),("OnlyOne","one"),("TrainCorpus","large"),("UseDict","yes")), taskName="evaluate_all", expectedResult=true)
    }

    """contain realization evaluate_all[Seq(("DataSet","train"),("Foo","bar"),("OnlyOne","one"),("TrainCorpus","small"),("UseDict","yes"))]""" in {
      test(graph, Seq(("UseDict","yes")), taskName="evaluate_all", expectedResult=true)
      test(graph, Seq(("DataSet","train"),("Foo","bar"),("OnlyOne","one"),("TrainCorpus","small"),("UseDict","yes")), taskName="evaluate_all", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","train"),("Foo","bar"),("OptimizerSeed","1"),("TrainCorpus","small"),("UseDict","no"))]""" in {
      test(graph, Seq(("Baseline","baseline")), taskName="evaluate_one", expectedResult=true)
      test(graph, Seq(("DataSet","train"),("Foo","bar"),("OptimizerSeed","1"),("TrainCorpus","small"),("UseDict","no")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","test"),("Foo","bar"),("OptimizerSeed","1"),("TestSplit","standard"),("TrainCorpus","small"),("UseDict","no"))]""" in {
      test(graph, Seq(("DataSet","test")), taskName="evaluate_one", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("Foo","bar"),("OptimizerSeed","1"),("TestSplit","standard"),("TrainCorpus","small"),("UseDict","no")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","test"),("Foo","bar"),("OptimizerSeed","2"),("TestSplit","standard"),("TrainCorpus","small"),("UseDict","no"))]""" in {
      test(graph, Seq(("DataSet","test"),("OptimizerSeed","2")), taskName="evaluate_one", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("Foo","bar"),("OptimizerSeed","2"),("TestSplit","standard"),("TrainCorpus","small"),("UseDict","no")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","test"),("Foo","bar"),("OptimizerSeed","2"),("TestSplit","random"),("TrainCorpus","small"),("UseDict","no"))]""" in {
      test(graph, Seq(("DataSet","test"),("OptimizerSeed","2"),("TestSplit","random")), taskName="evaluate_one", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("Foo","bar"),("OptimizerSeed","2"),("TestSplit","random"),("TrainCorpus","small"),("UseDict","no")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","test"),("Foo","bar"),("OptimizerSeed","2"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","no"))]""" in {
      test(graph, Seq(("DataSet","test"),("OptimizerSeed","2"),("TestSplit","random"),("TrainCorpus","large")), taskName="evaluate_one", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("Foo","bar"),("OptimizerSeed","2"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","no")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","2"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","yes"))]""" in {
      test(graph, Seq(("DataSet","test"),("OptimizerSeed","2"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","2"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","2"),("TestSplit","random"),("TrainCorpus","small"),("UseDict","yes"))]""" in {
      test(graph, Seq(("DataSet","test"),("OptimizerSeed","2"),("TestSplit","random"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","2"),("TestSplit","random"),("TrainCorpus","small"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","test"),("Foo","bar"),("OptimizerSeed","2"),("TestSplit","standard"),("TrainCorpus","large"),("UseDict","no"))]""" in {
      test(graph, Seq(("DataSet","test"),("OptimizerSeed","2"),("TrainCorpus","large")), taskName="evaluate_one", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("Foo","bar"),("OptimizerSeed","2"),("TestSplit","standard"),("TrainCorpus","large"),("UseDict","no")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","2"),("TestSplit","standard"),("TrainCorpus","large"),("UseDict","yes"))]""" in {
      test(graph, Seq(("DataSet","test"),("OptimizerSeed","2"),("TrainCorpus","large"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","2"),("TestSplit","standard"),("TrainCorpus","large"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","2"),("TestSplit","standard"),("TrainCorpus","small"),("UseDict","yes"))]""" in {
      test(graph, Seq(("DataSet","test"),("OptimizerSeed","2"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","2"),("TestSplit","standard"),("TrainCorpus","small"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","test"),("Foo","bar"),("OptimizerSeed","3"),("TestSplit","standard"),("TrainCorpus","small"),("UseDict","no"))]""" in {
      test(graph, Seq(("DataSet","test"),("OptimizerSeed","3")), taskName="evaluate_one", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("Foo","bar"),("OptimizerSeed","3"),("TestSplit","standard"),("TrainCorpus","small"),("UseDict","no")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","test"),("Foo","bar"),("OptimizerSeed","3"),("TestSplit","random"),("TrainCorpus","small"),("UseDict","no"))]""" in {
      test(graph, Seq(("DataSet","test"),("OptimizerSeed","3"),("TestSplit","random")), taskName="evaluate_one", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("Foo","bar"),("OptimizerSeed","3"),("TestSplit","random"),("TrainCorpus","small"),("UseDict","no")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","test"),("Foo","bar"),("OptimizerSeed","3"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","no"))]""" in {
      test(graph, Seq(("DataSet","test"),("OptimizerSeed","3"),("TestSplit","random"),("TrainCorpus","large")), taskName="evaluate_one", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("Foo","bar"),("OptimizerSeed","3"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","no")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","3"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","yes"))]""" in {
      test(graph, Seq(("DataSet","test"),("OptimizerSeed","3"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","3"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","3"),("TestSplit","random"),("TrainCorpus","small"),("UseDict","yes"))]""" in {
      test(graph, Seq(("DataSet","test"),("OptimizerSeed","3"),("TestSplit","random"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","3"),("TestSplit","random"),("TrainCorpus","small"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","test"),("Foo","bar"),("OptimizerSeed","3"),("TestSplit","standard"),("TrainCorpus","large"),("UseDict","no"))]""" in {
      test(graph, Seq(("DataSet","test"),("OptimizerSeed","3"),("TrainCorpus","large")), taskName="evaluate_one", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("Foo","bar"),("OptimizerSeed","3"),("TestSplit","standard"),("TrainCorpus","large"),("UseDict","no")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","3"),("TestSplit","standard"),("TrainCorpus","large"),("UseDict","yes"))]""" in {
      test(graph, Seq(("DataSet","test"),("OptimizerSeed","3"),("TrainCorpus","large"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","3"),("TestSplit","standard"),("TrainCorpus","large"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","3"),("TestSplit","standard"),("TrainCorpus","small"),("UseDict","yes"))]""" in {
      test(graph, Seq(("DataSet","test"),("OptimizerSeed","3"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","3"),("TestSplit","standard"),("TrainCorpus","small"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","test"),("Foo","bar"),("OptimizerSeed","1"),("TestSplit","random"),("TrainCorpus","small"),("UseDict","no"))]""" in {
      test(graph, Seq(("DataSet","test"),("TestSplit","random")), taskName="evaluate_one", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("Foo","bar"),("OptimizerSeed","1"),("TestSplit","random"),("TrainCorpus","small"),("UseDict","no")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","test"),("Foo","bar"),("OptimizerSeed","1"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","no"))]""" in {
      test(graph, Seq(("DataSet","test"),("TestSplit","random"),("TrainCorpus","large")), taskName="evaluate_one", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("Foo","bar"),("OptimizerSeed","1"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","no")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","1"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","yes"))]""" in {
      test(graph, Seq(("DataSet","test"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","1"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","1"),("TestSplit","random"),("TrainCorpus","small"),("UseDict","yes"))]""" in {
      test(graph, Seq(("DataSet","test"),("TestSplit","random"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","1"),("TestSplit","random"),("TrainCorpus","small"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","test"),("Foo","bar"),("OptimizerSeed","1"),("TestSplit","standard"),("TrainCorpus","large"),("UseDict","no"))]""" in {
      test(graph, Seq(("DataSet","test"),("TrainCorpus","large")), taskName="evaluate_one", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("Foo","bar"),("OptimizerSeed","1"),("TestSplit","standard"),("TrainCorpus","large"),("UseDict","no")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","1"),("TestSplit","standard"),("TrainCorpus","large"),("UseDict","yes"))]""" in {
      test(graph, Seq(("DataSet","test"),("TrainCorpus","large"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","1"),("TestSplit","standard"),("TrainCorpus","large"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","1"),("TestSplit","standard"),("TrainCorpus","small"),("UseDict","yes"))]""" in {
      test(graph, Seq(("DataSet","test"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","1"),("TestSplit","standard"),("TrainCorpus","small"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","train"),("Foo","bar"),("OptimizerSeed","2"),("TrainCorpus","small"),("UseDict","no"))]""" in {
      test(graph, Seq(("OptimizerSeed","2")), taskName="evaluate_one", expectedResult=true)
      test(graph, Seq(("DataSet","train"),("Foo","bar"),("OptimizerSeed","2"),("TrainCorpus","small"),("UseDict","no")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","train"),("Foo","bar"),("OptimizerSeed","2"),("TrainCorpus","large"),("UseDict","no"))]""" in {
      test(graph, Seq(("OptimizerSeed","2"),("TrainCorpus","large")), taskName="evaluate_one", expectedResult=true)
      test(graph, Seq(("DataSet","train"),("Foo","bar"),("OptimizerSeed","2"),("TrainCorpus","large"),("UseDict","no")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","train"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","2"),("TrainCorpus","large"),("UseDict","yes"))]""" in {
      test(graph, Seq(("OptimizerSeed","2"),("TrainCorpus","large"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
      test(graph, Seq(("DataSet","train"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","2"),("TrainCorpus","large"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","train"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","2"),("TrainCorpus","small"),("UseDict","yes"))]""" in {
      test(graph, Seq(("OptimizerSeed","2"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
      test(graph, Seq(("DataSet","train"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","2"),("TrainCorpus","small"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","train"),("Foo","bar"),("OptimizerSeed","3"),("TrainCorpus","small"),("UseDict","no"))]""" in {
      test(graph, Seq(("OptimizerSeed","3")), taskName="evaluate_one", expectedResult=true)
      test(graph, Seq(("DataSet","train"),("Foo","bar"),("OptimizerSeed","3"),("TrainCorpus","small"),("UseDict","no")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","train"),("Foo","bar"),("OptimizerSeed","3"),("TrainCorpus","large"),("UseDict","no"))]""" in {
      test(graph, Seq(("OptimizerSeed","3"),("TrainCorpus","large")), taskName="evaluate_one", expectedResult=true)
      test(graph, Seq(("DataSet","train"),("Foo","bar"),("OptimizerSeed","3"),("TrainCorpus","large"),("UseDict","no")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","train"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","3"),("TrainCorpus","large"),("UseDict","yes"))]""" in {
      test(graph, Seq(("OptimizerSeed","3"),("TrainCorpus","large"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
      test(graph, Seq(("DataSet","train"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","3"),("TrainCorpus","large"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","train"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","3"),("TrainCorpus","small"),("UseDict","yes"))]""" in {
      test(graph, Seq(("OptimizerSeed","3"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
      test(graph, Seq(("DataSet","train"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","3"),("TrainCorpus","small"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","train"),("Foo","bar"),("OptimizerSeed","1"),("TrainCorpus","large"),("UseDict","no"))]""" in {
      test(graph, Seq(("TrainCorpus","large")), taskName="evaluate_one", expectedResult=true)
      test(graph, Seq(("DataSet","train"),("Foo","bar"),("OptimizerSeed","1"),("TrainCorpus","large"),("UseDict","no")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","train"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","1"),("TrainCorpus","large"),("UseDict","yes"))]""" in {
      test(graph, Seq(("TrainCorpus","large"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
      test(graph, Seq(("DataSet","train"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","1"),("TrainCorpus","large"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization evaluate_one[Seq(("DataSet","train"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","1"),("TrainCorpus","small"),("UseDict","yes"))]""" in {
      test(graph, Seq(("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
      test(graph, Seq(("DataSet","train"),("Foo","bar"),("OnlyOne","one"),("OptimizerSeed","1"),("TrainCorpus","small"),("UseDict","yes")), taskName="evaluate_one", expectedResult=true)
    }

    """contain realization extract_dictionary[Seq(("DataSet","train"),("TrainCorpus","small"))]""" in {
      test(graph, Seq(("Baseline","baseline")), taskName="extract_dictionary", expectedResult=true)
      test(graph, Seq(("DataSet","train"),("TrainCorpus","small")), taskName="extract_dictionary", expectedResult=true)
    }

    """contain realization extract_dictionary[Seq(("DataSet","test"),("TestSplit","standard"))]""" in {
      test(graph, Seq(("DataSet","test")), taskName="extract_dictionary", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("TestSplit","standard")), taskName="extract_dictionary", expectedResult=true)
    }

    """contain realization extract_dictionary[Seq(("DataSet","test"),("TestSplit","random"))]""" in {
      test(graph, Seq(("DataSet","test"),("TestSplit","random")), taskName="extract_dictionary", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("TestSplit","random")), taskName="extract_dictionary", expectedResult=true)
    }

    """contain realization extract_dictionary[Seq(("DataSet","train"),("TrainCorpus","large"))]""" in {
      test(graph, Seq(("TrainCorpus","large")), taskName="extract_dictionary", expectedResult=true)
      test(graph, Seq(("DataSet","train"),("TrainCorpus","large")), taskName="extract_dictionary", expectedResult=true)
    }

    """contain realization nothing[Seq(("Foo","bar"))]""" in {
      test(graph, Seq(("Baseline","baseline")), taskName="nothing", expectedResult=true)
      test(graph, Seq(("Foo","bar")), taskName="nothing", expectedResult=true)
    }

    """contain realization optimize[Seq(("OptimizerSeed","1"),("TrainCorpus","small"),("UseDict","no"))]""" in {
      test(graph, Seq(("Baseline","baseline")), taskName="optimize", expectedResult=true)
      test(graph, Seq(("OptimizerSeed","1"),("TrainCorpus","small"),("UseDict","no")), taskName="optimize", expectedResult=true)
    }

    """contain realization optimize[Seq(("DataSet","test"),("OnlyOne","one"),("OptimizerSeed","2"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","yes"))]""" in {
      test(graph, Seq(("DataSet","test"),("OptimizerSeed","2"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","yes")), taskName="optimize", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("OnlyOne","one"),("OptimizerSeed","2"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","yes")), taskName="optimize", expectedResult=true)
    }

    """contain realization optimize[Seq(("DataSet","test"),("OnlyOne","one"),("OptimizerSeed","2"),("TestSplit","random"),("TrainCorpus","small"),("UseDict","yes"))]""" in {
      test(graph, Seq(("DataSet","test"),("OptimizerSeed","2"),("TestSplit","random"),("UseDict","yes")), taskName="optimize", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("OnlyOne","one"),("OptimizerSeed","2"),("TestSplit","random"),("TrainCorpus","small"),("UseDict","yes")), taskName="optimize", expectedResult=true)
    }

    """contain realization optimize[Seq(("DataSet","test"),("OnlyOne","one"),("OptimizerSeed","2"),("TestSplit","standard"),("TrainCorpus","large"),("UseDict","yes"))]""" in {
      test(graph, Seq(("DataSet","test"),("OptimizerSeed","2"),("TrainCorpus","large"),("UseDict","yes")), taskName="optimize", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("OnlyOne","one"),("OptimizerSeed","2"),("TestSplit","standard"),("TrainCorpus","large"),("UseDict","yes")), taskName="optimize", expectedResult=true)
    }

    """contain realization optimize[Seq(("DataSet","test"),("OnlyOne","one"),("OptimizerSeed","2"),("TestSplit","standard"),("TrainCorpus","small"),("UseDict","yes"))]""" in {
      test(graph, Seq(("DataSet","test"),("OptimizerSeed","2"),("UseDict","yes")), taskName="optimize", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("OnlyOne","one"),("OptimizerSeed","2"),("TestSplit","standard"),("TrainCorpus","small"),("UseDict","yes")), taskName="optimize", expectedResult=true)
    }

    """contain realization optimize[Seq(("DataSet","test"),("OnlyOne","one"),("OptimizerSeed","3"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","yes"))]""" in {
      test(graph, Seq(("DataSet","test"),("OptimizerSeed","3"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","yes")), taskName="optimize", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("OnlyOne","one"),("OptimizerSeed","3"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","yes")), taskName="optimize", expectedResult=true)
    }

    """contain realization optimize[Seq(("DataSet","test"),("OnlyOne","one"),("OptimizerSeed","3"),("TestSplit","random"),("TrainCorpus","small"),("UseDict","yes"))]""" in {
      test(graph, Seq(("DataSet","test"),("OptimizerSeed","3"),("TestSplit","random"),("UseDict","yes")), taskName="optimize", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("OnlyOne","one"),("OptimizerSeed","3"),("TestSplit","random"),("TrainCorpus","small"),("UseDict","yes")), taskName="optimize", expectedResult=true)
    }

    """contain realization optimize[Seq(("DataSet","test"),("OnlyOne","one"),("OptimizerSeed","3"),("TestSplit","standard"),("TrainCorpus","large"),("UseDict","yes"))]""" in {
      test(graph, Seq(("DataSet","test"),("OptimizerSeed","3"),("TrainCorpus","large"),("UseDict","yes")), taskName="optimize", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("OnlyOne","one"),("OptimizerSeed","3"),("TestSplit","standard"),("TrainCorpus","large"),("UseDict","yes")), taskName="optimize", expectedResult=true)
    }

    """contain realization optimize[Seq(("DataSet","test"),("OnlyOne","one"),("OptimizerSeed","3"),("TestSplit","standard"),("TrainCorpus","small"),("UseDict","yes"))]""" in {
      test(graph, Seq(("DataSet","test"),("OptimizerSeed","3"),("UseDict","yes")), taskName="optimize", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("OnlyOne","one"),("OptimizerSeed","3"),("TestSplit","standard"),("TrainCorpus","small"),("UseDict","yes")), taskName="optimize", expectedResult=true)
    }

    """contain realization optimize[Seq(("DataSet","test"),("OnlyOne","one"),("OptimizerSeed","1"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","yes"))]""" in {
      test(graph, Seq(("DataSet","test"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","yes")), taskName="optimize", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("OnlyOne","one"),("OptimizerSeed","1"),("TestSplit","random"),("TrainCorpus","large"),("UseDict","yes")), taskName="optimize", expectedResult=true)
    }

    """contain realization optimize[Seq(("DataSet","test"),("OnlyOne","one"),("OptimizerSeed","1"),("TestSplit","random"),("TrainCorpus","small"),("UseDict","yes"))]""" in {
      test(graph, Seq(("DataSet","test"),("TestSplit","random"),("UseDict","yes")), taskName="optimize", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("OnlyOne","one"),("OptimizerSeed","1"),("TestSplit","random"),("TrainCorpus","small"),("UseDict","yes")), taskName="optimize", expectedResult=true)
    }

    """contain realization optimize[Seq(("DataSet","test"),("OnlyOne","one"),("OptimizerSeed","1"),("TestSplit","standard"),("TrainCorpus","large"),("UseDict","yes"))]""" in {
      test(graph, Seq(("DataSet","test"),("TrainCorpus","large"),("UseDict","yes")), taskName="optimize", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("OnlyOne","one"),("OptimizerSeed","1"),("TestSplit","standard"),("TrainCorpus","large"),("UseDict","yes")), taskName="optimize", expectedResult=true)
    }

    """contain realization optimize[Seq(("DataSet","test"),("OnlyOne","one"),("OptimizerSeed","1"),("TestSplit","standard"),("TrainCorpus","small"),("UseDict","yes"))]""" in {
      test(graph, Seq(("DataSet","test"),("UseDict","yes")), taskName="optimize", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("OnlyOne","one"),("OptimizerSeed","1"),("TestSplit","standard"),("TrainCorpus","small"),("UseDict","yes")), taskName="optimize", expectedResult=true)
    }

    """contain realization optimize[Seq(("OptimizerSeed","2"),("TrainCorpus","small"),("UseDict","no"))]""" in {
      test(graph, Seq(("OptimizerSeed","2")), taskName="optimize", expectedResult=true)
      test(graph, Seq(("OptimizerSeed","2"),("TrainCorpus","small"),("UseDict","no")), taskName="optimize", expectedResult=true)
    }

    """contain realization optimize[Seq(("OptimizerSeed","2"),("TrainCorpus","large"),("UseDict","no"))]""" in {
      test(graph, Seq(("OptimizerSeed","2"),("TrainCorpus","large")), taskName="optimize", expectedResult=true)
      test(graph, Seq(("OptimizerSeed","2"),("TrainCorpus","large"),("UseDict","no")), taskName="optimize", expectedResult=true)
    }

    """contain realization optimize[Seq(("DataSet","train"),("OnlyOne","one"),("OptimizerSeed","2"),("TrainCorpus","large"),("UseDict","yes"))]""" in {
      test(graph, Seq(("OptimizerSeed","2"),("TrainCorpus","large"),("UseDict","yes")), taskName="optimize", expectedResult=true)
      test(graph, Seq(("DataSet","train"),("OnlyOne","one"),("OptimizerSeed","2"),("TrainCorpus","large"),("UseDict","yes")), taskName="optimize", expectedResult=true)
    }

    """contain realization optimize[Seq(("DataSet","train"),("OnlyOne","one"),("OptimizerSeed","2"),("TrainCorpus","small"),("UseDict","yes"))]""" in {
      test(graph, Seq(("OptimizerSeed","2"),("UseDict","yes")), taskName="optimize", expectedResult=true)
      test(graph, Seq(("DataSet","train"),("OnlyOne","one"),("OptimizerSeed","2"),("TrainCorpus","small"),("UseDict","yes")), taskName="optimize", expectedResult=true)
    }

    """contain realization optimize[Seq(("OptimizerSeed","3"),("TrainCorpus","small"),("UseDict","no"))]""" in {
      test(graph, Seq(("OptimizerSeed","3")), taskName="optimize", expectedResult=true)
      test(graph, Seq(("OptimizerSeed","3"),("TrainCorpus","small"),("UseDict","no")), taskName="optimize", expectedResult=true)
    }

    """contain realization optimize[Seq(("OptimizerSeed","3"),("TrainCorpus","large"),("UseDict","no"))]""" in {
      test(graph, Seq(("OptimizerSeed","3"),("TrainCorpus","large")), taskName="optimize", expectedResult=true)
      test(graph, Seq(("OptimizerSeed","3"),("TrainCorpus","large"),("UseDict","no")), taskName="optimize", expectedResult=true)
    }

    """contain realization optimize[Seq(("DataSet","train"),("OnlyOne","one"),("OptimizerSeed","3"),("TrainCorpus","large"),("UseDict","yes"))]""" in {
      test(graph, Seq(("OptimizerSeed","3"),("TrainCorpus","large"),("UseDict","yes")), taskName="optimize", expectedResult=true)
      test(graph, Seq(("DataSet","train"),("OnlyOne","one"),("OptimizerSeed","3"),("TrainCorpus","large"),("UseDict","yes")), taskName="optimize", expectedResult=true)
    }

    """contain realization optimize[Seq(("DataSet","train"),("OnlyOne","one"),("OptimizerSeed","3"),("TrainCorpus","small"),("UseDict","yes"))]""" in {
      test(graph, Seq(("OptimizerSeed","3"),("UseDict","yes")), taskName="optimize", expectedResult=true)
      test(graph, Seq(("DataSet","train"),("OnlyOne","one"),("OptimizerSeed","3"),("TrainCorpus","small"),("UseDict","yes")), taskName="optimize", expectedResult=true)
    }

    """contain realization optimize[Seq(("OptimizerSeed","1"),("TrainCorpus","large"),("UseDict","no"))]""" in {
      test(graph, Seq(("TrainCorpus","large")), taskName="optimize", expectedResult=true)
      test(graph, Seq(("OptimizerSeed","1"),("TrainCorpus","large"),("UseDict","no")), taskName="optimize", expectedResult=true)
    }

    """contain realization optimize[Seq(("DataSet","train"),("OnlyOne","one"),("OptimizerSeed","1"),("TrainCorpus","large"),("UseDict","yes"))]""" in {
      test(graph, Seq(("TrainCorpus","large"),("UseDict","yes")), taskName="optimize", expectedResult=true)
      test(graph, Seq(("DataSet","train"),("OnlyOne","one"),("OptimizerSeed","1"),("TrainCorpus","large"),("UseDict","yes")), taskName="optimize", expectedResult=true)
    }

    """contain realization optimize[Seq(("DataSet","train"),("OnlyOne","one"),("OptimizerSeed","1"),("TrainCorpus","small"),("UseDict","yes"))]""" in {
      test(graph, Seq(("UseDict","yes")), taskName="optimize", expectedResult=true)
      test(graph, Seq(("DataSet","train"),("OnlyOne","one"),("OptimizerSeed","1"),("TrainCorpus","small"),("UseDict","yes")), taskName="optimize", expectedResult=true)
    }

    """contain realization preproc[Seq(("DataSet","train"),("Side","src"),("TrainCorpus","small"))]""" in {
      test(graph, Seq(("Baseline","baseline")), taskName="preproc", expectedResult=true)
      test(graph, Seq(("DataSet","train"),("Side","src"),("TrainCorpus","small")), taskName="preproc", expectedResult=true)
    }

    """contain realization preproc[Seq(("DataSet","test"),("Side","src"),("TestSplit","standard"))]""" in {
      test(graph, Seq(("DataSet","test")), taskName="preproc", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("Side","src"),("TestSplit","standard")), taskName="preproc", expectedResult=true)
    }

    """contain realization preproc[Seq(("DataSet","test"),("Side","tgt"),("TestSplit","standard"))]""" in {
      test(graph, Seq(("DataSet","test"),("Side","tgt")), taskName="preproc", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("Side","tgt"),("TestSplit","standard")), taskName="preproc", expectedResult=true)
    }

    """contain realization preproc[Seq(("DataSet","test"),("Side","tgt"),("TestSplit","random"))]""" in {
      test(graph, Seq(("DataSet","test"),("Side","tgt"),("TestSplit","random")), taskName="preproc", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("Side","tgt"),("TestSplit","random")), taskName="preproc", expectedResult=true)
    }

    """contain realization preproc[Seq(("DataSet","test"),("Side","src"),("TestSplit","random"))]""" in {
      test(graph, Seq(("DataSet","test"),("TestSplit","random")), taskName="preproc", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("Side","src"),("TestSplit","random")), taskName="preproc", expectedResult=true)
    }

    """contain realization preproc[Seq(("DataSet","train"),("Side","tgt"),("TrainCorpus","small"))]""" in {
      test(graph, Seq(("Side","tgt")), taskName="preproc", expectedResult=true)
      test(graph, Seq(("DataSet","train"),("Side","tgt"),("TrainCorpus","small")), taskName="preproc", expectedResult=true)
    }

    """contain realization preproc[Seq(("DataSet","train"),("Side","tgt"),("TrainCorpus","large"))]""" in {
      test(graph, Seq(("Side","tgt"),("TrainCorpus","large")), taskName="preproc", expectedResult=true)
      test(graph, Seq(("DataSet","train"),("Side","tgt"),("TrainCorpus","large")), taskName="preproc", expectedResult=true)
    }

    """contain realization preproc[Seq(("DataSet","train"),("Side","src"),("TrainCorpus","large"))]""" in {
      test(graph, Seq(("TrainCorpus","large")), taskName="preproc", expectedResult=true)
      test(graph, Seq(("DataSet","train"),("Side","src"),("TrainCorpus","large")), taskName="preproc", expectedResult=true)
    }

    """contain realization process_dict[Seq(("DataSet","train"),("OnlyOne","one"),("TrainCorpus","small"))]""" in {
      test(graph, Seq(("Baseline","baseline")), taskName="process_dict", expectedResult=true)
      test(graph, Seq(("DataSet","train"),("OnlyOne","one"),("TrainCorpus","small")), taskName="process_dict", expectedResult=true)
    }

    """contain realization process_dict[Seq(("DataSet","test"),("OnlyOne","one"),("TestSplit","standard"))]""" in {
      test(graph, Seq(("DataSet","test")), taskName="process_dict", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("OnlyOne","one"),("TestSplit","standard")), taskName="process_dict", expectedResult=true)
    }

    """contain realization process_dict[Seq(("DataSet","test"),("OnlyOne","one"),("TestSplit","random"))]""" in {
      test(graph, Seq(("DataSet","test"),("TestSplit","random")), taskName="process_dict", expectedResult=true)
      test(graph, Seq(("DataSet","test"),("OnlyOne","one"),("TestSplit","random")), taskName="process_dict", expectedResult=true)
    }

    """contain realization process_dict[Seq(("DataSet","train"),("OnlyOne","one"),("TrainCorpus","large"))]""" in {
      test(graph, Seq(("TrainCorpus","large")), taskName="process_dict", expectedResult=true)
      test(graph, Seq(("DataSet","train"),("OnlyOne","one"),("TrainCorpus","large")), taskName="process_dict", expectedResult=true)
    }

  }    
    
  def test(graph:UnpackedGraph, tuples:Seq[(String,String)], taskName:String, expectedResult:Boolean): Unit = {
    
    
    val branches = Seq.newBuilder[Branch]
    tuples.foreach{ case (branchPointName, branchName) => 
      val branch = graph.branchFactory(name=branchName, branchPoint=branchPointName)
      branches += branch
    }
    //val sortedBranches = branches.result().sortWith{(a,b) => a.toString() < b.toString()}
    val realization = Realization.fromUnsorted(branches.result())
      
    if (expectedResult==true) {
      if (graph.goals.contains(taskName, realization) == false) {
        fail(s"The goals associated with this unpacked graph were expected to contain task ${taskName} with realization ${realization}, but did not.")
      }
    }
    
    graph.getTask(taskName, realization) match {
      case Some(result) => assertResult(expectedResult)(true)
      case None         => assertResult(expectedResult)(false)
    }    
  }
  
  def test(graph:UnpackedGraph, branchPointName:String, branchName:String, taskName:String, expectedResult:Boolean): Unit = {
    val tuple = (branchPointName, branchName)
    test(graph, Seq(tuple), taskName, expectedResult)  
  }
}

object UnpackedGraphTest {
  def unpack(workflowString:String): UnpackedGraph = {
    val workflow = GrammarParser.readWorkflow(workflowString)
    
    val packedGraph = new PackedGraph(workflow)
    
    val goals = packedGraph.goals
    
    val unpackedGraph = UnpackedGraph.unpack(goals)
    
    return unpackedGraph
  }
}