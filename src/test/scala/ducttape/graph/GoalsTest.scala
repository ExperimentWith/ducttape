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

  val acid = """
    global {
       ducttape_output=tutorial/acid/output
       ducttape_experimental_packages=true

       null="/dev/null"
}

package sametime :: .versioner=git .repo="git://github.com/jhclark/sametime.git" .ref=HEAD {
  ./build.sh
}

task extract_dictionary
	:: param=(DataSet: train=(TrainCorpus: small="Small training corpus" 
	                                       large="Large training corpus") 
	                    test=(TestSplit: standard="Standard test set" 
	                                     random="Randomized test set"))
	: sametime
	> out
{
	echo "Hello, Dictionary. I'm using parameter ${param}" > ${out}
}

task preproc
	< dict_in=$out@extract_dictionary
	< in=(Side: src=(DataSet: train=(TrainCorpus: small=sm.src large=lg.src)
	                    test=(TestSplit: standard=test.src random=rand.src))
	            tgt=(DataSet: train=(TrainCorpus: small=sm.tgt large=lg.tgt) 
	                    test=(TestSplit: standard=test.tgt random=rand.tgt)))
	> out
{
	echo "I'm using dictionary ${dict_in}"
	cat ${dict_in}
	
	tr '[:upper:]' '[:lower:]' < ${in} > ${out}
}

task process_dict
	< in=(OnlyOne: one=$out@extract_dictionary)
	> out
	> baz
{
	echo "Let's calculate a dictionary from ${in}" > ${out}
	touch ${baz}
}

summary sizes {
  of process_dict > FileSize {
    du -sh $out > $FileSize
  }
}

task build_model
	< src=$out@preproc[DataSet:train,Side:src]
	< tgt=$out@preproc[DataSet:train,Side:tgt]
	< dict=(UseDict: no=$null yes=$out@process_dict)
	> out
{
	echo "Dictionary = ${dict}" > ${out}
	echo >> ${out}
	paste ${src} ${tgt} >> ${out}
}

func sample_function
	:: x
	> out
{
	# Do nothing in particular
	echo ${x} > ${out}
}

task nothing calls sample_function
	:: x=(Foo: bar)
	> out 


task corpus_counts
	< dummy=(Foo: bar=$out@nothing[Foo:bar])
	< src=$out@preproc[Side:src]
	< tgt=$out@preproc[Side:tgt]
	> out
{
	wc ${dummy} ${src} ${tgt} > ${out}
}

task optimize
	< in=$out@build_model
	:: seed=(OptimizerSeed: 1..3)
	> out
{
	echo "Seed = ${seed}" > ${out}
	echo "Model = ${in}" >> ${out}
}

task evaluate_one
	< counts=$out@corpus_counts
	< weights=$out@optimize
	< model=$out@build_model
	> out
{
	echo "Counts = ${counts}"    > ${out}
	echo "Weights = ${weights}" >> ${out}
}

task evaluate_all
	< counts=$out@corpus_counts
	< weights=$out@optimize[OptimizerSeed:*]
	< model=$out@build_model
	> out
{
	echo "Counts = ${counts}"    > ${out}
	echo "Weights = ${weights}" >> ${out}
}

    """
  
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
    
    val packedGraph = new PackedGraph(workflow, confSpecs)
        
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
    
    val packedGraph = new PackedGraph(workflow, confSpecs)
        
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
    
    val workflow = GrammarParser.readWorkflow(acid + plan)
    val packedGraph = new PackedGraph(workflow, confSpecs)
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
    
    val workflow = GrammarParser.readWorkflow(acid + plan)
    val packedGraph = new PackedGraph(workflow, confSpecs)
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
    
    val workflow = GrammarParser.readWorkflow(acid + plan)
    val packedGraph = new PackedGraph(workflow, confSpecs)
    val goals = packedGraph.goals
       
    "not contain goals" in {   
      assertResult(0)(goals.size)
    }
    
    "not contain realization nothing" in {
      test(goals, packedGraph, branchPointName="Foo", branchName="bar", taskName="nothing", expectedResult=false)
    }
    
  }    


  "The packed graph for acid.tape workflow with plan to reach extract_dictionary using a plan with no vias" should {
  
    val plan = """
      plan {
        reach extract_dictionary
      }
      """
    
    val workflow = GrammarParser.readWorkflow(acid + plan)
    val packedGraph = new PackedGraph(workflow, confSpecs)
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
    
    val workflow = GrammarParser.readWorkflow(acid + plan)
    val packedGraph = new PackedGraph(workflow, confSpecs)
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
    
    val workflow = GrammarParser.readWorkflow(acid + plan)
    val packedGraph = new PackedGraph(workflow, confSpecs)
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
    
    val workflow = GrammarParser.readWorkflow(acid + plan)
    val packedGraph = new PackedGraph(workflow, confSpecs)
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
    
    val workflow = GrammarParser.readWorkflow(acid + plan)
    val packedGraph = new PackedGraph(workflow, confSpecs)
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
    
    val workflow = GrammarParser.readWorkflow(acid + plan)
    val packedGraph = new PackedGraph(workflow, confSpecs)
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
    
    val workflow = GrammarParser.readWorkflow(acid + plan)
    val packedGraph = new PackedGraph(workflow, confSpecs)
    val goals = packedGraph.goals
    print(goals)
    
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

  
  
//  "The packed graph for acid.tape workflow with the complete plan" should {
//  
//    val plan = """
//plan {
//	reach evaluate_one,evaluate_all via (DataSet: *)     * 
//	                                    (TrainCorpus: *) * 
//	                                    (TestSplit: *)   * 
//	                                    (UseDict: *)     * 
//	                                    (OnlyOne: *)     * 
//	                                    (Foo: *)         * 
//	                                    (OptimizerSeed: *)
//}
//      """
//    
//    val workflow = GrammarParser.readWorkflow(acid + plan)
//    val packedGraph = new PackedGraph(workflow, confSpecs)
//    val goals = packedGraph.goals
//
//    print(goals)
//    
//    "contain goals" in {   
//      assertResult(101)(goals.size)
//    }
//
//  }    
  
  def test(goals:Goals, packedGraph:PackedGraph, tuples:Seq[(String,String)], taskName:String, expectedResult:Boolean): Unit = {
    
    val branches = Seq.newBuilder[Branch]
    tuples.foreach{ case (branchPointName, branchName) => 
      val branch = packedGraph.branchFactory(name=branchName, branchPoint=branchPointName)
      branches += branch
    }
    val sortedBranches = branches.result().sortWith{(a,b) => a.toString() < b.toString()}
    val realization = new Realization(sortedBranches)
      
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