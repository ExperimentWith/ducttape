// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

import System._
import collection._
import sys.ShutdownHookThread

import java.io.File
import java.util.concurrent.ExecutionException
import java.util.regex.Pattern

import ducttape.cli.Config
import ducttape.cli.Directives
import ducttape.cli.ErrorUtils
import ducttape.cli.ErrorUtils.ex2err
import ducttape.cli.Opts
//import ducttape.cli.EnvironmentMode
//import ducttape.cli.Plans
import ducttape.cli.RealizationGlob
import ducttape.cli.ExecuteMode
import ducttape.db.WorkflowDatabase
import ducttape.db.TaskInfo
import ducttape.db.PackageInfo
import ducttape.db.InputInfo
import ducttape.db.OutputInfo
import ducttape.db.ParamInfo
import ducttape.exec.CompletionChecker
import ducttape.exec.Executor
import ducttape.exec.InputChecker
import ducttape.exec.PidWriter
import ducttape.exec.PackageBuilder
import ducttape.exec.PackageFinder
import ducttape.exec.Submitter
import ducttape.exec.TaskEnvironment
//import ducttape.exec.UnpackedDagVisitor
import ducttape.exec.DirectoryArchitect
import ducttape.exec.PackageVersioner
import ducttape.exec.PackageStatus
import ducttape.exec.FullTaskEnvironment
//import ducttape.exec.PartialOutputMover
import ducttape.exec.ForceUnlocker
//import ducttape.hyperdag.walker.Traversal
//import ducttape.hyperdag.walker.Arbitrary
//import ducttape.hyperdag.walker.BreadthFirst
//import ducttape.hyperdag.walker.DepthFirst
import ducttape.graph.PackedGraph
import ducttape.graph.UnpackedGraph
import ducttape.graph.UnpackedGraph.Task
import ducttape.graph.traversal.UnpackedGraphWalker
import ducttape.graph.traversal.Traversal
import ducttape.graph.traversal.Arbitrary
import ducttape.graph.traversal.BreadthFirst
import ducttape.graph.traversal.DepthFirst
import ducttape.graph.traversal.Visitor
import ducttape.syntax.AbstractSyntaxTree._
import ducttape.syntax.GrammarParser
import ducttape.syntax.StaticChecker
import ducttape.syntax.Namespace
import ducttape.syntax.ErrorBehavior
import ducttape.syntax.ErrorBehavior._
//import ducttape.workflow.builder.WorkflowBuilder
//import ducttape.workflow.HyperWorkflow
import ducttape.workflow.Realization
//import ducttape.workflow.TaskTemplate
//import ducttape.workflow.RealTask
//import ducttape.workflow.VersionedTask
//import ducttape.workflow.VersionedTaskId
import ducttape.versioner.VersionedPackageId
import ducttape.workflow.BranchPoint
import ducttape.workflow.Branch
//import ducttape.workflow.RealizationPlan
//import ducttape.workflow.PlanPolicy
//import ducttape.workflow.VertexFilter
//import ducttape.workflow.Types._
import ducttape.workflow.BuiltInLoader
//import ducttape.workflow.VertexFilter
//import ducttape.workflow.Visitors
//import ducttape.versioner.WorkflowVersionInfo
//import ducttape.versioner.FakeWorkflowVersionInfo
//import ducttape.versioner.TentativeWorkflowVersionInfo
//import ducttape.versioner.WorkflowVersionHistory
import ducttape.syntax.FileFormatException
import ducttape.syntax.WorkflowChecker
import ducttape.util.Files
import ducttape.util.OrderedSet
import ducttape.util.MutableOrderedSet
import ducttape.util.Environment
import ducttape.util.LogUtils
import ducttape.util.DucttapeException
import ducttape.util.BashException
import ducttape.util.Globs
import ducttape.util.Shell

import grizzled.slf4j.Logging
import scala.io.StdIn

class Ducttape(val opts:Opts) extends Logging {

  lazy val userConfig: WorkflowDefinition = {
    val userConfigFile = new File(Environment.UserHomeDir, ".ducttape")
    debug(s"Checking for user config at: ${userConfigFile.getAbsolutePath}")
    val userConfig: WorkflowDefinition = if (userConfigFile.exists) {
      GrammarParser.readConfig(userConfigFile)
    } else {
      new WorkflowDefinition(elements=Nil, files=Nil)
    }
    userConfig
  }
  
  lazy val workflow: WorkflowDefinition = {
    // read user config before printing anything to screen
//    val userConfig = getUserConfig()
    val builtins: Seq[WorkflowDefinition] = BuiltInLoader.load(DirectoryArchitect.builtinsDir)
    
    // make these messages optional with verbosity levels?
    debug(s"Reading workflow from ${opts.workflowFile.getAbsolutePath}")
    val wd: WorkflowDefinition = {
      val workflowOnly = ex2err(GrammarParser.readWorkflow(opts.workflowFile))
      
      val confStuff: WorkflowDefinition = ex2err(opts.config_file.value match {
        case Some(confFile) => {
          // TODO: Make sure workflow doesn't have anonymous conf?
          workflowOnly.anonymousConfig match {
            case Some(c) => throw new FileFormatException("Workflow cannot define anonymous config block if config file is used", c)
            case None => ;
          }
          
          err.println(s"Reading workflow configuration: ${confFile}")
          GrammarParser.readConfig(new File(confFile))
        }
        case None => new WorkflowDefinition(elements=Nil, files=Nil)
      })
      
      // Concatenate all definitions together into a single WorkflowDefinition object
      val externalDefinitions = builtins ++ Seq(userConfig) 
      val allDefinitions = externalDefinitions.foldLeft(workflowOnly){ (workflow, externalDefinition) => workflow ++ externalDefinition }
      
      allDefinitions
    }
    
    // Error-check workflow
    Ducttape.errorCheckAST(wd)
    
    wd // Use wd as the value for this.workflow
  }  
  
  
  
  val directoryArchitect: DirectoryArchitect = {
      val workflowBaseDir: File = {
        opts.output.value match {
          // output directory was specified on the command line: use that first
          case Some(outputDir) => new File(outputDir)
          case None => workflow.directives.output match { // ducttape_output=...
            case Some(value) => new File(value)
            // if unspecified, use PWD as the output directory
            case None => Environment.PWD
          }
        }
      }
      
      new DirectoryArchitect(workflow.directives.flat, opts.config_file.value, workflowBaseDir)    
  }
  
  val traversal: Traversal = opts.traversal.getOrElse("DepthFirst").toLowerCase.charAt(0) match {
      case 'a' => Arbitrary
      case 'b' => BreadthFirst
      case 'd' => DepthFirst
      case str @ _ => throw new RuntimeException(s"ERROR: Unknown traversal type: '${str}'")
    }
  
  val packedGraph = new PackedGraph(this.workflow)
  
	val goals = packedGraph.goals
	
	val unpackedGraph = {
    val graph = UnpackedGraph.unpack(this.goals)
    Ducttape.errorCheckUnpackedWorkflow(graph, this.traversal)
    graph
  }
    

  

  def getCompletedTasks(verbose: Boolean = false): CompletionChecker = {

		System.err.println("Checking for completed tasks")
		 
		def incompleteCallback(task: Task, msg: String) {
			import ducttape.cli.ColorUtils.colorizeDir
			if (verbose) {
				System.err.println(s"Task incomplete: ${colorizeDir(task.name, task.realization, directoryArchitect)}: ${msg}")
			}
		}

		val cc = new CompletionChecker(directoryArchitect, incompleteCallback)
			
		// use the user's traversaal type here so that the confirmation prompt has tasks in the right order
		Visitor.visitAll(unpackedGraph, cc, traversal=traversal)
		return cc
  }
  


  /**
   * Get what packages are needed by the planned vertices and
   * what repo version of each of those we have built already (if any)
   */
  def getPackageVersions(cc: Option[CompletionChecker], verbose: Boolean = false): PackageVersioner = {

    // find what packages are required by the planned vertices
		// TODO: Always return all packages when using auto_update?
		val packageMap = workflow.packages.map{packageDef => packageDef.name.toString() -> packageDef }.toMap
		val packageFinder = new PackageFinder(cc.map(_.todo), packageMap)

		Visitor.visitAll(unpackedGraph, packageFinder, traversal=traversal)
		System.err.println(s"Found ${packageFinder.packages.size} packages")

		// now see what the repo version is for these packages
		// and also determine if they need to be rebuilt in execute mode
		err.println("Checking for already built packages (if this takes a long time, consider switching to a local-disk git clone instead of a remote repository)...")
		val packageVersions = new PackageVersioner(directoryArchitect, workflow.versioners, verbose, directives=workflow.directives)
		packageVersions.findAlreadyBuilt(packageFinder.packages.toSeq)
		
		return packageVersions
  }

  
  def list():Unit = {
	  for (task <- new UnpackedGraphWalker(unpackedGraph, traversal)) {
		  println(s"${task.name} ${task.realization}")
    }
  }

  
  def markDone():Unit = {
	
	  if (opts.taskName == None) {
      opts.exitHelp("mark_done requires a taskName", 1)
	  }
	  if (opts.realNames.size < 1) {
		  opts.exitHelp("mark_done requires realization names", 1)
	  }

	  val taskPattern: Pattern = Pattern.compile(Globs.globToRegex(opts.taskName.get))
		val realPatterns: Seq[RealizationGlob] = opts.realNames.toSeq.map(new RealizationGlob(_))

		for (task <- new UnpackedGraphWalker(unpackedGraph, traversal)) {

			if (taskPattern.matcher(task.name.toString).matches) {

				if (realPatterns.exists(_.matches(task.realization))) {
					val env = new TaskEnvironment(directoryArchitect, task)
					if (CompletionChecker.isComplete(env)) {
						err.println(s"Task already complete: ${task.name}/${task.realization}")
					} else {
						try {
							CompletionChecker.forceCompletion(env)
							err.println("Forced completion of task: " + task)
						} catch {
						case e: Exception => System.err.println("WARNING: Failed to force completion of " + task)
						}
					}
				}
			}
		}
  }

  
  def viz():Unit = {
	  import ducttape.viz._
	  opts.typeFlag.value match {
	    case Some("packed") => {
	    	err.println("Generating GraphViz dot visualization of packed workflow...")
	    	println(packedGraph.toString())
	    }
      case Some("unpacked") | None => {
    	  err.println("Generating GraphViz dot visualization of unpacked workflow...")
    	  println(unpackedGraph.toString())
      }
      case _ => {
    	  throw new RuntimeException(s"Unknown visualization type: ${opts.typeFlag.value}")
      }
	  }
  }


  // supports '*' as a task or realization
  // or globs containing interlaced '*' and '?' as tasks or realizations
  def getVictims(taskToKill: String, realsToKill: Set[String]): OrderedSet[Task] = {

	  val taskPattern: Pattern = Pattern.compile(Globs.globToRegex(taskToKill))
		val realPatterns: Seq[RealizationGlob] = realsToKill.toSeq.map(new RealizationGlob(_))
      
		// TODO: Store namespace instead
		val victims = new mutable.HashSet[(String,Realization)]
		val victimList = new MutableOrderedSet[Task]
	  for (task <- new UnpackedGraphWalker(unpackedGraph, traversal)) {
		  if (taskPattern.matcher(task.name).matches) {
			  if (realPatterns.exists(_.matches(task.realization))) {
				  // TODO: Store seqs instead?
				  victims += ((task.name, task.realization))
					victimList += task
        }
		  } else {
			  // was this task invalidated by its parent?
			  // TODO: Can we propagate this in a more natural way
			  val isVictim = task.directDependencies.temporal.exists { parentTask =>
			  val parent = (parentTask.name, parentTask.realization)
			  victims(parent)
			}
			if (isVictim) {
				victims += ((task.name, task.realization))
				victimList += task
			}
		  }
	  }
	  //  TODO: Fix OrderedSet with a companion object so that we can use filter
	  val extantVictims = new MutableOrderedSet[Task]
		for (task <- victimList) {
			val taskEnv = new TaskEnvironment(directoryArchitect, task)
			if (taskEnv.where.exists) {
				extantVictims += task
			} else {
				err.println(s"No previous output for: ${task}")
			}
		}
	  return extantVictims
  }


  // TODO: Don't apply plan filtering to invalidation? More generally, we should let the user choose baseline-only, baseline-one-offs, cross product, or plan
  def invalidate():Unit = {
    if (opts.taskName == None) {
      opts.exitHelp("invalidate requires a taskName", 1)
    }
    if (opts.realNames.size < 1) {
      opts.exitHelp("invalidate requires realization names", 1)
    }
    val taskToKill = opts.taskName.get
    val realsToKill = opts.realNames.toSet
      
    // 1) Accumulate the set of changes
    err.println(s"Finding tasks to be invalidated: ${taskToKill} for realizations: ${realsToKill}")
    val victims: OrderedSet[Task] = getVictims(taskToKill, realsToKill)
    val victimList: Seq[Task] = victims.toSeq
      
    // 2) prompt the user
    import ducttape.cli.ColorUtils.colorizeDirs
    err.println("About to mark all the following directories as invalid so that a new version will be re-run for them:")
    err.println(colorizeDirs(victimList, directoryArchitect).mkString("\n"))
      
    val answer = if (opts.yes) {
    	'y'
    } else {
    	// note: user must still press enter
    	err.print("Are you sure you want to invalidate all these? [y/n] ")
    	StdIn.readChar
    }
      
    answer match {
      case 'y' | 'Y' => victims.foreach(task => {
    	  err.println(s"Invalidating ${task}")
    	  CompletionChecker.invalidate(new TaskEnvironment(directoryArchitect, task))
      })
      case _ => err.println("Doing nothing")
    }
  }

    
  def purge(): Unit = {
	  if (opts.taskName == None) {
		  opts.exitHelp("purge requires a taskName", 1)
	  }
	  if (opts.realNames.size < 1) {
		  opts.exitHelp("purge requires realization names", 1)
	  }
	  val taskToKill = opts.taskName.get
		val realsToKill = opts.realNames.toSet
      
		// 1) Accumulate the set of changes
		err.println(s"Finding tasks to be purged: ${taskToKill} for realizations: ${realsToKill}")
		val victimList: Seq[Task] = getVictims(taskToKill, realsToKill).toSeq
      
		// 2) prompt the user
		import ducttape.cli.ColorUtils.colorizeDirs
		err.println("About to permenantly delete the following directories:")
		val absDirs: Seq[File] = victimList.map { task: Task => directoryArchitect.assignDir(task) }
		err.println(colorizeDirs(victimList, directoryArchitect).mkString("\n"))
      
		val answer = if (opts.yes) {
			'y'
		} else {
			// note: user must still press enter
			err.print("Are you sure you want to delete all these? [y/n] ")
			StdIn.readChar
		}

		answer match {
		  case 'y' | 'Y' => absDirs.foreach { f: File =>
		    err.println(s"Deleting ${f.getAbsolutePath}")
		    Files.deleteDir(f)
		  }
		  case _ => err.println("Doing nothing")
		}
  }

  def summarize(): Unit = {

		// 1) find which summary the user asked for (else use them all)
		//    -- just use them all for now
    //    -- we should also allow the user to specify which "plans" they would like us to enumerate
    //    -- we should also allow ther user to specify task globs on the command line

    // TODO: More static checking of summaries to disallow params and inputs (and even packages for now)

    // TODO: As a static pre-processing step, we should make sure summaries are referring to existing tasks...
    //wd.tasks.find { _.name == taskName } match {
    //  case None => throw new FileFormatException("Task '%s' not found. Required by summary '%s'".format(taskName, summary.name), summaryDef)
    //  case Some(taskDef) => {
    //  }
    //}
        
    // TODO: Support more than just the most recent version via a command line option
    val cc = getCompletedTasks()
    // we need packageVersions in case user wants to use current tool to perform the summarizing
    val packageVersions = getPackageVersions(None)
        
    // 2) Run each summary block and store in a big table

    //    we also keep track of a) all branch points we've seen and produce one column in our output table for each
    //    and b) each branch seen for each of those branch points -- branch points having only a single branch will be omitted
    //    to make the control variables of each experiment more obvious
    val branchPointMap = new mutable.HashMap[BranchPoint, mutable.HashSet[Branch]]
    val labelSet = new mutable.HashSet[String]

    // we enforce that only one task may write to each row (realization) for each column below
    val results = new mutable.HashMap[Realization, mutable.HashMap[String,String]]
    // TODO: Move this code to its own SummaryMode file
    for (task <- new UnpackedGraphWalker(unpackedGraph, traversal)) {

    	for (summaryDef: SummaryDef <- workflow.summaries) {
    		// TODO: Check for namespace issues
    		for (ofDef: SummaryTaskDef <- summaryDef.ofs; if (ofDef.name.name == task.name)) {
    			val isComplete = cc.completed( (task.name, task.realization) )
    		  if (isComplete) {
    			  val taskEnv = new FullTaskEnvironment(directoryArchitect, packageVersions, task)
    				val workDir = directoryArchitect.getTempActionDir("summary")
    				Files.mkdirs(workDir)
                
    				val summaryOutputs: Seq[(String,String)] = ofDef.outputs.map { spec: Spec =>
    				  (spec.name, new File(workDir, spec.name).getAbsolutePath)
    				}
                
    				// TODO: Use all the same variables as a submitter?
    				val env: Seq[(String,String)] = Seq( ("TASK_DIR", taskEnv.where.getAbsolutePath) ) ++ summaryOutputs ++ taskEnv.env
                
    				// f) run the summary command
    				val code = ofDef.commands.toString
            val stdPrefix = taskEnv.task.toString
            val stdoutFile = new File(workDir, "summary_stdout.txt")
            val stderrFile = new File(workDir, "summary_stderr.txt")
            val exitCodeFile = new File(workDir, "summary_exit_code.txt")
                
            err.println(s"Summarizing ${summaryDef.name}: ${task}")
            val exitCode = Shell.run(code, stdPrefix, workDir, env, stdoutFile, stderrFile)
            Files.write(s"${exitCode}", exitCodeFile)
            if (exitCode != 0) {
            	throw new BashException(s"Summary '${summaryDef.name}' of ${taskEnv.task} failed")
            }
                
    				// g) extract the result from the file and put it in our table
    				for ( (outputName, path) <- summaryOutputs) {
    					val lines: Seq[String] = Files.read(new File(path))
    					if (lines.size != 1) {
    						throw new BashException(s"For summary '${summaryDef.name}', expected exactly one line in '${path}', but instead found ${lines.size}")
    					}
    				  val label = outputName
    					val result: String = lines(0)
    					val row = results.getOrElseUpdate(task.realization, new mutable.HashMap[String,String] )
    					if (row.get(label) != None) {
    						throw new RuntimeException(s"Multiple tasks are attempting to write to the column '${label}' for the realization ${task.realization.toFullString(hashLongNames=false)}")
    					}
    				  row += label -> result
    					for (branch <- task.realization.branches) {
    						branchPointMap.getOrElseUpdate(branch.branchPoint, new mutable.HashSet) += branch
    					}
    				  labelSet += label
    				}
                
    				// h) cleanup
    				Files.deleteDir(workDir)
    		  }
    		}
    	}
    }

    // TODO: Command line option to prevent comments from being written to stdout

    // 3) print the table out in a nice tab-delimited format
    // first line is header
    val allBranchPoints = branchPointMap.keys.toSeq.sortBy(_.name)
    val (constantBranchPointMap, variableBranchPointMap) = branchPointMap.toSeq.sortBy(_._1.name).partition { case (bp, branches) => branches.size == 1 }
    for ( (bp, branches) <- constantBranchPointMap) {
    	System.out.println(s"# Constant branch point: ${bp}=${branches.head}")
    }
    val variableBranchPoints: Seq[BranchPoint] = variableBranchPointMap.map(_._1)
    System.out.println(s"# Variable branch points: ${variableBranchPoints.mkString(" ")}")

    val labels: Seq[String] = labelSet.toSeq.sorted
    val header: Seq[String] = variableBranchPoints.map(_.name) ++ labels
    System.out.println(header.mkString("\t"))
    for ( (real, values) <- results) {
    	val branches: Map[BranchPoint, String] = real.branches.map { branch => (branch.branchPoint, branch.name) }.toMap
      val cols: Seq[String] = variableBranchPoints.map { bp => branches.getOrElse(bp, "--") } ++
      labels.map { label => values.getOrElse(label, "--") }
      System.out.println(cols.mkString("\t"))
    }
      
  }
  
  
  def unlock():Unit = {

		if (opts.taskName == None) {
			opts.exitHelp("unlock requires a taskName (or singled-quoted glob)", 1)
		}
		if (opts.realNames.size < 1) {
			opts.exitHelp("unlock requires realization names (or singled-quoted glob)", 1)
		}

		val taskToUnlock = opts.taskName.get
	  val realsToUnlock = opts.realNames.toSet
		val taskPattern: Pattern = Pattern.compile(Globs.globToRegex(taskToUnlock))
		val realPatterns: Seq[RealizationGlob] = realsToUnlock.toSeq.map(new RealizationGlob(_))

		// first, find all locked tasks
		val victims: Seq[(String,Realization)] = {
		  err.println("Finding all locked tasks in this plan")

		  val cc = getCompletedTasks()	
		  val locked: Seq[(String,Realization)] = cc.locked.toSeq

		  // then, filter down based on the task/real pattern
		  err.println("Filtering based on task/realization pattern")
		
		  val victims: Seq[(String,Realization)] = locked.filter { case (task: String, real: Realization) =>
		    taskPattern.matcher(task).matches && realPatterns.exists(_.matches(real))
		  }
		  
		  victims
		}

		// is there anything to do?
		if (victims.size == 0) {
			System.err.println("No tasks in this plan that match %s / %s are currently locked -- nothing to do".format(taskToUnlock, realsToUnlock.mkString(" ")))
		} else {
			// 2) Prompt user
			import ducttape.cli.ColorUtils.colorizeDir
			System.err.println("Remove locks:")
			for ( (task, real) <- victims) {
				System.err.println(s"${Config.greenColor}UNLOCK:${Config.resetColor} ${colorizeDir(task, real, directoryArchitect)}")
			}

			val answer = if (opts.yes) {
				true
			} else {
				// note: user must still press enter
				if (victims.size > 0) {
					System.err.print(s"Are you sure you want to FORCE UNLOCK these ${victims.size} tasks? (Only do this if you sure no other process is using them) [y/n] ")
					StdIn.readBoolean
				} else {
					false
				}
			}

			answer match {
  			case true => Visitor.visitAll(unpackedGraph, new ForceUnlocker(directoryArchitect, todo=victims.toSet), traversal=traversal)
	  		case _ => System.err.println("Doing nothing")
			}
		}

  }

  
  def update(): Unit = {

	  // update *all* packages in the current workflow atomically
		val packages: Seq[PackageDef] = workflow.packages //workflow.packageDefs.values.toSet.toSeq
		val versioners = workflow.versioners
		System.err.println(s"Found ${packages.size} packages")
		err.println("Checking for new versions of packages...")
		val packageVersions = new PackageVersioner(directoryArchitect, versioners, forceUpdate=true, directives=workflow.directives)
		packageVersions.findAlreadyBuilt(packages)

		for (packageName <- packageVersions.packagesToBuild) {
			System.err.println(s"${Config.greenColor}BUILD:${Config.resetColor} ${packageName}")
		}

    if (packageVersions.packagesToBuild.size > 0) {
	    val answer = if (opts.yes) {
		    true
	    } else {
		    // note: user must still press enter
		    System.err.print(s"Are you sure you want to build these ${packageVersions.packagesToBuild.size} packages? [y/n] ")
		    System.err.flush()
		    StdIn.readBoolean
	    }

	    if (answer) {
	    	System.err.println("Retrieving code and building...")
	    	val builder = new PackageBuilder(directoryArchitect, packageVersions)
	    	builder.build(packageVersions.packagesToBuild)
	    } else {
	    	System.err.println("Doing nothing.")
	    }
    }

  }
  
  
  def runWorkflow(): Unit = {

		// The completion checker will start tentatively handing out our new workflow version
		// in preparation for creating the uncommitted version
		val cc: CompletionChecker = getCompletedTasks(verbose=true)

		val packagesThunk = () => getPackageVersions(Some(cc))
		val submitter = new Submitter(workflow.submitters)
		ExecuteMode.run(unpackedGraph, cc, submitter, packagesThunk, traversal, opts, directoryArchitect, workflow.directives)
      
  }
  
}

object Ducttape extends Logging {
  
  lazy val welcomeMessage: String = {
    val ducttapeVersion: String = try { 
      Files.readJarResource("/version.info").head
    } catch {
      case _: Throwable => "(version unknown)"
    }
    s"ducttape $ducttapeVersion\nby Jonathan Clark and Lane Schwartz"    
  }
  
  def initializeConsoleColors(opts:Opts): Unit = {
    if (opts.no_color || !Environment.hasTTY) {
      Config.clearColors()
    }

    ShutdownHookThread { // make sure we never leave the color in a bad state on exit
      println(Config.resetColor)
      System.err.println(Config.resetColor)
    }
  }

  /** pass 1 error checking: directly use workflow AST */
  def errorCheckAST(workflow:WorkflowDefinition): Unit = {
      // TODO: Once undeclared/unused variable works better, change this back from Ignore to Warn
      val undeclaredBehavior = ErrorBehavior.parse(workflow.directives.undeclared_vars, default=Ignore)
      val unusedBehavior = ErrorBehavior.parse(workflow.directives.unused_vars, default=Ignore)

      val (warnings, errors) = {
        val bashChecker = new StaticChecker(undeclaredBehavior, unusedBehavior)
        val (warnings1, errors1) = bashChecker.check(workflow)
        
        val workflowChecker = new WorkflowChecker(workflow)
        val (warnings2, errors2) = workflowChecker.check()
        (warnings1 ++ warnings2, errors1 ++ errors2)
      }
      for (e: FileFormatException <- warnings) {
        ErrorUtils.prettyPrintError(e, prefix="WARNING", color=Config.warnColor)
      }
      for (e: FileFormatException <- errors) {
        ErrorUtils.prettyPrintError(e, prefix="ERROR", color=Config.errorColor)
      }
      if (warnings.size > 0) System.err.println(s"${warnings.size} warnings")
      if (errors.size > 0) System.err.println(s"${errors.size} errors")
      if (errors.size > 0) {
        exit(1)
      }
  }

  /** pass 2 error checking: use unpacked workflow */
  def errorCheckUnpackedWorkflow(unpackedGraph:UnpackedGraph, traversal:Traversal) {
    val wd = unpackedGraph.goals.packedGraph.workflow
    val workflowChecker = new WorkflowChecker(wd)
    val (warnings, errors) = workflowChecker.checkUnpacked(unpackedGraph, traversal, wd.submitters)
    for (e: FileFormatException <- warnings) {
      ErrorUtils.prettyPrintError(e, prefix="WARNING", color=Config.warnColor)
    }
    for (e: FileFormatException <- errors) {
      ErrorUtils.prettyPrintError(e, prefix="ERROR", color=Config.errorColor)
    }
    if (warnings.size > 0) System.err.println(s"${warnings.size} warnings")
    if (errors.size > 0) System.err.println(s"${errors.size} errors")
    if (errors.size > 0) {
      exit(1)
    }
  }
  
  def main(args: Array[String]) {
    LogUtils.initJavaLogging()

    val opts = new Opts(args)
    initializeConsoleColors(opts)
    
    err.println(welcomeMessage)
    val ducttape = new Ducttape(opts)
        
    if (ducttape.workflow.directives.flat) System.err.println("Using structure: flat")

    ex2err(opts.mode match {
      case "list"       => ducttape.list()
      case "mark_done"  => ducttape.markDone()
      case "viz"        => ducttape.viz()
      case "invalidate" => ducttape.invalidate()
      case "purge"      => ducttape.purge()
      case "summary"    => ducttape.summarize()
      case "unlock"     => ducttape.unlock()
      case "update"     => ducttape.update()
      case "exec" | _   => ducttape.runWorkflow()
    })
  }
  
}
