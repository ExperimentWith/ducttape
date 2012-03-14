package ducttape.syntax
import ducttape.util.AbstractTest
import ducttape.syntax.GrammarParser.Parser
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BranchPointTest extends AbstractTest("branch point",Grammar.branchPoint) {
 
  def successCases = Set(
    """(branchPointName: a=1)""",
    """(branchPointName: a=1 b=5)""",   
    """(greeting: y="welcome home" z="bugger off")""",
    """(sauce: a1="A1 Sauce" ketchup="Tomato Ketchup" wasabi="wasabi")""",
    "(flags: a=\"\"\"-avze 'ssh -o \"SomeOption=Value\"\"\"\" b=\"kumbaya\" )",
    "(flags: a=\"\"\"-avze 'ssh -o \"SomeOption=Value\"\"\"\" b=\"kumbaya\")",
    "(flags: a=\"\"\"-avze 'ssh -o \"SomeOption=Value\"\"\"\" b=kumbaya)",

    // Bare rvalues
    """(branchPointName: 1)""",
    """(branchPointName: 1 5)""",    
    
    // Complex nesting
    "(a: a1=(k: 8..12) a4=7)",
    "(a: a1=$g@taskH[i:j])",
    "(a: a1=(b: f=$g@taskH[i:j]) a2=5 a3=(k: 8..12) a4=7)",      
    "(a: a1=(b: c=$d@taskE) a2=5 a3=(k: 8..12) a4=7)",      
    "(a: a1=(b: c=$d@taskE f=$g@taskH[i:j]) a2=5 a3=(k: 8..12) a4=7)",
    "(a: a1=(b: c=(x: x1=$d@taskE x2=farOut x3=\"\"\"Quoted!\"\"\") f=$g@taskH[i:j]) a2=5 a3=(k: 8..12) a4=7)",
    
    // Complex nesting with bare rvalues
    "(a: (k: 8..12) a4=7)",
    "(a: a1=(k: 8..12) 7)",
    "(a: (k: 8..12) 7)",
    "(a: (b: c=d) e=f)",
    "(a: (b: d d2=var d3 d4) e=f)",
    "(a: (b: c=(x: x1=$d@taskE x2=farOut x3=\"\"\"Quoted!\"\"\") f=$g@taskH[i:j]) a2=5 a3=(k: 8..12) a4=7)",
    "(a: b=(b: c=(x: d=$d@taskE farOut \"\"\"Quoted!\"\"\") $g@taskH[i:j]) 5 (k: 8..12) 7)",
    "(a: (b: (x: $d@taskE farOut \"\"\"Quoted!\"\"\") $g@taskH[i:j]) 5 (k: 8..12) 7)"

  ) 
  
  def failureCases = Set(
    "",
    " ",
    "NaN",
    "Infinity",
    "-Infinity",
    "+Infinity",
    "3.14e3.14",
    "A-variable_Name__",
    "A_variable_Name__",
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_",
    "!@#$%^&*()",
      "1",
      "-1",
      "+1",
      "3.141526535897923384626433832795028841971693993751058209",
      "123456789012345678901234567890123456789012345678901234567890",
      "123456789012345678901234567890123456789012345678901234567890e2147483647",
      "123456789012345678901234567890123456789012345678901234567890e-2147483647",
      "0",
      "0.00",
      "123",
      "-123",
      "1.23E3",
      "1.23E+3",
      "12.3E+7",
      "12.0",
      "12.3",
      "0.00123",
      "-1.23E-12",
      "1234.5E-4",
      "0E+7",
      "-0"     ,
    "10e-2147483648",
    "10e2147483648",
    "123456789012345678901234567890123456789012345678901234567890e123456789012345678901234567890123456789012345678901234567890"
      
  ) 
  
  def errorCases = Set(
    "(a: a1=g@taskH[i:j])"
//    """(branchPointName: .1..5)""",   
//    """(branchPointName: 1..-.05)""",
//    """(branchPointName: 1.0...5)""",
//    """(branchPointName: -.10e1..10e999)""",
//    """(branchPointName: .9e256..7.7e1024)"""      
  )
  
}