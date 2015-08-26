// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.syntax
import ducttape.util.AbstractTest
import ducttape.syntax.GrammarParser.Parser
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class OutputAssignmentTest extends AbstractTest("output assignment",Grammar.outputAssignment) {
 
  def successCases = Set(
      
    // Sequential branch point
    """a_1=(branchPointName: 1..5)""",   
    """bcd=(branchPointName: 1..5.0)""",
    """_QRS=(branchPointName: 1.0..5.0)""",
    """t165s_z=(branchPointName: 10e1..10e999)""",
    """x=(branchPointName: 9.9e256..7.7e1024)""",
    """y=(branchPointName: 1..6..2)""",   
    """z=(branchPointName: 1..5.0..0.5)""",
    """_1=(branchPointName: 1.0..5.0..2)""",
    """z_=(branchPointName: 10e1..10e999..1)""",
    """Qz124356797809708970897089780970897=(branchPointName: 9.9e256..7.7e1024..5.4e3)""",      
      
    // Branch graft
    "in=$variableName@taskName[branchPointName:branchName]",
    "var=$variableName@taskName[branchPointName:*]",
    "a=$variableName@taskName[a:b,c:d]",
    "b=$variableName@taskName[a1:b2,c3:*,d4:e5,f6:g7]",

    // Shorthand branch graft
    "in=@taskName[branchPointName:branchName]",
    "var=@taskName[branchPointName:*]",
    "a=@taskName[a:b,c:d]",
    "b=@taskName[a1:b2,c3:*,d4:e5,f6:g7]",
    
    // Task variable reference
    "c=$A_variable_Name__@foo",
    "dDt=$ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_@barBar",
    "e_1=$abc@def",
      
    // Variable reference
    "q_687_abt=$A_variable_Name__",
    "a1_sauce=$ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_",
    "life_42=$abc",

    // Shorthand task variable reference
    "c=@foo",
    "dDt=@barBar",
    "e_1=@def",
      
    // Shorthand variable reference
    "q_687_abt=@",
    "a1_sauce=@",
    "life_42=@",     
    
    // Unquoted literal
    "zip=A_variable_Name__",
    "a=ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_",
    "dee=/path/to/something/cool",
    "doo=abc",
    "dah=A_variable_Name__",
    "zip=ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_",
    "a=A-variable_Name__",
    "dee=A_variable_Name__",
    
    // Quoted literal
    """aye="This is a quoted string"""",
    """my='This one uses single quotes '""",
    """oh=' Escape\tsequences\nare\rallowed! '""",
    "my=\"Unicode sequences should be fine \u21AF too\"",
    "what=\'Unicode sequences should be fine \u2231 too\'",
    
    // Unbound
    "a",
    "b_1",
    "_z"
    
  ) 
  
  def failureCases = Set(
    ""  ,
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_ :",
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_     :",
    " ",   
    """"This is a badly quoted string\"""",
    """"This one is, too"it seems"""",
    """'Starting with a single and ending with a double"""",
    """"Starting with a double and ending with a single'""",    
    "$variableName@taskName[",
    "$A-variable_Name__",
    "$",
 

// Variables with dots
    
    // Sequential branch point
    """.a_1=(branchPointName: 1..5)""",   
    """.bcd=(branchPointName: 1..5.0)""",
    """._QRS=(branchPointName: 1.0..5.0)""",
    """.t165s_z=(branchPointName: 10e1..10e999)""",
    """.x=(branchPointName: 9.9e256..7.7e1024)""",
    """.y=(branchPointName: 1..6..2)""",   
    """.z=(branchPointName: 1..5.0..0.5)""",
    """._1=(branchPointName: 1.0..5.0..2)""",
    """.z_=(branchPointName: 10e1..10e999..1)""",
    """.Qz124356797809708970897089780970897=(branchPointName: 9.9e256..7.7e1024..5.4e3)""",      
      
    // Branch graft
    ".in=$variableName@taskName[branchPointName:branchName]",
    ".var=$variableName@taskName[branchPointName:*]",
    ".a=$variableName@taskName[a:b,c:d]",
    ".b=$variableName@taskName[a1:b2,c3:*,d4:e5,f6:g7]",
      
    // Task variable reference
    ".c=$A_variable_Name__@foo",
    ".dDt=$ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_@barBar",
    ".e_1=$abc@def",
      
    // Variable reference
    ".q_687_abt=$A_variable_Name__",
    ".a1_sauce=$ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_",
    ".life_42=$abc",
    
    // Unquoted literal
    ".zip=A_variable_Name__",
    ".a=ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_",
    ".dee=/path/to/something/cool",
    ".doo=abc",
    ".dah=A_variable_Name__:",
    ".zip=ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_:",
    ".a=A-variable_Name__",
    ".dee=A_variable_Name__",
    
    // Quoted literal
    """.aye="This is a quoted string"""",
    """.my='This one uses single quotes '""",
    """.oh=' Escape\tsequences\nare\rallowed! '""",
    ".my=\"Unicode sequences should be fine \u21AF too\"",
    ".what=\'Unicode sequences should be fine \u2231 too\'"     
  ) 
  
  def errorCases = Set(
    "dah=A_variable_Name__:",
    "zip=ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_:"
  )
  
}