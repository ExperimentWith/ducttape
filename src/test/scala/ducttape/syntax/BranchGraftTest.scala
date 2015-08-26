// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.syntax
import ducttape.util.AbstractTest
import ducttape.syntax.GrammarParser.Parser
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BranchGraftTest extends AbstractTest("branch graft",Grammar.branchGraft) {
 
  def successCases = Set(
      "$variableName@taskName[branchPointName:branchName]",
      "$variableName@taskName[branchPointName:*]",
      "$variableName@taskName[a:b,c:d]",
      "$variableName@taskName[a1:b2,c3:*,d4:e5,f6:g7]",
      "$variableName@taskName[a1:1]",
      "$variableName@taskName[a1:-1]",
      "$variableName@taskName[a1:1.5]",
      "$variableName@taskName[a1:-1.5]",
      "$variableName@taskName[a1:1,c3:*,d4:-7.75,f6:g7]",
      
      "${variableName}@taskName[branchPointName:branchName]",
      "${variableName}@taskName[branchPointName:*]",
      "${variableName}@taskName[a:b,c:d]",
      "${variableName}@taskName[a1:b2,c3:*,d4:e5,f6:g7]",
      
      "$variableName[branchPointName:branchName]",
      "$variableName[branchPointName:*]",
      "$variableName[a:b,c:d]",
      "$variableName[a1:b2,c3:*,d4:e5,f6:g7]",
      
      "${variableName}[branchPointName:branchName]",
      "${variableName}[branchPointName:*]",
      "${variableName}[a:b,c:d]",
      "${variableName}[a1:b2,c3:*,d4:e5,f6:g7]"
  ) 
  
  def failureCases = Set(
    "",
    " ",
    "var@taskName[a1:b2]",
    "$A_variable_Name__",
    "$ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_",
    "$abc",
    "abc",
    "A_variable_Name__:",
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_:",
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_ :",
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_     :",
    "A-variable_Name__",
    "A_variable_Name__",
    """"This is a quoted string"""",
    """'This one uses single quotes '""",
    """' Escape\tsequences\nare\rallowed! '""",
    "\"Unicode sequences should be fine \u21AF too\"",
    "\'Unicode sequences should be fine \u2231 too\'",
    "A_variable_Name__",
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_",
    "/path/to/something/cool",
    """"This is a badly quoted string\"""",
    """"This one is, too"it seems"""",
    """'Starting with a single and ending with a double"""",
    """"Starting with a double and ending with a single'""" ,
    "$A-variable_Name__"
  ) 
  
  def errorCases = Set(  
    "$variableName@taskName[",      
    
    "$ "    
  )
  
}