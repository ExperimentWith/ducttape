// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.syntax
import ducttape.util.AbstractTest
import ducttape.syntax.GrammarParser.Parser
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class SequentialBranchPointTest extends AbstractTest("sequential branch point",Grammar.sequentialBranchPoint) {
 
  def successCases = Set(
      """(branchPointName: 1..5)""",   
      """(branchPointName: 1..5.0)""",
      """(branchPointName: 1.0..5.0)""",
      """(branchPointName: 10e1..10e999)""",
      """(branchPointName: 9.9e256..7.7e1024)""",
      """(branchPointName: 1..6..2)""",   
      """(branchPointName: 1..5.0..0.5)""",
      """(branchPointName: 1.0..5.0..2)""",
      """(branchPointName: 10e1..10e999..1)""",
      """(branchPointName: 9.9e256..7.7e1024..5.4e3)""",
      
      // Anonymous sequential branch point
      """(1..5)""",   
      """(1..5.0)""",
      """(1.0..5.0)""",
      """(10e1..10e999)""",
      """(9.9e256..7.7e1024)""",
      """(1..6..2)""",   
      """(1..5.0..0.5)""",
      """(1.0..5.0..2)""",
      """(10e1..10e999..1)""",
      """(9.9e256..7.7e1024..5.4e3)"""      
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
      "-0"     
  ) 
  
  def errorCases = Set(
    """(branchPointName: .1..5)""",   
    """(branchPointName: 1..-.05)""",
    """(branchPointName: 1.0...5)""",
    """(branchPointName: -.10e1..10e999)""",
    """(branchPointName: .9e256..7.7e1024)"""     
  )
  
  override def exceptionCases = Set(
    """(branchPointName: 1..10e-2147483648""",
    """(branchPointName: 1..10e2147483648""",
    """(branchPointName: 1..123456789012345678901234567890123456789012345678901234567890e123456789012345678901234567890123456789012345678901234567890"""
      
  )
  
}