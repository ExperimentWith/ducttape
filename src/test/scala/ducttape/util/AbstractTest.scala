// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.util

import org.scalatest.WordSpec
import scala.util.parsing.combinator.Parsers
import scala.collection.Set
import ducttape.syntax.GrammarParser
import ducttape.syntax.GrammarParser.ParseResult
import ducttape.syntax.GrammarParser.Parser
import ducttape.syntax.Grammar._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

abstract class AbstractTest(val description:String, val parser:Parser[Any]) extends WordSpec {

  def successCases : Set[String]
  
  def failureCases : Set[String]
  
  def errorCases : Set[String]
  
  def exceptionCases : Set[String] = Set()
  
  def quote(string:String) : String = {
    return string.replaceAll("\n","""\n""")
  }
  
  "Parsing "+description should {
    
    for (value <- successCases) {   
      "succeed for \""+quote(value)+"\"" in {
        val result: ParseResult[Any] = GrammarParser.parseAll(parser, value);
        Tests.verify(this,result)
      }
    }

    for (value <- failureCases) {   
      "fail for \""+quote(value)+"\"" in {
        val result: ParseResult[Any] = GrammarParser.parseAll(parser, value);
        Tests.verifyFailure(this,result)
      }
    }       
    
    for (value <- errorCases) {   
      "error for \""+quote(value)+"\"" in {
//        try {
          val result: ParseResult[Any] = GrammarParser.parseAll(parser, value);
          Tests.verifyError(this,result)
//        } catch {
//         case e:Exception => ()
//        }
      }
    }    
    
        for (value <- exceptionCases) {   
      "error for \""+quote(value)+"\"" in {
        try {
          val result: ParseResult[Any] = GrammarParser.parseAll(parser, value);
          Tests.verifyError(this,result)
        } catch {
         case e:Exception => ()
        }
      }
    }  
  }
  
}
