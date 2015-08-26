// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.syntax
import ducttape.util.AbstractTest
import ducttape.syntax.GrammarParser.Parser
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TaskBlockTest extends AbstractTest("task header",Grammar.taskBlock) {
 
  def successCases = Set(
"""task hello {
      echo "hello, world!"
}""",
"""// Hello, world
task hello {
      echo "hello, world!"
}""",
""" // Hello, world
task hello {
      echo "hello, world!"
}""",
""" // Hello, world
// some more
task hello {
      echo "hello, world!"
}""",
"""task hello 
{
      echo "hello, world!"
}""",
"""task funky < in=foo > out {
  function die () {
    echo "$@" 1>&2 > some.txt
    exit 1
  }
  
  # Now do it!
  die()
}""",
"""task funky < in=foo > out
{
  function die () {
    echo "$@" 1>&2 > some.txt
    exit 1
  }
  
  # Now do it!
  die()
}""",
"""task funky < in=foo > out :: p=param
{
  function die () {
    echo "$@" 1>&2 > some.txt
    exit 1
  }
  
  # Now do it!
  die()
}""",
"""task funky 
     < in=foo > out :: p=param
{
  function die () {
    echo "$@" 1>&2 > some.txt
    exit 1
  }
  
  # Now do it!
  die()
}""",
"""task funky 
     < in=foo 
     > out 
     :: p=param
{
  function die () {
    echo "$@" 1>&2 > some.txt
    exit 1
  }
  
  # Now do it!
  die()
}""",
"""task funky
    // input
    < in=foo x=73
    // out
    > out
    // parameters
    :: p=param
{
  function die () {
    echo "$@" 1>&2 > some.txt
    exit 1
  }
  
  # Now do it!
  die()
}""",
"""task funky < in=foo x=73
    // out
    > out
    // parameters
    :: p=param
{
  function die () {
    echo "$@" 1>&2 > some.txt
    exit 1
  }
  
  # Now do it!
  die()
}""",
"""task funky : moses < in=foo x=73
    // out
    > out
    // parameters
    :: p=param
{
  function die () {
    echo "$@" 1>&2 > some.txt
    exit 1
  }
  
  # Now do it!
  die()
}""",
"""task funky // moses
    : moses < in=foo x=73
    // out
    > out
    // parameters
    :: p=param
{
  function die () {
    echo "$@" 1>&2 > some.txt
    exit 1
  }
  
  # Now do it!
  die()
}""",
"""task funky // moses
: moses < in=foo x=73
    // out
    > out
    // parameters
    :: p=param
{
  function die () {
    echo "$@" 1>&2 > some.txt
    exit 1
  }
  
  # Now do it!
  die()
}""",
"""task funky 
// moses
: moses < in=foo x=73
    // out
    > out
    // parameters
    :: p=param
{
  function die () {
    echo "$@" 1>&2 > some.txt
    exit 1
  }
  
  # Now do it!
  die()
}""",
"""task funky
  // moses
  : moses 
  // input
  < in=foo x=73
  // out
  > out
  // parameters
  :: p=param
{
  function die () {
    echo "$@" 1>&2 > some.txt
    exit 1
  }
  
  # Now do it!
  die()
}""",
"""task funky
  // moses
  : moses 
  // input
  < in=foo x=73
  // out
  > out
  // parameters
  :: p=param {
  function die () {
    echo "$@" 1>&2 > some.txt
    exit 1
  }
  
  # Now do it!
  die()
}""",
""" // Hello, world

task hello {
      echo "hello, world!"
}""",
""" // Hello, world
// some more

task hello {
      echo "hello, world!"
}""",
""" // Hello, world

// some more
task hello {
      echo "hello, world!"
}""",
""" // Hello, world

// some more

task hello {
      echo "hello, world!"
}""",
      """task hello {
        echo "hello, world!"
      }""",
"""task hello { // This is not a valid bash comment
      echo "hello, world!"
}""",
"""task hello { # Comments are not allowed after opening { braces
      echo "hello, world!"
}"""      
  ) 
  
  def failureCases = Set(
"""task funky < in=foo > out  
 bar {
  function die () {
    echo "$@" 1>&2 > some.txt
    exit 1
  }
  
  # Now do it!
  die()
}""" ,      
    " ",      
    "A-variable_Name__",
    "",      
    "> x y_txt",
    "< a=/etc/passwd b=/etc/hosts",
    "> x",
    "< a=$x@first > x",
    "tokenizer < in=(DataSet: train=a.txt tune=b.txt test=c.txt) > out",
    "< in=$out@tokenize[DataSet:train] > model",
    "< in=$out@tokenize[DataSet:tune] > weights",
    "moses tokenizerr giza < in=$out@tokenize[DataSet:test] > hyps",
    """moses tokenizerr giza
    < in=$out@tokenize[DataSet:test] > hyps""",
    """moses tokenizerr giza
    // Do some inputs
    < in=$out@tokenize[DataSet:test] 
    // Here's the result
    > hyps""" ,
    """// Package comments
      moses tokenizerr giza
    // Do some inputs
    < in=$out@tokenize[DataSet:test] 
    // Here's the result
    > hyps"""    
  ) 
  
  def errorCases = Set(                           
"""task hello {
      echo "hello, world!"
} yay"""
  )
  
}