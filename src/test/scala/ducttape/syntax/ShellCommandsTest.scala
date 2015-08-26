// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.syntax
import ducttape.util.AbstractTest
import ducttape.syntax.GrammarParser.Parser
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ShellCommandsTest extends AbstractTest("shell commands",Grammar.shellCommands) {
 
  def successCases = Set(
    "",
    " ",
    "A-variable_Name__",  
    "moses tokenizer giza",
    "moses",
    """ruby -e 'puts "#{month_string.upcase}(#{today.year}-#{month_num})#{month_string.downcase}"'""",
    """# Package comments
      moses tokenizerr giza""",
    "the { open } close",  
    "the { open then {nested} } close",  
    "the { {nested} } close",  
    "the { {{}} } close", 
   "the { {{}} } close { more! }",       
    """
      #!/bin/bash

      # NOTE:
      # Versions 1.9 (or higher) of aclocal and automake are required.

      # For Mac OSX users:
      # Standard distribution usually includes versions 1.6.
      # Get versions 1.9 or higher
      # Set the following variable to the correct paths
      #ACLOCAL="/path/to/aclocal-1.9"
      #AUTOMAKE="/path/to/automake-1.9"

      function die () {
      echo "$@" >&2
      exit 1
      }

      if [ -z "$ACLOCAL" ]
      then
      ACLOCAL=`which aclocal`
      fi

      if [ -z "$AUTOMAKE" ]
      then
      AUTOMAKE=`which automake`
      fi

      if [ -z "$AUTOCONF" ]
      then
      AUTOCONF=`which autoconf`
      fi

      if [ -z "$LIBTOOLIZE" ]
      then
      LIBTOOLIZE=`which libtoolize`

      if [ -z "$LIBTOOLIZE" ]
      then
      LIBTOOLIZE=`which glibtoolize`
      fi
      fi


      echo "Calling $ACLOCAL..."
      $ACLOCAL -I m4 || die "aclocal failed"
      echo "Calling $AUTOCONF..."
      $AUTOCONF  || die "autoconf failed"
      echo "Calling $AUTOMAKE..."
      $AUTOMAKE || die "automake failed"
      echo "Calling $LIBTOOLIZE"
      $LIBTOOLIZE || die "libtoolize failed"


      echo
      echo "You should now be able to configure and build:"
      echo "   ./configure [--with-srilm=/path/to/srilm] [--with-irstlm=/path/to/irstlm] [--with-randlm=/path/to/randlm] [--without-kenlm] [--with-synlm=/path/to/modelblocks] [--with-xmlrpc-c=/path/to/xmlrpc-c-config]"
      echo "   make -j 4"
      echo    
    """,
"""
#!/bin/bash

# NOTE:
# Versions 1.9 (or higher) of aclocal and automake are required.

# For Mac OSX users:
# Standard distribution usually includes versions 1.6.
# Get versions 1.9 or higher
# Set the following variable to the correct paths
#ACLOCAL="/path/to/aclocal-1.9"
#AUTOMAKE="/path/to/automake-1.9"

function die () {
  echo "$@" >&2
  exit 1
}

if [ -z "$ACLOCAL" ]
then
    ACLOCAL=`which aclocal`
fi

if [ -z "$AUTOMAKE" ]
then
    AUTOMAKE=`which automake`
fi

if [ -z "$AUTOCONF" ]
then
    AUTOCONF=`which autoconf`
fi

if [ -z "$LIBTOOLIZE" ]
then
    LIBTOOLIZE=`which libtoolize`

    if [ -z "$LIBTOOLIZE" ]
    then
        LIBTOOLIZE=`which glibtoolize`
    fi
fi


echo "Calling $ACLOCAL..."
$ACLOCAL -I m4 || die "aclocal failed"
echo "Calling $AUTOCONF..."
$AUTOCONF  || die "autoconf failed"
echo "Calling $AUTOMAKE..."
$AUTOMAKE || die "automake failed"
echo "Calling $LIBTOOLIZE"
$LIBTOOLIZE || die "libtoolize failed"


echo
echo "You should now be able to configure and build:"
echo "   ./configure [--with-srilm=/path/to/srilm] [--with-irstlm=/path/to/irstlm] [--with-randlm=/path/to/randlm] [--without-kenlm] [--with-synlm=/path/to/modelblocks] [--with-xmlrpc-c=/path/to/xmlrpc-c-config]"
echo "   make -j 4"
echo    
"""     
  ) 
  
  def failureCases = Set(
    "}",
    "{",
    " }"    
  ) 
  
  def errorCases = Set(
    
  )
  
  
}