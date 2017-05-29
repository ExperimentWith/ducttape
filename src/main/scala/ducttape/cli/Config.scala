// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.cli

object Config {
  // TODO: Use Map for color sot that we can remove all of them easily?
  var headerColor = Console.BLUE
  var byColor = Console.BLUE
  var taskColor = Console.CYAN
  var warnColor = Console.BOLD + "\u001b[1;33;40m\u001b[1m"
  var errorColor = Console.RED
  var resetColor = Console.RESET

  var modeColor = Console.GREEN

  var errorLineColor = Console.BLUE // file and line number of error
  var errorScriptColor = Console.WHITE // quote from file

  var taskNameColor = Console.CYAN
  var realNameColor = Console.BLUE
  var realFullNameColor = Console.YELLOW

  var greenColor = Console.GREEN
  var redColor = Console.RED
  
  // TODO: Enum?
  def clearColors() {
    headerColor = ""
    byColor = ""
    taskColor = ""
    warnColor = ""
    errorColor = ""
    resetColor = ""

    modeColor = ""

    errorLineColor = ""
    errorScriptColor = ""

    taskNameColor = ""
    realNameColor = ""
    realFullNameColor = ""

    greenColor = ""
    redColor = ""
  }
}
