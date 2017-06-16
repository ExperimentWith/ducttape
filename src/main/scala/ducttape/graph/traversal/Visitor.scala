// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
package ducttape.graph.traversal

import ducttape.graph.UnpackedGraph
import ducttape.graph.UnpackedGraph.Task


import grizzled.slf4j.Logging


trait Visitor {
  def visit(task: Task)
}



object Visitor extends Logging {

  def visitAll(
      graph: UnpackedGraph,
      visitor: Visitor,
      numCores: Int = 1,
      traversal: Traversal = Arbitrary): Visitor = {
    
    debug(s"Visiting workflow using traversal: ${traversal}")
    val walker = new UnpackedGraphWalker(graph, traversal)
    walker.foreach(numCores, { task: Task =>
      debug(s"Visiting ${task}")
      visitor.visit(task)
    })
    visitor
  }

}
