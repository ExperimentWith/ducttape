// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.exec

import collection._
import ducttape.graph.UnpackedGraph.Literal
import ducttape.graph.UnpackedGraph.Task
import ducttape.graph.traversal.Visitor
import ducttape.workflow.Realization
//import ducttape.workflow.RealTask
import ducttape.syntax.Namespace
import ducttape.syntax.AbstractSyntaxTree.PackageDef
import ducttape.syntax.AbstractSyntaxTree.Spec
import ducttape.syntax.FileFormatException
import java.io.File

// Visits each planned task, discovering what software packages
// will be required to execute it 
class PackageFinder(todo: Option[Set[(String,Realization)]],
                    packageDefs: Map[String,PackageDef]) extends Visitor {
  
  val packages = new mutable.HashSet[PackageDef]
  
  override def visit(task: Task) {
    // TODO: Why do we need todo here? Isn't this enforced by the walker?
    if (todo == None || todo.get( (task.name, task.realization) )) {
      for (packageName: Literal <- task.specs.packages) {
        // TODO: XXX: HACK: Lane: This may not handle namespaces correctly
//        val packageNamespace = Namespace.fromString(packageSpec.name)
        if (packageDefs.contains(packageName.value)) {
          packages += packageDefs(packageName.value)
        } else {
          // TODO: This should be checked by now...
          throw new RuntimeException(s"Undefined package ${packageName.value}")
        }
      }
    }
  }
}
