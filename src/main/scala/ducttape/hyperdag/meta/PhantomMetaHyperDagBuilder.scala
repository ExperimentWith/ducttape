// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.hyperdag.meta

import ducttape.hyperdag.PackedVertex

/**
 * See [[ducttape.hyperdag.meta.PhantomMetaHyperDag]] for definitions of phantom, etc.
 *
 * "comment" in all methods below indicates a GraphViz comment.
 */
class PhantomMetaHyperDagBuilder[V,M,H,E](epsilonV: V = null, epsilonH: H = null, epsilonE: E = null) {
  
  private val delegate = new MetaHyperDagBuilder[Option[V],M,H,E](Some(epsilonV), epsilonH, epsilonE)
  

  def addPhantomVertex(comment: Option[String] = None): PackedVertex[Option[V]]
    = delegate.addVertex(None, comment)
    
  def addPhantomVertex(comment: String): PackedVertex[Option[V]]
    = delegate.addVertex(None, Some(comment))

  def addVertex(v: V, comment: Option[String] = None): PackedVertex[Option[V]]
    = delegate.addVertex(Some(v), comment)
    
  def addVertex(v: V, comment: String): PackedVertex[Option[V]]
    = delegate.addVertex(Some(v), Some(comment))

  /* this method allows sources to be optional, indicating that edges *may*
   * have phantom source vertices */
  def addMetaEdge(m: M,
                  hyperEdgeInfo: Seq[(H, Seq[(PackedVertex[Option[V]],E)])],
                  sink: PackedVertex[Option[V]],
                  comment: Option[String]): MetaEdge[M,H,E] = {
    delegate.addMetaEdge(m, hyperEdgeInfo, sink, comment)
  }
  
  def addMetaEdge(m: M,
                  hyperEdgeInfo: Seq[(H, Seq[(PackedVertex[Option[V]],E)])],
                  sink: PackedVertex[Option[V]],
                  comment: String): MetaEdge[M,H,E] = {
    delegate.addMetaEdge(m, hyperEdgeInfo, sink, Some(comment))
  }
  
  // create an immutable version of this graph
  def build() = {
    /*for ((v: PackedVertex[_], me: MetaEdge[M,H,E]) <- delegate.build().metaEdgesByEpsilon){
      print(v + " : " + me + "\n")
    }*/
    //print(delegate.build().toGraphViz())
    //exit(0)
    new PhantomMetaHyperDag[V,M,H,E](delegate.build())
  }
}
