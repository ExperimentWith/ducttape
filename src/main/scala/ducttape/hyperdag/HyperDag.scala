// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.hyperdag

import collection._
import ducttape.viz._
import ducttape.hyperdag.walker.VertexFilter
import ducttape.hyperdag.walker.DefaultVertexFilter
import ducttape.hyperdag.walker.DefaultToD
import ducttape.hyperdag.walker.RealizationMunger
import ducttape.hyperdag.walker.DefaultRealizationMunger
import ducttape.hyperdag.walker.Traversal
import ducttape.hyperdag.walker.Arbitrary

// XXX: HACK
import ducttape.workflow.SpecGroup

/**
 * An immutable representation of a Directed Acyclic Hypergraph (HyperDAG), returned
 * by a builder.
 *
 * roots: the vertices that have no parents
 * vertices: a complete list of all vertices in the HyperDAG
 * inEdgesMap: a map from a packed vertex to its incoming hyperdges
 * outEdgesMap: a map from a packed vertex to a sequence of hyperedges. each hyperedge
 *              will have one or more source vertices. the specified packed vertex must be
 *              one of those source vertices.
 * edges: a map from a hyperedge to a pair. the first element of the pair specifies
 *        the source vertices of the hyperedge (which are isomorphic to the hyperedge's edges).
 *        the second element of the pair specifies the sink vertex of the hyperedge.
 *
 * Note: In GraphViz or written notation, a "sink" vertex is the vertex pointed to by
 *       the hyperedge's arrow.
 *
 * V is the vertex payload type (in a workflow, this will be a TaskTemplate)
 * H is the hyperedge payload type (each hyperedge is composed of component "incoming" edges;
 *                                  in a workflow, this might be a Branch)
 * E is the edge payload type (in a workflow, this will be the set of input-output file pair
 *                             connected by the edge)
 */
class HyperDag[V,H,E](val roots: Seq[PackedVertex[V]],
                      val vertices: Seq[PackedVertex[V]],
                      private[hyperdag] val inEdgesMap: Map[PackedVertex[_], Seq[HyperEdge[H,E]]],
                      private[hyperdag] val outEdgesMap: Map[PackedVertex[_], Seq[HyperEdge[H,E]]],
                      private[hyperdag] val edges: Map[HyperEdge[H,E], (Seq[PackedVertex[V]],PackedVertex[V])]) {
  
  val size: Int = vertices.size

  def packedWalker() = new walker.PackedDagWalker[V](this)

  def unpackedWalker[D,F](munger: RealizationMunger[V,H,E,D,F],
                          vertexFilter: VertexFilter[V,H,E,D],
                          toD: H => D,
                          traversal: Traversal = Arbitrary)
                         (implicit ordering: Ordering[D])
    = new walker.UnpackedDagWalker[V,H,E,D,F](this, munger, vertexFilter, toD, traversal)
    
  def unpackedWalker[D](vertexFilter: VertexFilter[V,H,E,D] = new DefaultVertexFilter[V,H,E,D],
                        toD: H => D = new DefaultToD[H],
                        traversal: Traversal = Arbitrary)
                       (implicit ordering: Ordering[D]) = {
    val munger = new DefaultRealizationMunger[V,H,E,D]
    new walker.UnpackedDagWalker[V,H,E,D,immutable.HashSet[D]](this, munger, vertexFilter, toD, traversal)
  }
    
  def inEdges(v: PackedVertex[_]): Seq[HyperEdge[H,E]]
    = inEdgesMap.getOrElse(v, Seq.empty)
  def outEdges(v: PackedVertex[_]): Seq[HyperEdge[H,E]]
    = outEdgesMap.getOrElse(v, Seq.empty)
  def parents(v: PackedVertex[_]): Seq[PackedVertex[V]]
    = for (e <- inEdges(v); src <- sources(e)) yield src
  def children(v: PackedVertex[_]): Seq[PackedVertex[V]]
    = for (e <- outEdges(v)) yield sink(e)
  def sources(e: HyperEdge[H,E]): Seq[PackedVertex[V]]
    = edges(e)._1
  def sink(e: HyperEdge[H,E]): PackedVertex[V]
    = edges(e)._2

  def toGraphViz(): String = toGraphViz(vertices, inEdges, { v => v.toString }) 

  def toGraphViz(vertexList: Seq[PackedVertex[V]],
                 inEdgesFunc: PackedVertex[V] => Seq[HyperEdge[H,E]],
                 stringifyV: PackedVertex[V] => String): String = {
      
    // TODO: Expose these
    // XXX: HACK
    def stringifyH(h: H): String = if (h == null) "" else h.toString
    def stringifyE(e: E): String = if (e == null) {
      ""
    } else if(e.isInstanceOf[SpecGroup]) {
      val sg = e.asInstanceOf[SpecGroup]
      sg.toString(withNewlines=true)
    } else {
      e.toString
    }
    def colorizeV(v: PackedVertex[V]): String = {
      if (v.toString.startsWith("Phantom")) {
        "gray"
      } else if(v.toString.startsWith("Epsilon")) {
        "yellow"
      } else {
        "white"
      }  
    }
      
    val str = new StringBuilder(1000)
    str ++= "digraph G {\n"
    for (v: PackedVertex[V] <- vertexList) {
      if(v.toString.startsWith("Epsilon")){
        val color = colorizeV(v)
        str ++= "\"%s\" [fillcolor=\"%s\",style=\"filled\"]\n".format(GraphViz.escape(stringifyV(v)), color)
        for (he: HyperEdge[H,E] <- inEdgesFunc(v)) {
          for ( (ant, e) <- sources(he).zip(he.e)) {
            str ++= "\"%s\" -> \"%s\" [label=\"%s\"]\n".format(
                  GraphViz.escape(stringifyV(ant)),
                  GraphViz.escape(stringifyV(v)),
                  GraphViz.escape(stringifyH(he.h)))                      
          }
        }
      }
    }
    str ++= "}\n"
    str.toString
  }
}
