// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.hyperdag.meta

import collection._
import ducttape.hyperdag.walker.PackedMetaDagWalker
import ducttape.hyperdag.walker.UnpackedMetaDagWalker
import ducttape.hyperdag.HyperDag
import ducttape.hyperdag.PackedVertex
import ducttape.hyperdag.HyperEdge
import ducttape.hyperdag.walker.MetaVertexFilter
import ducttape.hyperdag.walker.DefaultMetaVertexFilter
import ducttape.hyperdag.walker.DefaultToD
import ducttape.hyperdag.walker.RealizationMunger
import ducttape.hyperdag.walker.DefaultRealizationMunger
import ducttape.hyperdag.walker.Traversal
import ducttape.hyperdag.walker.Arbitrary
import ducttape.workflow.SpecGroup
import ducttape.viz.GraphViz

/** essentially -- an AND-OR HyperDAG 
 * an implementation of MetaHyperDAGs based on transforming
 * meta-edges into epsilon vertices (but these are hidden from the user)
 *
 * this immutable representation is returned by a builder.
 *
 * walker will note when epsilon vertices are completed, but not actually
 * return them to the user
 *
 * V is the vertex payload type (in a workflow, this will be a TaskTemplate)
 * M is the metaedge payload type (in a workflow, this will be the BranchPoint)
 * H is the hyperedge payload type (each hyperedge is composed of component "incoming" edges;
 *                                  in a workflow, this might be a Branch)
 * E is the edge payload type (in a workflow, this will be the set of input-output file pair
 *                             connected by the edge) */
class MetaHyperDag[V,M,H,E](val delegate: HyperDag[V,H,E],
                            private[hyperdag] val metaEdgesByEpsilon: Map[PackedVertex[_],MetaEdge[M,H,E]],
                            private[hyperdag] val epsilonEdges: Set[HyperEdge[H,E]]) {

  // don't include epsilon vertices
  val size: Int = delegate.size - metaEdgesByEpsilon.size

  private[hyperdag] def isEpsilon(v: PackedVertex[_]) = metaEdgesByEpsilon.contains(v)
  private[hyperdag] def shouldSkip(v: PackedVertex[_]) = isEpsilon(v)
  private[hyperdag] def isEpsilon(h: HyperEdge[H,E]) = epsilonEdges(h)

  def packedWalker() = new PackedMetaDagWalker[V](this) // TODO: Exclude epsilons from completed, etc.

  def unpackedWalker[D,F](munger: RealizationMunger[V,H,E,D,F],
                          vertexFilter: MetaVertexFilter[V,H,E,D],
                          toD: H => D,
                          traversal: Traversal = Arbitrary)
                         (implicit ordering: Ordering[D])= {
    // TODO: Exclude epsilons from completed, etc.
    // TODO: Map epsilons and phantoms for the munger in this class instead of putting
    // the burden on the munger
    new UnpackedMetaDagWalker[V,M,H,E,D,F](this, munger, vertexFilter, toD, traversal)
  }
  
  def unpackedWalker[D](vertexFilter: MetaVertexFilter[V,H,E,D] = new DefaultMetaVertexFilter[V,H,E,D],
                        toD: H => D = new DefaultToD[H],
                        traversal: Traversal = Arbitrary)
                       (implicit ordering: Ordering[D]) = {
    val munger = new DefaultRealizationMunger[V,H,E,D]
    new UnpackedMetaDagWalker[V,M,H,E,D,immutable.HashSet[D]](this, munger, vertexFilter, toD, traversal)
  }

  def inMetaEdges(v: PackedVertex[_]): Seq[MetaEdge[M,H,E]]
    = for (parent <- delegate.parents(v)) yield metaEdgesByEpsilon(parent)
  def inHyperEdges(me: MetaEdge[M,H,E]): Seq[HyperEdge[H,E]]
    = delegate.inEdgesMap.getOrElse(me.epsilonV, Seq.empty)
  def outHyperEdges(v: PackedVertex[_]): Seq[HyperEdge[H,E]]
    = delegate.outEdgesMap.getOrElse(v, Seq.empty)
  def outMetaEdge(he: HyperEdge[H,E]): MetaEdge[M,H,E] = {
    return metaEdgesByEpsilon(delegate.sink(he))
  }

  private def skipEpsilons(v: PackedVertex[_], func: PackedVertex[_] => Seq[PackedVertex[V]]): Seq[PackedVertex[V]] = {
    val directParents = func(v)
    // check if we can just return these parents without modification
    if (directParents.exists(p => shouldSkip(p))) {
      // replace the epsilon vertices by their parents
      // it's guaranteed that those parents are not epsilon vertices themselves
      // TODO: This could be made into an ArrayBuffer if this turns out to be inefficient
      directParents.flatMap {
        case p if (isEpsilon(p)) => func(p)
        case p => Seq(p) // direct parent is normal
      }
    } else {
      directParents
    }    
  }

  def vertices() = delegate.vertices.filter(!shouldSkip(_))
  def parents(v: PackedVertex[_]): Seq[PackedVertex[V]] = skipEpsilons(v, delegate.parents)
  def children(v: PackedVertex[_]): Seq[PackedVertex[V]] = skipEpsilons(v, delegate.children)
  
  def sources(e: MetaEdge[M,H,E]): Seq[PackedVertex[V]] = {
    val srcs = new mutable.ArrayBuffer[PackedVertex[V]]
    for (inEdge <- inHyperEdges(e)) {
      srcs ++= delegate.sources(inEdge)
    }
    srcs
  }
  def sources(e: HyperEdge[H,E]): Seq[PackedVertex[V]] = delegate.sources(e)
  def sink(e: HyperEdge[H,E]): PackedVertex[V] = sink(outMetaEdge(e))
  def sink(e: MetaEdge[M,H,E]): PackedVertex[V] = delegate.children(e.epsilonV).head

  def toGraphViz(): String = toGraphViz(metaEdgesByEpsilon, { v => v.toString })
  //def toGraphViz(): String = delegate.toGraphViz()

  def toGraphViz(vertexMap: Map[PackedVertex[_],MetaEdge[M,H,E]],
                 stringifyV: PackedVertex[_] => String): String = {
      
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
    def colorizeV(v: PackedVertex[_]): String = {
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
    for ((v: PackedVertex[_], me: MetaEdge[M,H,E]) <- vertexMap) {
      val color = colorizeV(v)
      str ++= "\"%s\" [fillcolor=\"%s\",style=\"filled\"]\n".format(GraphViz.escape(stringifyV(v)), color)
      for (he: HyperEdge[H,E] <- inHyperEdges(me)) {
        for ( (ant, e) <- sources(he).zip(he.e)) {
          val color = colorizeV(ant)
          str ++= "\"%s\" [fillcolor=\"%s\",style=\"filled\"]\n".format(GraphViz.escape(stringifyV(ant)), color)
          str ++= "\"%s\" -> \"%s\" [label=\"%s\"]\n".format(
                  GraphViz.escape(stringifyV(ant)),
                  GraphViz.escape(stringifyV(v)),
                  GraphViz.escape(stringifyH(he.h)))
        }
      }      
      str ++= "\"%s\" -> \"%s\" [label=\"%s\"]\n".format(
              GraphViz.escape(stringifyV(v)),
              GraphViz.escape(stringifyV(sink(me))),
              GraphViz.escape("")) 
    }
    str ++= "}\n"
    str.toString
  }
}

