/*
 *  Topology.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2014 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.synth

import collection.immutable.{IndexedSeq => Vec}
import collection.mutable.{Set => MSet, Stack => MStack}

object Topology {
  def empty[V, E <: Edge[V]] = apply[V, E](emptySeq, Set.empty)(0, Map.empty)

  trait Edge[+V] {
    def sourceVertex: V
    def targetVertex: V
  }

  private val emptySeq = Vec.empty[Nothing]
}

/** An online toplogical order maintenance structure. This is an immutable data structure with
  * amortized costs. The edge adding operation returns a new copy of the modified structure along
  * with a list of vertices which have been moved due to the insertion. The caller can then use
  * that list to adjust any views (e.g. DSP processes).
  *
  * @param  vertices      the vertices in the structure
  * @param  edges         a set of edges between the vertices
  * @param  unpositioned  the number of unpositioned vertices (the leading elements in `vertices`)
  * @param  edgeMap       allows lookup of edges via vertex keys
  */
final case class Topology[V, E <: Topology.Edge[V]](vertices: Vec[V], edges: Set[E])
                                                   (unpositioned: Int, edgeMap: Map[V, Set[E]])
  extends Ordering[V] {

  import Topology.emptySeq

  type T = Topology[V, E]

  override def toString = s"Topology($vertices, $edges)($unpositioned, $edgeMap)"

  /** For two positioned vertices `a` and `b`, returns `-1` if `a` is before `b`, or `1` if `a` follows `b`,
    *  or `0` if both are equal. Throws an exception if `a` or `b` is unpositioned.
    */
  def compare(a: V, b: V): Int = {
    val ai = vertices.indexOf(a)
    val bi = vertices.indexOf(b)
    require(ai >= unpositioned && bi >= unpositioned)
    if (ai < bi) -1 else if (ai > bi) 1 else 0
  }

  /** Tries to insert an edge into the topological order.
    * Throws an exception of the source or target vertex of the edge is not contained in the vertex list of this
    * structure.
    *
    * @param e  the edge to insert
    * @return  `None` if the edge would violate acyclicity, otherwise `Some` tuple contains
    *          the new topology, the reference vertex and the affected vertices which need to
    *          be moved with respect to the reference to reflect the new ordering. In case
    *          that the reference is the source vertex of the added edge, the affected vertices
    *          should be moved _after_ the reference and keep their internal grouping order.
    *          In case the reference is the target vertex, the affected vertices sequence is
    *          guaranteed to consist only exactly one element -- the source vertex -- which
    *          should be moved _before_ the reference
    */
  def addEdge(e: E): Option[(T, V, Vec[V])] = {
    val source	   = e.sourceVertex
    val target	   = e.targetVertex
    val upBound	   = vertices.indexOf(source)
    val loBound	   = vertices.indexOf(target)
    require(loBound >= 0 && upBound >= 0)
    val newEdgeMap: Map[V, Set[E]] = edgeMap + (source -> (edgeMap.getOrElse(source, Set.empty) + e))
    val newEdgeSet = edges + e

    // dealing with unpositioned elements
    if (upBound < unpositioned) { // first edge for source
      if (loBound < unpositioned) { // first edge for target
        val min         = math.min(upBound, loBound)
        val max         = math.max(upBound, loBound)
        val newUnpos    = unpositioned - 2
        val newVertices = vertices
          .patch(min     , emptySeq, 1)
          .patch(max - 1 , emptySeq, 1)
          .patch(newUnpos, Vec(source, target), 0)
        Some((copy(newVertices, newEdgeSet)(newUnpos, newEdgeMap), source, Vec(target)))
      } else {
        //            Some( (this, emptySeq) )
        val newUnpos    = unpositioned - 1
        val sourceSeq   = Vec(source)
        val newVertices = vertices
          .patch(upBound    , emptySeq , 1)
          .patch(loBound - 1, sourceSeq, 0)
        Some((copy(newVertices, newEdgeSet)(newUnpos, newEdgeMap), target, sourceSeq))
      }

      // regular algorithm
    } else if (loBound > upBound) {
      Some((copy(vertices, newEdgeSet)(unpositioned, newEdgeMap), source, emptySeq))
    } else if (loBound < upBound) {
      val visited = MSet.empty[V]
      if (!discovery(visited, newEdgeMap, target, upBound)) {
        None // Cycle --> Abort
      } else {
        val (newVertices, affected) = shift(visited, loBound, upBound)
        val newUnpos                = if (loBound < unpositioned) unpositioned - 1 else unpositioned
        Some((copy(newVertices, newEdgeSet)(newUnpos, newEdgeMap), source, affected))
      }
    } else {
      // loBound == upBound
      None
    }
  }

  def removeEdge(e: E): T = {
    if (edges.contains(e)) {
      val source = e.sourceVertex
      copy(edges = edges - e)(unpositioned, edgeMap + (source -> (edgeMap(source) - e)))
    } else this
  }

  def addVertex(v: V): T = {
    require(!vertices.contains(v))
    // XXX TEST
    //      copy( vertices.patch( unpositioned, Vec( v ), 0 ), unpositioned + 1 )
    copy(v +: vertices)(unpositioned + 1, edgeMap)
  }

  /** Removes a vertex and all associated edges. */
  def removeVertex(v: V): T = {
    val idx = vertices.indexOf(v)
    if (idx >= 0) {
      val newV = vertices.patch(idx, emptySeq, 1)
      if (idx < unpositioned) {
        val newUnpos  = unpositioned - 1
        copy(newV)(newUnpos, edgeMap)
      } else {
        if (edgeMap.contains(v)) {
          val e     = edgeMap(v)
          val newEM = edgeMap - v
          val newE  = edges -- e
          copy(newV, newE)(unpositioned, newEM)
        } else {
          copy(newV)(unpositioned, edgeMap)
        }
      }
    } else this
  }

  // note: assumes audio rate
  private def discovery(visited: MSet[V], newEdgeMap: Map[V, Set[E]], v: V, upBound: Int): Boolean = {
    val targets = MStack(v)
    while (targets.nonEmpty) {
      val v           = targets.pop()
      visited        += v
      val moreTargets = newEdgeMap.getOrElse(v, Set.empty).map(_.targetVertex)
      val grouped     = moreTargets.groupBy { t =>
        val vidx = vertices.indexOf(t)
        if (vidx < upBound) -1 else if (vidx > upBound) 1 else 0
      }
      if (grouped.contains(0)) return false // cycle detected
      // visit s if it was not not already visited
      // and if it is in affected region
      //         grouped.get( -1 ).foreach( targets.pushAll( _.diff( visited )))
      targets.pushAll(grouped.getOrElse(-1, Set.empty).filter(!visited.contains(_)))
    }
    true
  }

  // initial cond: loBound (target) < upBound (source)
  private def shift(visited: collection.Set[V], loBound: Int, upBound: Int): (Vec[V], Vec[V]) = {
    // shift vertices in affected region down ord
    val (a, b) = vertices.splitAt(upBound)
    val (begin, target) = a.splitAt(loBound)
    //      val (source, end)          = b.splitAt( 1 )
    val source = b.head
    val end = b.tail
    val (affected, unaffected) = target.partition(visited.contains)

    val shifted = begin ++ unaffected ++ (source +: affected) ++ end

    // push to transaction
    //      error( "NOT YET IMPLEMENTED" )
    //      affected.foldLeft( source )( (pred, succ) => { succ.moveAfter( tx, pred ); succ })

    (shifted, affected)
  }
}