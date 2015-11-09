/*
 *  AuralAttributeTargetImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2015 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc
package impl

import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.synth.{Txn, DynamicUser, BusNodeSetter, Resource, Synth, AudioBus, NodeRef, Sys}
import de.sciss.synth
import de.sciss.synth.SynthGraph
import de.sciss.synth.proc.AuralAttribute.Value

import scala.concurrent.stm.{Ref, TMap}

class AuralAttributeTargetImpl[S <: Sys[S]](target: NodeRef, key: String, targetBus: AudioBus)
  extends AuralAttribute.Target[S] {

  import targetBus.{numChannels, server}

  private def ctlName = graph.Attribute.controlName(key)

  private val map       = TMap.empty[AuralAttribute[S], Connected]
  private val stateRef  = Ref[State](Empty)

  private final class Connected(val value: AuralAttribute.Value,
                                      val users: List[DynamicUser]) extends Disposable[Txn] {
    def dispose()(implicit tx: Txn): Unit = users.foreach(_.dispose())
  }

  private sealed trait State {
    def put   (source: AuralAttribute[S], value: AuralAttribute.Value)(implicit tx: S#Tx): State
    def remove(source: AuralAttribute[S])(implicit tx: S#Tx): State
  }

  private final class AddRemoveEdge(edge: NodeRef.Edge) extends DynamicUser {
    def add   ()(implicit tx: Txn): Unit = server.addEdge   (edge)
    def remove()(implicit tx: Txn): Unit = server.removeEdge(edge)
  }

  private def putSingleScalar(source: AuralAttribute[S], value: AuralAttribute.Scalar)
                                   (implicit tx: S#Tx): State = {
    implicit val itx = tx.peer
    target.node.set(value.toControl(ctlName))
    val cc = new Connected(value, Nil)
    map.put(source, cc).foreach(_.dispose())
    new Single(source, cc)
  }

  private def putSingleStream(source: AuralAttribute[S], value: AuralAttribute.Stream)
                                   (implicit tx: S#Tx): State = {
    implicit val itx = tx.peer
    val edge      = NodeRef.Edge(value.source, target)
    val edgeUser  = new AddRemoveEdge(edge)
    val busUser   = BusNodeSetter.mapper(ctlName, value.bus, target.node)
    addUser(edgeUser)
    addUser(busUser)
    val cc        = new Connected(value, edgeUser :: busUser :: Nil)
    map.put(source, cc).foreach(_.dispose())
    new Single(source, cc)
  }

  // ----

  private object Empty extends State {
    def put(source: AuralAttribute[S], value: AuralAttribute.Value)(implicit tx: S#Tx): State =
      value match {
        case sc: AuralAttribute.Scalar => putSingleScalar(source, sc)
        case sc: AuralAttribute.Stream => putSingleStream(source, sc)
      }

    def remove(source: AuralAttribute[S])(implicit tx: S#Tx): State =
      throw new NoSuchElementException(source.toString)
  }

  // ----

  private final class Single(aa: AuralAttribute[S], cc: Connected) extends State {
    def put(source: AuralAttribute[S], value: AuralAttribute.Value)(implicit tx: S#Tx): State =
      if (source == aa) {
        Empty.put(source, value)
      } else {
        implicit val itx = tx.peer
        cc.dispose()
        val c1 = mkVertex(cc.value)
        val c2 = mkVertex(   value)
        map.put(aa    , c1)
        map.put(source, c2)
        Multiple
      }

    def remove(source: AuralAttribute[S])(implicit tx: S#Tx): State = {
      implicit val itx = tx.peer
      map.remove(source).fold(throw new NoSuchElementException(source.toString))(_.dispose())
      Empty
    }
  }

  // ----

  private object Multiple extends State {
    def put(source: AuralAttribute[S], value: Value)(implicit tx: S#Tx): State = {
      implicit val itx = tx.peer
      val con = mkVertex(value)
      map.put(source, con).foreach(_.dispose())
      this
    }

    def remove(source: AuralAttribute[S])(implicit tx: S#Tx): State = ???
  }
  
  // ----

  private def mkVertex(value: AuralAttribute.Value)(implicit tx: S#Tx): Connected = {
    val users: List[DynamicUser] = value match {
      case sc: AuralAttribute.Scalar =>
        val g = SynthGraph {
          import synth._
          import ugen._
          val value = "value".kr(Vector.fill(numChannels)(0f))
          Out.ar("bus".kr, value)
        }
        val values0   = sc.values
        val len0      = values0.length
        val syn       = Synth.play(g, nameHint = Some("attr"))(target = server,
          args = List("value" -> Vector.tabulate[Float](numChannels)(i => values0(i % len0))))
        addDependency(syn)
        val edge      = NodeRef.Edge(syn, target)
        val edgeUser  = new AddRemoveEdge(edge)
        val busUser   = BusNodeSetter.writer("bus", targetBus, syn)
        addUser(edgeUser)
        addUser(busUser)
        edgeUser :: busUser :: Nil

      case sc: AuralAttribute.Stream =>
        ???
    }
    new Connected(value, users)
  }

  private def addUser(user: DynamicUser)(implicit tx: S#Tx): Unit = ???
  private def addDependency(r: Resource)(implicit tx: S#Tx): Unit = ???

  def put(source: AuralAttribute[S], value: Value)(implicit tx: S#Tx): Unit =
    stateRef.transform(_.put(source, value))(tx.peer)

  def remove(source: AuralAttribute[S])(implicit tx: S#Tx): Unit =
    stateRef.transform(_.remove(source))(tx.peer)
}