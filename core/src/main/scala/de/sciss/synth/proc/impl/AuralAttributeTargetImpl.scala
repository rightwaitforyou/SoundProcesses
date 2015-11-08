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

  private[this] def ctlName = graph.Attribute.controlName(key)

  private[this] val map       = TMap.empty[AuralAttribute[S], Connected]
  private[this] val stateRef  = Ref[State](Empty)

  private[this] final class Connected(val value: AuralAttribute.Value,
                                      val users: List[DynamicUser]) extends Disposable[Txn] {
    def dispose()(implicit tx: Txn): Unit = users.foreach(_.dispose())
  }

  private[this] sealed trait State {
    def put   (source: AuralAttribute[S], value: AuralAttribute.Value)(implicit tx: S#Tx): State
    def remove(value: AuralAttribute.Value)(implicit tx: S#Tx): State
  }

  private[this] final class AddRemoveEdge(edge: NodeRef.Edge) extends DynamicUser {
    import edge.source.server

    def add   ()(implicit tx: Txn): Unit = server.addEdge   (edge)
    def remove()(implicit tx: Txn): Unit = server.removeEdge(edge)
  }

  private[this] def putSingleScalar(source: AuralAttribute[S], value: AuralAttribute.Scalar)
                                   (implicit tx: S#Tx): State = {
    target.node.set(value.toControl(ctlName))
    map.put(source, new Connected(value, Nil))(tx.peer)
    SingleScalar
  }

  private[this] object Empty extends State {
    def put(source: AuralAttribute[S], value: AuralAttribute.Value)(implicit tx: S#Tx): State =
      value match {
        case sc: AuralAttribute.Scalar => putSingleScalar(source, sc)

        case AuralAttribute.Stream(sourceNode, sourceBus) =>
          val edge      = NodeRef.Edge(sourceNode, target)
          val edgeUser  = new AddRemoveEdge(edge)
          val busUser   = BusNodeSetter.mapper(ctlName, sourceBus, target.node)
          addUser(edgeUser)
          addUser(busUser)
          map.put(source, new Connected(value, edgeUser :: busUser :: Nil))(tx.peer)
          SingleStream
      }

    def remove(value: AuralAttribute.Value)(implicit tx: S#Tx): State = this
  }

  private[this] object SingleScalar extends State {
    def put(source: AuralAttribute[S], value: AuralAttribute.Value)(implicit tx: S#Tx): State =
      value match {
        case sc: AuralAttribute.Scalar => putSingleScalar(source, sc)  // simply replace

        case AuralAttribute.Stream(sourceNode, sourceBus) =>
          val edge      = NodeRef.Edge(sourceNode, target)
          val edgeUser  = new AddRemoveEdge(edge)
          val busUser   = BusNodeSetter.mapper(ctlName, sourceBus, target.node)
          addUser(edgeUser)
          addUser(busUser)
          map.put(source, new Connected(value, edgeUser :: busUser :: Nil))(tx.peer)
          SingleStream
      }

    def remove(value: AuralAttribute.Value)(implicit tx: S#Tx): State = Empty
  }

  private[this] def addUser(user: DynamicUser)(implicit tx: S#Tx): Unit = ???

  private[this] object SingleStream extends State {
    def put(source: AuralAttribute[S], value: AuralAttribute.Value)(implicit tx: S#Tx): State = ???

    def remove(value: AuralAttribute.Value)(implicit tx: S#Tx): State = Empty
  }

  private[this] def addDependency(r: Resource)(implicit tx: S#Tx): Unit = ???

  def put(source: AuralAttribute[S], value: Value)(implicit tx: S#Tx): Unit = {
    if (map.isEmpty(tx.peer)) putFirst(source, value)
  }

  private[this] def putFirst(source: AuralAttribute[S], value: Value)(implicit tx: S#Tx): Unit =
    value match {
      case sc: AuralAttribute.Scalar =>
        target.node.set(sc.toControl(ctlName))
      case AuralAttribute.Stream(sourceNode, sourceBus) =>
        ???
    }

  private[this] def put1(source: AuralAttribute[S], value: Value)(implicit tx: S#Tx): Unit = {
    implicit val itx = tx.peer

    // cases
    // - map was empty | map size was one and value replaces previous value
    //   - value is scalar -> stay scalar
    //   - value is stream -> establish stream
    // - map size was one and value is new
    //   -

    val oldSize   = map.size
//    val oldValOpt = map.put(source, value)
    val isScalar  = value.isScalar

//    (oldSize, value, oldValOpt) match {
//      case (0, sc: AuralAttribute.Scalar, _) =>
//      case (0, sc: AuralAttribute.Stream, _) =>
//      case (1, sc: AuralAttribute.Scalar, Some(osc: AuralAttribute.Scalar)) =>
//
//    }

    if (oldSize == 0) {
      value match {
        case sc: AuralAttribute.Scalar =>
          target.node.set(sc.toControl(ctlName))
        case AuralAttribute.Stream(sourceNode, sourceBus) =>
          ???
      }

    } else {
      targetBus.busOption match {
        case Some(bus) =>
        //  targetBus.addWriter()

        case None =>
      }
    }


    ???
  }

  private[this] def lift(in: AuralAttribute.Scalar, numChannels: Int)(implicit tx: S#Tx): AuralAttribute.Stream = {
    val g = SynthGraph {
      import synth._
      import ugen._
      val values0 = in.values
      val len0    = values0.length
      val value   = "value".kr(Vector.tabulate[Float](numChannels)(i => values0(i % len0)))
      Out.ar("bus".kr, value)
    }
    val syn = Synth.play(g, nameHint = Some("attr"))(target = ???, args = ???, addAction = ???, dependencies = ???)
    addDependency(syn)
    BusNodeSetter.mapper(controlName = ???, bus = targetBus, node = target.node)
    ???
  }

  def remove(source: AuralAttribute[S])(implicit tx: S#Tx): Unit = {

    ???
  }
}
