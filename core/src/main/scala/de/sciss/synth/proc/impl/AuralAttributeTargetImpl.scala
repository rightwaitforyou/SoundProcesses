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
    def add   ()(implicit tx: Txn): Unit = server.addEdge   (edge)
    def remove()(implicit tx: Txn): Unit = server.removeEdge(edge)
  }

  private[this] def putSingleScalar(source: AuralAttribute[S], value: AuralAttribute.Scalar)
                                   (implicit tx: S#Tx): State = {
    implicit val itx = tx.peer
    target.node.set(value.toControl(ctlName))
    map.put(source, new Connected(value, Nil)).foreach(_.dispose())
    new SingleScalar(source)
  }

  private[this] def putSingleStream(source: AuralAttribute[S], value: AuralAttribute.Stream)
                                   (implicit tx: S#Tx): State = {
    implicit val itx = tx.peer
    val edge      = NodeRef.Edge(value.source, target)
    val edgeUser  = new AddRemoveEdge(edge)
    val busUser   = BusNodeSetter.mapper(ctlName, value.bus, target.node)
    addUser(edgeUser)
    addUser(busUser)
    map.put(source, new Connected(value, edgeUser :: busUser :: Nil)).foreach(_.dispose())
    new SingleStream(source)
  }

  // ----

  private[this] object Empty extends State {
    def put(source: AuralAttribute[S], value: AuralAttribute.Value)(implicit tx: S#Tx): State =
      value match {
        case sc: AuralAttribute.Scalar => putSingleScalar(source, sc)
        case sc: AuralAttribute.Stream => putSingleStream(source, sc)
      }

    def remove(value: AuralAttribute.Value)(implicit tx: S#Tx): State = this
  }

  // ----

  private[this] final class SingleScalar(aa: AuralAttribute[S]) extends State {
    def put(source: AuralAttribute[S], value: AuralAttribute.Value)(implicit tx: S#Tx): State =
      if (source == aa)
        Empty.put(source,value)
      else
        value match {
          case sc: AuralAttribute.Scalar => ???
          case sc: AuralAttribute.Stream =>
            implicit val itx = tx.peer
            map.transform { case (key1, oldConn) =>
              mkVertex(oldConn.value)
            }
            val newConn = mkVertex(sc)
            ??? // map.put(source)

            ???
        }

    def remove(value: AuralAttribute.Value)(implicit tx: S#Tx): State = Empty
  }

  private[this] def addUser(user: DynamicUser)(implicit tx: S#Tx): Unit = ???

  // ----

  private[this] final class SingleStream(aa: AuralAttribute[S]) extends State {
    def put(source: AuralAttribute[S], value: AuralAttribute.Value)(implicit tx: S#Tx): State =
      if (source == aa)
        Empty.put(source, value)
      else
        value match {
          case sc: AuralAttribute.Scalar => ???
          case sc: AuralAttribute.Stream => ???
        }

    def remove(value: AuralAttribute.Value)(implicit tx: S#Tx): State = ??? // Empty
  }

  // ----

  private[this] def mkVertex(value: AuralAttribute.Value)(implicit tx: S#Tx): Connected = {
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
