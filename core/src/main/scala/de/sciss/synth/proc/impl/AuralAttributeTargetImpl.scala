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

import de.sciss.lucre.stm.TxnLike
import de.sciss.lucre.synth.{AudioBus, BusNodeSetter, DynamicUser, NodeRef, Resource, Synth, Txn}
import de.sciss.synth
import de.sciss.synth.{ControlSet, SynthGraph}
import de.sciss.synth.proc.AuralAttribute.{Instance, Scalar, Stream, Value}

import scala.concurrent.stm.{Ref, TMap, TSet}

class AuralAttributeTargetImpl(target: NodeRef.Full, key: String, targetBus: AudioBus)
  extends AuralAttribute.Target with DynamicUser {

  import TxnLike.peer

  import targetBus.{numChannels, server}

  private def ctlName   = graph.Attribute.controlName(key)

  private val map       = TMap.empty[Instance, Connected]
  private val stateRef  = Ref[State](Empty)
  private val instances = TSet.empty[Instance]

  private final class Connected(val value: Value,
                                val users: List[DynamicUser], val resources: List[Resource])
    extends DynamicUser {

    def attach()(implicit tx: Txn): this.type = {
      if (users    .nonEmpty) target.addUser(this)
      if (resources.nonEmpty) resources.foreach(target.addResource)
      this
    }

    def add()(implicit tx: Txn): Unit = users.foreach(_.add())

    def remove()(implicit tx: Txn): Unit = {
      if (resources.nonEmpty) resources.foreach(target.removeResource)
      if (users.nonEmpty) {
        target.removeUser(this)
        users.foreach(_.remove())
      }
    }

    override def toString = s"Connected($value, $users)"
  }

  private sealed trait State {
    def put   (instance: Instance, value: Value)(implicit tx: Txn): State
    def remove(instance: Instance)(implicit tx: Txn): State
  }

  private final class AddRemoveEdge(edge: NodeRef.Edge) extends DynamicUser {
    def add   ()(implicit tx: Txn): Unit = server.addEdge   (edge)
    def remove()(implicit tx: Txn): Unit = server.removeEdge(edge)

    override def toString = s"AddRemoveEdge($edge)"
  }

  private final class AddRemoveVertex(vertex: NodeRef) extends DynamicUser {
    def add   ()(implicit tx: Txn): Unit = server.addVertex(vertex)
    def remove()(implicit tx: Txn): Unit = {
      server.removeVertex(vertex)
      // node.free()
    }

    override def toString = s"AddRemoveVertex($vertex)"
  }

  private def putSingleScalar(instance: Instance, value: Scalar)
                                   (implicit tx: Txn): State = {
    val ctlSet = value.toControl(ctlName, numChannels = numChannels)
    // target.node.set(ctlSet)
    target.addControl(ctlSet)
    val cc = new Connected(value, Nil, Nil)
    map.put(instance, cc).foreach(_.dispose())
    cc.attach()
    new Single(instance, cc)
  }

  private def putSingleStream(instance: Instance, value: Stream)
                             (implicit tx: Txn): State = {
    val edge      = NodeRef.Edge(value.source, target)
    val edgeUser  = new AddRemoveEdge(edge)
    val busUser   = BusNodeSetter.mapper(ctlName, value.bus, target.node)
    val cc        = new Connected(value, edgeUser :: busUser :: Nil, Nil)
    map.put(instance, cc).foreach(_.dispose())
    cc.attach()
    new Single(instance, cc)
  }

  // ----

  private object Empty extends State {
    def put(instance: Instance, value: Value)(implicit tx: Txn): State =
      value match {
        case sc: Scalar => putSingleScalar(instance, sc)
        case sc: Stream => putSingleStream(instance, sc)
      }

    def remove(instance: Instance)(implicit tx: Txn): State =
      throw new NoSuchElementException(instance.toString)

    override def toString = "Empty"
  }

  // ----

  private final class Single(instance1: Instance, con1: Connected) extends State {
    def put(instance: Instance, value: Value)(implicit tx: Txn): State =
      if (instance == instance1) {
        Empty.put(instance, value)
      } else {
        con1.dispose()
        val tgtBusUser = BusNodeSetter.mapper(ctlName, targetBus, target.node)
        target.addUser(tgtBusUser)
        val c1 = mkVertex(con1.value)
        val c2 = mkVertex(     value)
        map.put(instance1, c1)
        map.put(instance , c2)
        new Multiple(tgtBusUser)
      }

    def remove(instance: Instance)(implicit tx: Txn): State = {
      map.remove(instance).fold(throw new NoSuchElementException(instance.toString))(_.dispose())
      if (!con1.value.isScalar) {
        // We had a `mapan` for which the bus input is now gone.
        // Right now, `ButNodeSetter.mapper` does _not_ undo the
        // mapping if you remove it. XXX TODO -- I don't know if it should...
        // Therefore, to avoid glitches from stale bus contents,
        // we must explicitly set the control to some value (i.e. zero)
        val ctlSet: ControlSet =
          if (numChannels == 1) ctlName -> 0f
          else                  ctlName -> Vector.fill(numChannels)(0f)
        target.addControl(ctlSet)
      }
      Empty
    }

    override def toString = s"Single($instance1, $con1)"
  }

  // ----

  private final class Multiple(tgtBusUser: DynamicUser) extends State {
    def put(instance: Instance, value: Value)(implicit tx: Txn): State = {
      val con = mkVertex(value)
      map.put(instance, con).foreach(_.dispose())
      this
    }

    def remove(instance: Instance)(implicit tx: Txn): State = {
      map.remove(instance).fold(throw new NoSuchElementException(instance.toString))(_.dispose())
      map.size match {
        case 1 =>
          val (aa, cc) = map.head
          target.removeUser(tgtBusUser)
          tgtBusUser.dispose()  // XXX TODO --- not sure this should be done by `removeUser` automatically
          cc.dispose()
          Empty.put(aa, cc.value)
        case x =>
          assert(x > 2)
          this
      }
    }

    override def toString = "Multiple"
  }
  
  // ----

  private def mkVertex(value: Value)(implicit tx: Txn): Connected = {
    def make(syn: Synth, users0: List[DynamicUser]): Connected = {
      val vertexUser  = new AddRemoveVertex(syn)
      val outEdge     = NodeRef.Edge(syn, target)
      val outEdgeUser = new AddRemoveEdge(outEdge)
      val outBusUser  = BusNodeSetter.writer("out", targetBus, syn)
      val users       = vertexUser :: outEdgeUser :: outBusUser :: users0

      val cc = new Connected(value, users = users, resources = syn :: Nil)
      cc.attach()
    }

    value match {
      case sc: Scalar =>
        val g = SynthGraph {
          import synth._
          import ugen._
          val in = "in".kr(Vector.fill(numChannels)(0f))
          Out.ar("out".kr, in)
        }
        val values0     = sc.values
        val inChannels  = values0.length
        val syn         = Synth.play(g, nameHint = Some("attr-set"))(target = server,
          args = List("in" -> Vector.tabulate[Float](numChannels)(i => values0(i % inChannels))))
        make(syn, Nil)

      case sc: Stream =>
        // - basically the same synth (mapped control in, bus out)
        // - .reader/.mapper source-bus; .write targetBus
        // - add both vertices, add both edges
        val inChannels  = sc.bus.numChannels
        val g = SynthGraph {
          import synth._
          import ugen._
          val in  = "in".ar(Vector.fill(inChannels)(0f)) // In.ar("in".kr, inChannels)
          val ext = Vector.tabulate(numChannels)(in \ _)
          Out.ar("out".kr, ext)
        }
        val syn         = Synth.play(g, nameHint = Some("attr-map"))(
          target = server, dependencies = sc.source.node :: Nil)
        val inEdge      = NodeRef.Edge(sc.source, syn)
        val inEdgeUser  = new AddRemoveEdge(inEdge)
        val inBusUser   = BusNodeSetter.mapper("in" , sc.bus   , syn)
        val users0      = inEdgeUser :: inBusUser :: Nil
        make(syn, users0)
    }
  }

  // def dispose()(implicit tx: Txn): Unit = ...

  // this is a no-op, we just use removal
  def add()(implicit tx: Txn): Unit = ()

  def remove()(implicit tx: Txn): Unit = {
    instances.foreach(_.dispose())
  }

  def put(instance: Instance, value: Value)(implicit tx: Txn): Unit =
    stateRef.transform(_.put(instance, value))(tx.peer)

  def remove(instance: Instance)(implicit tx: Txn): Unit = {
    stateRef.transform(_.remove(instance))(tx.peer)
    instances.remove(instance)
    // if (!instances.remove(instance)) throw new IllegalStateException(s"Instance $instance was not added")
  }

  def add(instance: Instance)(implicit tx: Txn): Unit =
    if (!instances.add(instance)) throw new IllegalStateException(s"Instance $instance was already added")
}