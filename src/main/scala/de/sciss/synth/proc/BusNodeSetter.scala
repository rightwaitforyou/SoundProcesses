/*
 *  BusNodeSetter.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2013 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth
package proc

import concurrent.stm.{Ref => ScalaRef}

trait BusNodeSetter extends DynamicBusUser {
  def node: Node
  def controlName: String
}

trait AudioBusNodeSetter extends BusNodeSetter with DynamicAudioBusUser {
  def migrateTo(newBus: RichAudioBus)(implicit tx: Txn): AudioBusNodeSetter
}

trait ControlBusNodeSetter extends BusNodeSetter with DynamicControlBusUser {
  def migrateTo(newBus: RichControlBus)(implicit tx: Txn): ControlBusNodeSetter
}

object BusNodeSetter {
  def reader(controlName: String, bus: RichAudioBus, node: Node): AudioBusNodeSetter =
    new AudioReaderImpl(controlName, bus, node)

  def reader(controlName: String, bus: RichControlBus, node: Node): ControlBusNodeSetter =
    new ControlReaderImpl(controlName, bus, node)

  def writer(controlName: String, bus: RichAudioBus, node: Node): AudioBusNodeSetter =
    new AudioWriterImpl(controlName, bus, node)

  def writer(controlName: String, bus: RichControlBus, node: Node): ControlBusNodeSetter =
    new ControlWriterImpl(controlName, bus, node)

  def readerWriter(controlName: String, bus: RichAudioBus, node: Node): AudioBusNodeSetter =
    new AudioReaderWriterImpl(controlName, bus, node)

  def readerWriter(controlName: String, bus: RichControlBus, node: Node): ControlBusNodeSetter =
    new ControlReaderWriterImpl(controlName, bus, node)

  def mapper(controlName: String, bus: RichAudioBus, node: Node): AudioBusNodeSetter =
    new AudioMapperImpl(controlName, bus, node)

  def mapper(controlName: String, bus: RichControlBus, node: Node): ControlBusNodeSetter =
    new ControlMapperImpl( controlName, bus, node )

  private sealed trait ImplLike extends BusNodeSetter {
    final val added = ScalaRef(initialValue = false)
  }

  private sealed trait AudioSetterLike extends ImplLike {
    final def busChanged(b: AudioBus)(implicit tx: Txn): Unit =
      if (node.isOnline) node.set(audible = true, pairs = controlName -> b.index)
  }

  private sealed trait ControlSetterLike extends ImplLike {
    final def busChanged(b: ControlBus)(implicit tx: Txn): Unit =
      if (node.isOnline) node.set(audible = true, pairs = controlName -> b.index)
  }

  private trait AudioMapperLike extends ImplLike {
    final def busChanged(b: AudioBus)(implicit tx: Txn): Unit =
      node.mapan(true, controlName -> b)
  }

  private sealed trait ControlMapperLike extends ImplLike {
    final def busChanged(b: ControlBus)(implicit tx: Txn): Unit =
      node.mapn(true, controlName -> b)
  }

  private abstract class AbstractAudioImpl
    extends ImplLike with RichAudioBus.User with AudioBusNodeSetter {
    final def migrateTo(newBus: RichAudioBus)(implicit tx: Txn): AudioBusNodeSetter = {
      require(newBus.numChannels == bus.numChannels)
      val wasAdded = added.get(tx.peer)
      if (wasAdded) remove()
      val res = newInstance(newBus)
      if (wasAdded) res.add()
      res
    }

    def newInstance(newBus: RichAudioBus): AudioBusNodeSetter
  }

  private abstract class AbstractControlImpl
    extends ImplLike with RichControlBus.User with ControlBusNodeSetter {
    final def migrateTo(newBus: RichControlBus)(implicit tx: Txn): ControlBusNodeSetter = {
      require(newBus.numChannels == bus.numChannels)
      val wasAdded = added.get(tx.peer)
      if( wasAdded ) remove()
      val res = newInstance(newBus)
      if (wasAdded) res.add()
      res
    }

    def newInstance(newBus: RichControlBus): ControlBusNodeSetter
  }

  private abstract class AbstractAudioReader extends AbstractAudioImpl {
    final def add()(implicit tx: Txn): Unit = {
      val wasAdded = added.swap(true)(tx.peer)
      if (wasAdded) sys.error("Was already added : " + this)
      bus.addReader(this)
    }

    final def remove()(implicit tx: Txn): Unit = {
      val wasAdded = added.swap(false)(tx.peer)
      if (wasAdded) bus.removeReader(this)
    }
  }

  private abstract class AbstractControlReader extends AbstractControlImpl {
    final def add()(implicit tx: Txn): Unit = {
      val wasAdded = added.swap(true)(tx.peer)
      if (wasAdded) sys.error("Was already added : " + this)
      bus.addReader(this)
    }

    final def remove()(implicit tx: Txn): Unit = {
      val wasAdded = added.swap(false)(tx.peer)
      if (wasAdded) bus.removeReader(this)
    }
  }

  /*
   *    Careful not use case classes here, as multiple readers / writers for the
   *    same combo might be wanted in a read / write set!
   */
  private final class AudioReaderImpl(val controlName: String, val bus: RichAudioBus, val node: Node)
    extends AbstractAudioReader with AudioSetterLike {
    def newInstance(newBus: RichAudioBus) = reader(controlName, newBus, node)

    override def toString = "BusNodeSetter.reader(" + controlName + ", " + bus + ", " + node + ")"
  }

  private final class ControlReaderImpl(val controlName: String, val bus: RichControlBus, val node: Node)
    extends AbstractControlReader with ControlSetterLike {
    def newInstance(newBus: RichControlBus) = reader(controlName, newBus, node)

    override def toString = "BusNodeSetter.reader(" + controlName + ", " + bus + ", " + node + ")"
  }

  private final class AudioMapperImpl(val controlName: String, val bus: RichAudioBus, val node: Node)
    extends AbstractAudioReader with AudioMapperLike {
    def newInstance(newBus: RichAudioBus) = mapper(controlName, newBus, node)

    override def toString = "BusNodeSetter.mapper(" + controlName + ", " + bus + ", " + node + ")"
  }

  private final class ControlMapperImpl(val controlName: String, val bus: RichControlBus, val node: Node)
    extends AbstractControlReader with ControlMapperLike {
    def newInstance( newBus: RichControlBus ) = mapper( controlName, newBus, node )

    override def toString = "BusNodeSetter.mapper(" + controlName + ", " + bus + ", " + node + ")"
  }

  /*
   *    Careful not use case classes here, as multiple readers / writers for the
   *    same combo might be wanted in a read / write set!
   */
  private final class AudioWriterImpl(val controlName: String, val bus: RichAudioBus, val node: Node)
    extends AbstractAudioImpl with AudioSetterLike {
    def add()(implicit tx: Txn): Unit = {
      val wasAdded = added.swap(true)(tx.peer)
      if (wasAdded) sys.error("Was already added : " + this)
      bus.addWriter(this)
    }

    def remove()(implicit tx: Txn): Unit = {
      val wasAdded = added.swap(false)(tx.peer)
      if (wasAdded) bus.removeWriter(this)
    }

    def newInstance(newBus: RichAudioBus) = writer(controlName, newBus, node)

    override def toString = "BusNodeSetter.writer(" + controlName + ", " + bus + ", " + node + ")"
  }

  private final class ControlWriterImpl(val controlName: String, val bus: RichControlBus, val node: Node)
    extends AbstractControlImpl with ControlSetterLike {
    def add()(implicit tx: Txn): Unit = {
      val wasAdded = added.swap(true)(tx.peer)
      if (wasAdded) sys.error("Was already added : " + this)
      bus.addWriter(this)
    }

    def remove()(implicit tx: Txn): Unit = {
      val wasAdded = added.swap(false)(tx.peer)
      if (wasAdded) bus.removeWriter(this)
    }

    def newInstance(newBus: RichControlBus) = writer(controlName, newBus, node)

    override def toString = "BusNodeSetter.writer(" + controlName + ", " + bus + ", " + node + ")"
  }

  /*
   *    Careful not use case classes here, as multiple readers / writers for the
   *    same combo might be wanted in a read / write set!
   */
  private final class AudioReaderWriterImpl( val controlName: String, val bus: RichAudioBus, val node: Node )
   extends AbstractAudioImpl with AudioSetterLike {

    object dummy extends RichAudioBus.User {
      def busChanged(b: AudioBus)(implicit tx: Txn) = ()
    }

    def add()(implicit tx: Txn): Unit = {
      val wasAdded = added.swap(true)(tx.peer)
      if (wasAdded) sys.error("Was already added : " + this)
      bus.addReader(this)
      bus.addWriter(dummy)
    }

    def remove()(implicit tx: Txn): Unit = {
      val wasAdded = added.swap(false)(tx.peer)
      if (wasAdded) {
        bus.removeWriter(dummy)
        bus.removeReader(this)
      }
    }

    def newInstance(newBus: RichAudioBus) = readerWriter(controlName, newBus, node)

    override def toString = "BusNodeSetter.readerWriter(" + controlName + ", " + bus + ", " + node + ")"
  }

  private final class ControlReaderWriterImpl(val controlName: String, val bus: RichControlBus, val node: Node)
    extends AbstractControlImpl with ControlSetterLike {

    object dummy extends RichControlBus.User {
      def busChanged(b: ControlBus)(implicit tx: Txn) = ()
    }

    def add()(implicit tx: Txn): Unit = {
      val wasAdded = added.swap(true)(tx.peer)
      if (wasAdded) sys.error("Was already added : " + this)
      bus.addReader(this)
      bus.addWriter(dummy)
    }

    def remove()(implicit tx: Txn): Unit = {
      val wasAdded = added.swap(false)(tx.peer)
      if (wasAdded) {
        bus.removeWriter(dummy)
        bus.removeReader(this)
      }
    }

    def newInstance(newBus: RichControlBus) = readerWriter(controlName, newBus, node)

    override def toString = "BusNodeSetter.readerWriter(" + controlName + ", " + bus + ", " + node + ")"
  }
}