/*
 *  DynamicBusUser.scala
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

trait DynamicBusUser /* extends Removable */ {
  def add   ()(implicit tx: Txn): Unit
  def remove()(implicit tx: Txn): Unit

  def bus: RichBus
}

trait DynamicAudioBusUser extends DynamicBusUser {
  def bus: RichAudioBus
  def migrateTo(newBus: RichAudioBus)(implicit tx: Txn): DynamicAudioBusUser
}

trait DynamicControlBusUser extends DynamicBusUser {
  def bus: RichControlBus
  def migrateTo(newBus: RichControlBus)(implicit tx: Txn): DynamicControlBusUser
}

object DynamicBusUser {
  def reader(bus: RichAudioBus): DynamicAudioBusUser =
    new AudioReaderImpl(bus)

  def reader(bus: RichControlBus): DynamicControlBusUser =
    new ControlReaderImpl(bus)

  def writer(bus: RichAudioBus): DynamicAudioBusUser =
    new AudioWriterImpl(bus)

  def writer(bus: RichControlBus): DynamicControlBusUser =
    new ControlWriterImpl(bus)

  //   def readerWriter( bus: RichAudioBus ) : DynamicAudioBusUser =
  //      new AudioReaderWriterImpl( bus )

  //   def readerWriter( bus: RichControlBus ) : DynamicControlBusUser =
  //      new ControlReaderWriterImpl( bus )

  private abstract class AbstractAudioImpl extends DynamicAudioBusUser with RichAudioBus.User {
    final val added = ScalaRef(initialValue = false)

    final def busChanged(bus: AudioBus)(implicit tx: Txn) = ()

    final def migrateTo(newBus: RichAudioBus)(implicit tx: Txn): DynamicAudioBusUser = {
      require(newBus.numChannels == bus.numChannels)
      val wasAdded = added.get(tx.peer)
      if (wasAdded) remove()
      val res = newInstance(newBus)
      if (wasAdded) res.add()
      res
    }

    def newInstance(newBus: RichAudioBus): DynamicAudioBusUser
  }

  private final class AudioReaderImpl(val bus: RichAudioBus) extends AbstractAudioImpl {
    def add   ()(implicit tx: Txn): Unit = bus.addReader   (this)
    def remove()(implicit tx: Txn): Unit = bus.removeReader(this)

    def newInstance(newBus: RichAudioBus): DynamicAudioBusUser = reader(newBus)
  }

  private final class AudioWriterImpl(val bus: RichAudioBus) extends AbstractAudioImpl {
    def add   ()(implicit tx: Txn): Unit = bus.addWriter   (this)
    def remove()(implicit tx: Txn): Unit = bus.removeWriter(this)

    def newInstance(newBus: RichAudioBus): DynamicAudioBusUser = writer(newBus)
  }

  private abstract class AbstractControlImpl extends DynamicControlBusUser with RichControlBus.User {
    final val added = ScalaRef(initialValue = false)

    final def busChanged(bus: ControlBus)(implicit tx: Txn) = ()

    final def migrateTo(newBus: RichControlBus)(implicit tx: Txn): DynamicControlBusUser = {
      require(newBus.numChannels == bus.numChannels)
      val wasAdded = added.get(tx.peer)
      if (wasAdded) remove
      val res = newInstance(newBus)
      if (wasAdded) res.add
      res
    }

    def newInstance(newBus: RichControlBus): DynamicControlBusUser
  }

  private final class ControlReaderImpl(val bus: RichControlBus) extends AbstractControlImpl {
    def add   ()(implicit tx: Txn): Unit = bus.addReader   (this)
    def remove()(implicit tx: Txn): Unit = bus.removeReader(this)

    def newInstance(newBus: RichControlBus): DynamicControlBusUser = reader(newBus)
  }

  private final class ControlWriterImpl(val bus: RichControlBus) extends AbstractControlImpl {
    def add   ()(implicit tx: Txn): Unit = bus.addWriter   (this)
    def remove()(implicit tx: Txn): Unit = bus.removeWriter(this)

    def newInstance(newBus: RichControlBus): DynamicControlBusUser = writer(newBus)
  }
}