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

package de.sciss.lucre.synth

import concurrent.stm.{Ref => ScalaRef}
import de.sciss.synth.{ControlBus => SControlBus, AudioBus => SAudioBus}

trait DynamicBusUser /* extends Removable */ {
  def add   ()(implicit tx: Txn): Unit
  def remove()(implicit tx: Txn): Unit

  def bus: Bus
}

trait DynamicAudioBusUser extends DynamicBusUser {
  def bus: AudioBus
  def migrateTo(newBus: AudioBus)(implicit tx: Txn): DynamicAudioBusUser
}

trait DynamicControlBusUser extends DynamicBusUser {
  def bus: ControlBus
  def migrateTo(newBus: ControlBus)(implicit tx: Txn): DynamicControlBusUser
}

object DynamicBusUser {
  def reader(bus: AudioBus): DynamicAudioBusUser =
    new AudioReaderImpl(bus)

  def reader(bus: ControlBus): DynamicControlBusUser =
    new ControlReaderImpl(bus)

  def writer(bus: AudioBus): DynamicAudioBusUser =
    new AudioWriterImpl(bus)

  def writer(bus: ControlBus): DynamicControlBusUser =
    new ControlWriterImpl(bus)

  //   def readerWriter( bus: AudioBus ) : DynamicAudioBusUser =
  //      new AudioReaderWriterImpl( bus )

  //   def readerWriter( bus: ControlBus ) : DynamicControlBusUser =
  //      new ControlReaderWriterImpl( bus )

  private abstract class AbstractAudioImpl extends DynamicAudioBusUser with AudioBus.User {
    final val added = ScalaRef(initialValue = false)

    final def busChanged(bus: SAudioBus)(implicit tx: Txn) = ()

    final def migrateTo(newBus: AudioBus)(implicit tx: Txn): DynamicAudioBusUser = {
      require(newBus.numChannels == bus.numChannels)
      val wasAdded = added.get(tx.peer)
      if (wasAdded) remove()
      val res = newInstance(newBus)
      if (wasAdded) res.add()
      res
    }

    def newInstance(newBus: AudioBus): DynamicAudioBusUser
  }

  private final class AudioReaderImpl(val bus: AudioBus) extends AbstractAudioImpl {
    def add   ()(implicit tx: Txn): Unit = bus.addReader   (this)
    def remove()(implicit tx: Txn): Unit = bus.removeReader(this)

    def newInstance(newBus: AudioBus): DynamicAudioBusUser = reader(newBus)
  }

  private final class AudioWriterImpl(val bus: AudioBus) extends AbstractAudioImpl {
    def add   ()(implicit tx: Txn): Unit = bus.addWriter   (this)
    def remove()(implicit tx: Txn): Unit = bus.removeWriter(this)

    def newInstance(newBus: AudioBus): DynamicAudioBusUser = writer(newBus)
  }

  private abstract class AbstractControlImpl extends DynamicControlBusUser with ControlBus.User {
    final val added = ScalaRef(initialValue = false)

    final def busChanged(bus: SControlBus)(implicit tx: Txn) = ()

    final def migrateTo(newBus: ControlBus)(implicit tx: Txn): DynamicControlBusUser = {
      require(newBus.numChannels == bus.numChannels)
      val wasAdded = added.get(tx.peer)
      if (wasAdded) remove
      val res = newInstance(newBus)
      if (wasAdded) res.add
      res
    }

    def newInstance(newBus: ControlBus): DynamicControlBusUser
  }

  private final class ControlReaderImpl(val bus: ControlBus) extends AbstractControlImpl {
    def add   ()(implicit tx: Txn): Unit = bus.addReader   (this)
    def remove()(implicit tx: Txn): Unit = bus.removeReader(this)

    def newInstance(newBus: ControlBus): DynamicControlBusUser = reader(newBus)
  }

  private final class ControlWriterImpl(val bus: ControlBus) extends AbstractControlImpl {
    def add   ()(implicit tx: Txn): Unit = bus.addWriter   (this)
    def remove()(implicit tx: Txn): Unit = bus.removeWriter(this)

    def newInstance(newBus: ControlBus): DynamicControlBusUser = writer(newBus)
  }
}