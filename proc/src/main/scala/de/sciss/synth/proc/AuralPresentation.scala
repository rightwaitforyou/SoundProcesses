/*
 *  AuralPresentation.scala
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

package de.sciss.synth.proc

import de.sciss.lucre.stm
import stm.Disposable
import impl.{AuralPresentationImpl => Impl}
import de.sciss.lucre.synth.{Sys, Group}
import de.sciss.synth.{ControlBus => SControlBus}

object AuralPresentation {
  // ---- implementation forwards ----

  //  def run[S <: Sys[S]](transport: ProcTransport[S], aural: AuralSystem): AuralPresentation[S] =
  //    Impl.run[S](transport, aural)

  def run[S <: Sys[S]](transport: ProcTransport[S], aural: AuralSystem, sensor: Option[SensorSystem] = None)
                      (implicit tx: S#Tx): AuralPresentation[S] =
    Impl.run[S](transport, aural, sensor)

  private[proc] trait Running[S <: Sys[S]] {
    /** Queries the number of channel associated with a scanned input.
      * Throws a control throwable when no value can be determined, making
      * the ugen graph builder mark the querying graph element as incomplete
      * (missing information).
      *
      * @param timed        the process whose graph is currently built
      * @param time         the time at which to query the scan
      * @param key          the scan key
      * @param numChannels  a given number of channels if `>= 0`, or `-1` to accept whatever the scan in provides
      *
      * @return             the number of channels for the scan input at the given time
      */
    def scanInNumChannels(timed: TimedProc[S], time: Long, key: String, numChannels: Int)(implicit tx: S#Tx): Int

    /** Queries the number of channels associated with an attribute input.
      * @param timed        the process whose graph is currently built
      * @param key          the attribute key
      *
      * @return             the number of channels for the attribute input
      */
    def attrNumChannels(timed: TimedProc[S], key: String)(implicit tx: S#Tx): Int

    def sensorBus: SControlBus
  }

  final private[proc] case class MissingInfo[S <: Sys[S]](source: TimedProc[S], key: String) extends Throwable
}
trait AuralPresentation[S <: Sys[S]] extends Disposable[S#Tx] {
  def group(implicit tx: S#Tx): Option[Group]

  def stopAll(implicit tx: S#Tx): Unit

  // def sensors(implicit tx: S#Tx): Unit
}
