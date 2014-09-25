/*
 *  TransportView.scala
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
package gui

import impl.{TransportViewImpl => Impl}
import de.sciss.lucre.synth.Sys
import de.sciss.lucre.swing.View
import de.sciss.lucre.stm
import de.sciss.audiowidgets.TimelineModel

object TransportView {
  def apply[S <: Sys[S]](transport: Transport[S] /*.Realtime[S, Obj.T[S, Proc.Elem], Transport.Proc.Update[S]] */,
                         timelineModel: TimelineModel, hasMillis: Boolean, hasLoop: Boolean)
                        (implicit tx: S#Tx, cursor: stm.Cursor[S]): TransportView[S] =
    Impl[S](transport, timelineModel, hasMillis = hasMillis, hasLoop = hasLoop)
}

trait TransportView[S <: Sys[S]] extends View[S] {
  def transport /* ( implicit tx: S#Tx ) */ : /* Workspace.*/ Transport[S]
  def timelineModel: TimelineModel
}