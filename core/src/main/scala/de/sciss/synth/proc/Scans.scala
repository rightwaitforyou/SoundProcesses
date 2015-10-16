/*
 *  Scans.scala
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

import de.sciss.lucre.stm.Sys

object Scans {
  trait Modifiable[S <: Sys[S]] extends Scans[S] {
    /** Adds a new scan by the given key. If a span by that name already exists, the old scan is returned. */
    def add   (key: String)(implicit tx: S#Tx): Scan[S]

    def remove(key: String)(implicit tx: S#Tx): Boolean
  }
}

trait Scans[S <: Sys[S]] {
  def get(key: String)(implicit tx: S#Tx): Option[Scan[S]]

  def keys(implicit tx: S#Tx): Set[String]

  def iterator(implicit tx: S#Tx): Iterator[(String, Scan[S])]
}
