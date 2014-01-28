/*
 *  Graphemes.scala
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

import de.sciss.lucre.{event => evt}

object Graphemes {
  trait Modifiable[S <: evt.Sys[S]] extends Graphemes[S] {
    def add   (key: String, grapheme: Grapheme[S])(implicit tx: S#Tx): Unit
    def remove(key: String)(implicit tx: S#Tx): Boolean
  }
}

trait Graphemes[S <: evt.Sys[S]] {
  def get(key: String)(implicit tx: S#Tx): Option[Grapheme[S]]
  def keys(implicit tx: S#Tx): Set[String]
}
