/*
 *  Implicits.scala
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

import de.sciss.lucre.synth.expr.Strings
import de.sciss.lucre.synth.Sys

object Implicits {
  implicit final class RichProc[S <: Sys[S]](val proc: Proc[S]) extends AnyVal {
    def name(implicit tx: S#Tx): String =
      proc.attributes[Attribute.String](ProcKeys.attrName).fold("<unnamed>")(_.value)

    def name_=(value: String)(implicit tx: S#Tx): Unit = {
      proc.attributes.put(ProcKeys.attrName, Attribute.String(Strings.newConst(value)))
    }
  }
}