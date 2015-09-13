/*
 *  package.scala
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

package de.sciss.synth

import java.text.SimpleDateFormat
import java.util.{Date, Locale}

import de.sciss.lucre.expr
import de.sciss.lucre.stm.{Obj, Sys}

import scala.annotation.elidable
import scala.annotation.elidable.CONFIG
import scala.language.{existentials, higherKinds}

package object proc {
  private lazy val logHeader = new SimpleDateFormat("[d MMM yyyy, HH:mm''ss.SSS] 'proc' - ", Locale.US)
  var showLog           = false
  var showAuralLog      = false
  var showTransportLog  = false

  @elidable(CONFIG) private[proc] def logAural(what: => String): Unit =
    if (showAuralLog) Console.out.println(logHeader.format(new Date()) + "aural " + what)

  @elidable(CONFIG) private[proc] def logTransport(what: => String): Unit =
    if (showTransportLog) Console.out.println(logHeader.format(new Date()) + "transport " + what)

  @elidable(CONFIG) private[proc] def log(what: => String): Unit =
    if (showLog) Console.out.println(logHeader.format(new Date()) + what)

//  // ---- types ----
//
//  type Folder[S <: Sys[S]] = expr.List.Modifiable[S, Obj[S]]
}