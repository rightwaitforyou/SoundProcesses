/*
 *  package.scala
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

package de.sciss.synth

import de.sciss.lucre.expr
import java.text.SimpleDateFormat
import java.util.{Date, Locale}
import annotation.elidable
import annotation.elidable.CONFIG
import de.sciss.lucre.event.Sys
import scala.language.higherKinds
import scala.language.existentials

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

  // ---- types ----

  type Folder[S <: Sys[S]] = expr.List.Modifiable[S, Obj[S], Obj.Update[S]]
}