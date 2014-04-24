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

import de.sciss.lucre.{bitemp, event => evt}
import bitemp.BiGroup
import java.text.SimpleDateFormat
import java.util.{Date, Locale}
import annotation.elidable
import annotation.elidable._
import evt.Sys
import de.sciss.lucre.expr.Expr
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.language.higherKinds
import scala.language.existentials


package object proc {
  type ProcGroup    [S <: Sys[S]] = BiGroup[S, Proc[S], Proc.Update[S]]
  type TimedProc    [S <: Sys[S]] = BiGroup.TimedElem[S, Proc[S]]
  type ProcTransport[S <: Sys[S]] = Transport[S, Proc[S], Transport.Proc.Update[S]]  // Proc.Update[ S ]

  type Param = Double

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

  // ---- elements ----

  type El[S <: Sys[S]] = Elem[S, X forSome { type X[~ <: Sys[~]]}]

  def  IntElem    [S <: Sys[S]](peer: Expr[S, Int    ])(implicit tx: S#Tx): IntElem    [S] = ???
  type IntElem    [S <: Sys[S]] = Elem.Expr[S, Int]

  def  DoubleElem [S <: Sys[S]](peer: Expr[S, Double ])(implicit tx: S#Tx): DoubleElem [S] = ???
  type DoubleElem [S <: Sys[S]] = Elem.Expr[S, Double]

  def  LongElem   [S <: Sys[S]](peer: Expr[S, Long   ])(implicit tx: S#Tx): LongElem   [S] = ???
  type LongElem   [S <: Sys[S]] = Elem.Expr[S, Long]

  def  BooleanElem[S <: Sys[S]](peer: Expr[S, Boolean])(implicit tx: S#Tx): BooleanElem[S] = ???
  type BooleanElem[S <: Sys[S]] = Elem.Expr[S, Boolean]

  def  StringElem [S <: Sys[S]](peer: Expr[S, String ])(implicit tx: S#Tx): StringElem [S] = ???
  type StringElem [S <: Sys[S]] = Elem.Expr[S, String]

  def  FadeSpecElem[S <: Sys[S]](peer: Expr[S, FadeSpec.Value])(implicit tx: S#Tx): FadeSpecElem[S] = ???
  type FadeSpecElem[S <: Sys[S]] = Elem.Expr[S, FadeSpec.Value]

  def  DoubleVecElem[S <: Sys[S]](peer: Expr[S, Vec[Double]])(implicit tx: S#Tx): DoubleVecElem[S] = ???
  type DoubleVecElem[S <: Sys[S]] = Elem.Expr[S, Vec[Double]]

  def  AudioGraphemeElem[S <: Sys[S]](peer: Grapheme.Elem.Audio[S])(implicit tx: S#Tx): AudioGraphemeElem[S] = ???
  type AudioGraphemeElem[S <: Sys[S]] = Elem[S, Grapheme.Elem.Audio]

  def  ArtifactLocationElem[S <: Sys[S]](peer: Artifact.Location[S])(implicit tx: S#Tx): ArtifactLocationElem[S] = ???
  type ArtifactLocationElem[S <: Sys[S]] = Elem[S, Artifact.Location]

  def  ProcGroupElem[S <: Sys[S]](peer: ProcGroup[S])(implicit tx: S#Tx): ProcGroupElem[S] = ???
  type ProcGroupElem[S <: Sys[S]] = Elem[S, ProcGroup]

  type Folder[S <: Sys[S]] = Elem[S, Folder.Peer]
}