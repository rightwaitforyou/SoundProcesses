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

  // type El[S <: Sys[S]] = Elem[S, X forSome { type X[~ <: Sys[~]]}]

  def   IntElem    [S <: Sys[S]](peer: Expr[S, Int    ])(implicit tx: S#Tx): IntElem    [S] =
    proc.impl.ElemImpl.Int(peer)
  trait IntElem    [S <: Sys[S]] extends Elem[S] { type Peer = Expr[S, Int] }

  def   DoubleElem [S <: Sys[S]](peer: Expr[S, Double ])(implicit tx: S#Tx): DoubleElem [S] =
    proc.impl.ElemImpl.Double(peer)
  trait DoubleElem [S <: Sys[S]] extends Elem[S] { type Peer = Expr[S, Double] }

  def  LongElem    [S <: Sys[S]](peer: Expr[S, Long   ])(implicit tx: S#Tx): LongElem   [S] =
    proc.impl.ElemImpl.Long(peer)
  trait LongElem   [S <: Sys[S]] extends Elem[S] { type Peer = Expr[S, Long] }

  def  BooleanElem [S <: Sys[S]](peer: Expr[S, Boolean])(implicit tx: S#Tx): BooleanElem[S] =
    proc.impl.ElemImpl.Boolean(peer)
  trait BooleanElem[S <: Sys[S]] extends Elem[S] { type Peer = Expr[S, Boolean] }

  def  StringElem  [S <: Sys[S]](peer: Expr[S, String ])(implicit tx: S#Tx): StringElem [S] =
    proc.impl.ElemImpl.String(peer)
  trait StringElem [S <: Sys[S]] extends Elem[S] { type Peer = Expr[S, String] }

  def  FadeSpecElem [S <: Sys[S]](peer: Expr[S, FadeSpec.Value])(implicit tx: S#Tx): FadeSpecElem[S] =
    proc.impl.ElemImpl.FadeSpec(peer)
  trait FadeSpecElem[S <: Sys[S]] extends Elem[S] { type Peer = Expr[S, FadeSpec.Value] } // FadeSpec.Elem[S]

  def  DoubleVecElem [S <: Sys[S]](peer: Expr[S, Vec[Double]])(implicit tx: S#Tx): DoubleVecElem[S] =
    proc.impl.ElemImpl.DoubleVec(peer)
  trait DoubleVecElem[S <: Sys[S]] extends Elem[S] { type Peer = Expr[S, Vec[Double]] }

  def  AudioGraphemeElem [S <: Sys[S]](peer: Grapheme.Elem.Audio[S])(implicit tx: S#Tx): AudioGraphemeElem[S] =
    proc.impl.ElemImpl.AudioGrapheme(peer)
  trait AudioGraphemeElem[S <: Sys[S]] extends Elem[S] { type Peer = Grapheme.Elem.Audio[S] }

  def  ArtifactLocationElem [S <: Sys[S]](peer: Artifact.Location[S])(implicit tx: S#Tx): ArtifactLocationElem[S] =
    proc.impl.ElemImpl.ArtifactLocation(peer)
  trait ArtifactLocationElem[S <: Sys[S]] extends Elem[S] { type Peer = Artifact.Location[S] }

  def  ProcGroupElem [S <: Sys[S]](peer: ProcGroup[S])(implicit tx: S#Tx): ProcGroupElem[S] =
    proc.impl.ElemImpl.ProcGroup(peer)
  trait ProcGroupElem[S <: Sys[S]] extends Elem[S] { type Peer = ProcGroup[S] }
}