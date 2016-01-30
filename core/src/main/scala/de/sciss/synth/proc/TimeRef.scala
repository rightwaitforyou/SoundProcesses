/*
 *  TimeRef.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2016 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc

import de.sciss.span.{SpanLike, Span}

object TimeRef {
  /** `Undefined` with general type */
  def undefined: Option = Undefined

  final val SampleRate = 14112000.0 // lcm(88.2k, 96k); note: value is copied in AuralContextImpl

  /** Utility method that generates a string representation of
    * the time in seconds of a given frame index.
    */
  def framesToSecs(n: Long): String = if (n == Long.MinValue) "-inf" else if (n == Long.MaxValue) "inf" else {
    val s = n / SampleRate
    f"$s%1.3f"
  }

  def spanToSecs(span: SpanLike): String = span match {
    case Span.Void => "(void)"
    case Span.All => "(all)"
    case s: Span.Bounded =>
      val start = s match {
        case hs: Span.HasStart => framesToSecs(hs.start)
        case _ => "-inf"
      }
      val stop = s match {
        case hs: Span.HasStop => framesToSecs(hs.stop)
        case _ => "inf"
      }
      s"(${start}s - ${stop}s)"
  }

  case object Undefined extends Option {
    /** For convenience, an undefined time references reports a frame of zero. */
    val frame         = 0L
    /** The span is of an undefined time reference is void. */
    val span          = Span.Void
    val offset        = 0L
    val isDefined     = false

    def force         = new TimeRef(Span.From(0L), 0L)
  }

  /** A time reference specifies the temporal context
    * within which an aural object is invoked. It may
    * be either undefined (there is no notion of
    * pointers in virtual performance time), or
    * defined by a span for the invoked object and
    * a time frame corresponding to the current position.
    */
  sealed trait Option {
    /** The overall played span. */
    def span : SpanLike

    /** `true` for `TimeRef.Apply`, `false` for `TimeRef.Undefined`. */
    def isDefined: Boolean

    /** Offsets within the logical span of the object.
      * For an undefined time reference, this will report zero,
      * otherwise it is equal to `frame - span.start`. If the
      * span does not have a defined start, it will also report zero.
      */
    def offset: Long

    /** If the reference is undefined, translates it into a defined one beginning at zero
      * with a span from zero. Otherwise returns the reference unmodified.
      */
    def force: TimeRef
  }
}

final case class TimeRef(span: Span.HasStart, frame: Long) extends TimeRef.Option {
  /** The relative offset of the frame to the span's start in frames. */
  def offset: Long = frame - span.start

  def isDefined = true
  def force     = this

  def shift(deltaFrames: Long): TimeRef = {
    val span1 = span.shift(deltaFrames)
    new TimeRef(span1, frame + deltaFrames)
  }

  def updateOffset(newOffset: Long): TimeRef = new TimeRef(span, frame = span.start + offset)

  def child(that: SpanLike): TimeRef.Option = {
    val spanZ = span.shift(-span.start)
    val span1 = spanZ.intersect(that)
    span1 match {
      case s: Span.HasStart => new TimeRef(s, frame)
      case _                => TimeRef.Undefined
    }
  }

  import TimeRef.{spanToSecs, framesToSecs}

  override def toString = s"TimeRef(span = $span / ${spanToSecs(span)}, frame = $frame / ${framesToSecs(frame)})"
}