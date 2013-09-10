package de.sciss.synth
package proc
package graph

object Time {
  private[proc] final val key = "$time"

  def ir: GE = Time()
}
/** Absolute time on the canvas, in seconds. */
final case class Time() extends GE.Lazy with ScalarRated {
  protected def makeUGens: UGenInLike = Time.key.ir
}

object Offset {
  private[proc] final val key = "$off"

  def ir: GE = Offset()
}
/** Start time offset within the proc, in seconds. Will be zero if proc is started from the beginning. */
final case class Offset() extends GE.Lazy with ScalarRated {
  protected def makeUGens: UGenInLike = Offset.key.ir
}

object Duration {
  private[proc] final val key = "$dur"

  def ir: GE = Duration()
}

/** Total duration of proc in seconds. If proc was started midway through, this is still its total
  * length. To gather for how long it's going to play, use `Duration() - Offset()`.
  */
final case class Duration() extends GE.Lazy with ScalarRated {
  protected def makeUGens: UGenInLike = Duration.key.ir
}
