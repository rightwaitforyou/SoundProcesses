package de.sciss.synth.proc

import de.sciss.lucre.stm.Sys

object Scan {
//   /**
//    * A real-time signal produced by another process
//    *
//    * @param timed   the source process of the signal
//    * @param key     the scan key in the source process
//    */
//   final case class Sink[ S <: Sys[ S ]]( timed: TimedProc[ S ], key: String )
//   extends Value[ S ]
//
//   /**
//    * The real-time signal produced (output) by this process
//    */
//   case object Source extends Value[ Nothing ]
}
trait Scan[ S <: Sys[ S ]] {

}
