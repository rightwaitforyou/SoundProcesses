package de.sciss.synth.proc
package impl

import de.sciss.lucre.synth.Sys

/** An object used in the preparatory phase of playing a process. It allows
  * the addition of asynchronous processors.
  */
final class AsyncProcBuilder[S <: Sys[S]](val obj: Proc[S]) {
  var resources = List.empty[AsyncResource[S]]
}