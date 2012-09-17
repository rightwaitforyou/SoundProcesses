package de.sciss.synth.proc

import de.sciss.lucre.{data, stm}
import stm.Sys

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

   object Link {
//      implicit def none( unit: Unit ) : Output = ???
      implicit def grapheme[ S <: Sys[ S ]]( link: Grapheme[ S ]) : Link[ S ] = ???
      implicit def scan[     S <: Sys[ S ]]( link: Scan[     S ]) : Link[ S ] = ???
   }
   sealed trait Link[ S ]
}
trait Scan[ S <: Sys[ S ]] {
   import Scan._

   def sinks( implicit tx: S#Tx ) : data.Iterator[ S#Tx, Link[ S ]]
   // for now, links are not in t_p; this is probably fine, because
   // we have graphemes for such a 'declarative' view, and the scan as needle is really
   // more the 'procedural' approach
   def addSink( sink: Link[ S ])( implicit tx: S#Tx ) : Unit
   def removeSink( sink: Link[ S ])( implicit tx: S#Tx ) : Unit

   def source( implicit tx: S#Tx ) : Option[ Link[ S ]]
   def source_=( link: Option[ Link[ S ]])( implicit tx: S#Tx ) : Unit
}
