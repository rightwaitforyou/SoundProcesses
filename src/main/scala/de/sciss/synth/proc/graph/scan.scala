package de.sciss.synth
package proc
package graph

import ugen.Out

//object scan {
//
//}
final case class scan( key: String ) {
   def :=( in: GE ) {
      val bus = ("$out_" + key).kr
      Out.ar( bus, in )
      add( ProcGraph.Out )
   }

   private def add( direction: ProcGraph.Direction ) {
      ProcGraph.builder.addScan( key, direction )
   }

   def ar( default: Double ) : GE = {
      val ctl = ("$in_" + key).ar( default )
      add( ProcGraph.In )
      ctl
   }
}
