package de.sciss.synth
package proc
package graph

object scan {
   private[proc] sealed trait Elem { def key: String }

   private final case class In( key: String, default: Double )
   extends GE.Lazy with Elem with AudioRated {
      def displayName = "scan.In"

      override def toString = displayName + "(\"" + key + "\")"

      def makeUGens: UGenInLike = {
         val ctl = ("$in_" + key).ar( default )
         ctl.expand
      }
   }

//   private final case class Out( key: String, signal: GE )
//   extends UGenSource.ZeroOut( "scan.Out" ) with Elem with WritesBus {
////      def displayName = "scan.Out"
//
//      override def toString = displayName + "(\"" + key + "\")"
//
//      protected def makeUGen(args: IndexedSeq[ UGenIn ]) {}
//
//      protected def makeUGens {}
//   }

   private final case class Out( key: String, in: GE )
   extends Lazy.Expander[ Unit ] with Elem with WritesBus {
//      def displayName = "scan.Out"

      override def toString = "scan.Out(\"" + key + "\")"

      protected def makeUGens {
         val bus = ("$out_" + key).kr
         ugen.Out.ar( bus, in )
      }
   }
}
final case class scan( key: String ) {
//   private def add( direction: ProcGraph.Direction ) {
//      ProcGraph.builder.addScan( key, direction )
//   }

   def ar( default: Double ) : GE = scan.In( key, default )

//   def ar( default: Double ) : GE = {
//      val ctl = ("$in_" + key).ar( default )
//      add( ProcGraph.In )
//      ctl
//   }

   def :=( in: GE ) {
      scan.Out( key, in )
   }

//   def :=( in: GE ) {
//      val bus = ("$out_" + key).kr
//      Out.ar( bus, in )
//      add( ProcGraph.Out )
//   }
}