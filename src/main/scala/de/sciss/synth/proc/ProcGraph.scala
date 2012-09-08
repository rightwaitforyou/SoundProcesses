package de.sciss.synth
package proc

import de.sciss.lucre.stm.ImmutableSerializer
import de.sciss.lucre.{DataOutput, DataInput}
import impl.SynthGraphSerializer
import annotation.switch

object ProcGraph {
   private final val SER_VERSION = 1

   implicit object Serializer extends ImmutableSerializer[ ProcGraph ] {
      def write( pg: ProcGraph, out: DataOutput ) {
         out.writeUnsignedByte( SER_VERSION )
         SynthGraphSerializer.write( pg.synthGraph, out )
         pg.scans.foreach { case (key, dir) =>
            out.writeUnsignedByte( dir.id )
            out.writeString( key )
         }
         out.writeUnsignedByte( -1 )
      }

      def read( in: DataInput ) : ProcGraph = {
         val serVer = in.readUnsignedByte()
         require( serVer == SER_VERSION, "Incompatible serialized  (found " + serVer + ", required " + SER_VERSION + ")" )
         val synthGraph = SynthGraphSerializer.read( in )
         var scans = Map.empty[ String, Direction ]
         var id = in.readUnsignedByte()
         while( id >= 0 ) {
            val key = in.readString()
            scans += key -> Direction( id )
            id = in.readUnsignedByte()
         }
         ProcGraph( synthGraph, scans )
      }
   }

   private val builders = new ThreadLocal[ ProcGraphBuilder ] {
      override protected def initialValue = null
   }
   def builder: ProcGraphBuilder = {
      val res = builders.get
      if( res == null ) sys.error( "Calling outside of ProcGraph { } call" )
      res
   }

   def apply( thunk: => Any ) : ProcGraph = {
      val b    = new BuilderImpl
      val old  = builders.get()
      builders.set( b )
      try {
         val sg = SynthGraph( thunk )
         b.build( sg )
      } finally {
         builders.set( old )
      }
   }

   private final class BuilderImpl extends ProcGraphBuilder {
      private var scanMap = Map.empty[ String, Direction ]

      def build( sg: SynthGraph ) : ProcGraph = ProcGraph( sg, scanMap )

      private[proc] def addScan( key: String, direction: ProcGraph.Direction ) {
         val d = scanMap.getOrElse( key, direction ).add( direction )
         scanMap += key -> d
      }
   }

   object Direction {
      def apply( id: Int ) : Direction = (id: @switch) match {
         case In.id => In
         case Out.id => Out
         case Bidi.id => Bidi
         case _ => sys.error( "Invalid Direction id " + id )
      }
   }
   sealed trait Direction {
      def id: Int
      def add( d: Direction ): Direction
   }
   case object In extends Direction {
      final val id = 0
      def add( d: Direction ) : Direction = d match {
         case In     => In
         case Out    => Bidi
         case Bidi   => Bidi
      }
   }
   case object Out extends Direction {
      final val id = 1
      def add( d: Direction ) : Direction = d match {
         case In     => Bidi
         case Out    => Out
         case Bidi   => Bidi
      }
   }
   case object Bidi extends Direction {
      final val id = 2
      def add( d: Direction ) : Direction = this
   }
}
final case class ProcGraph( synthGraph: SynthGraph, scans: Map[ String, ProcGraph.Direction ])

sealed trait ProcGraphBuilder {
   private[proc] def addScan( key: String, direction: ProcGraph.Direction )
}