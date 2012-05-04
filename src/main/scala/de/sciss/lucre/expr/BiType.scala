package de.sciss.lucre.expr

import de.sciss.lucre.stm.Sys
import de.sciss.lucre.{event, DataOutput}
import event.{Pull, Targets}

/**
 * Extends `Type` with a an expression form which acts as a cursor on a bi-temporal object.
 */
trait BiType[ A ] extends Type[ A ] {
   def newCursor[ S <: Sys[ S ]]( bi: Bi[ S, A ], time: Expr[ S, Long ])( implicit tx: S#Tx ): Ex[ S ] = {
      val targets = Targets.partial[ S ]
      new Cursor[ S ]( targets, bi, time )
   }

   private final class Cursor[ S <: Sys[ S ]]( protected val targets: Targets[ S ], bi: Bi[ S, A ],
                                               time: Expr[ S, Long ])
      extends Expr.Node[ S, A ] {
      def reader: event.Reader[ S, Ex[ S ]] = serializer[ S ]

      protected def writeData( out: DataOutput ) {
         out.writeUnsignedByte( 3 )
         bi.write( out )
         time.write( out )
      }

      def value( implicit tx: S#Tx ): A = bi.get( time.value ).value

      private[lucre] def connect()( implicit tx: S#Tx ) {
         sys.error( "TODO" )
      }

      private[lucre] def disconnect()( implicit tx: S#Tx ) {
         sys.error( "TODO" )
      }

      private[lucre] def pullUpdate( pull: Pull[ S ])( implicit tx: S#Tx ): Option[ Change[ S ]] = {
         sys.error( "TODO" )
      }
   }

}
