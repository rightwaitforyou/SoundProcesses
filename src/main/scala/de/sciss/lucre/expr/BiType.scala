package de.sciss.lucre.expr

import de.sciss.lucre.stm.{Serializer, Sys}
import de.sciss.lucre.{DataInput, event, DataOutput}
import event.{Pull, Targets}

/**
 * Extends `Type` with a an expression form which acts as a cursor on a bi-temporal object.
 */
trait BiType[ A ] extends Type[ A ] {
   private implicit object ValueSer extends Serializer[ A ] {
      def write( v: A, out: DataOutput ) { writeValue( v, out )}
      def read( in: DataInput ) : A = readValue( in )
   }

   def newCursor[ S <: Sys[ S ]]( bi: Bi[ S, A ], time: Expr[ S, Long ])( implicit tx: S#Tx ): Ex[ S ] = {
      val targets = Targets.partial[ S ]
      val init    = bi.value( time.value )
      val cache   = tx.newPartialVar[ A ]( targets.id, init )
      new Cursor[ S ]( targets, cache, bi, time )
   }

   private final class Cursor[ S <: Sys[ S ]]( protected val targets: Targets[ S ], cache: S#Var[ A ],
                                               bi: Bi[ S, A ], time: Expr[ S, Long ])
      extends Expr.Node[ S, A ] {
      def reader: event.Reader[ S, Ex[ S ]] = serializer[ S ]

      protected def writeData( out: DataOutput ) {
         out.writeUnsignedByte( 3 )
         bi.write( out )
         time.write( out )
      }

      def value( implicit tx: S#Tx ): A = bi.get( time.value ).value

      private[lucre] def connect()( implicit tx: S#Tx ) {
         bi.changed   ---> this
         time.changed ---> this
      }

      private[lucre] def disconnect()( implicit tx: S#Tx ) {
         bi.changed   -/-> this
         time.changed -/-> this
      }

      private[lucre] def pullUpdate( pull: Pull[ S ])( implicit tx: S#Tx ): Option[ Change[ S ]] = {
         val biChanged     = bi.changed
         val timeChanged   = time.changed

         val biChange = if( biChanged.isSource( pull )) {
            biChanged.pullUpdate( pull )
         } else {
            None
         }
         val timeChange = if( timeChanged.isSource( pull )) {
            timeChanged.pullUpdate( pull )
         } else {
            None
         }

         (biChange, timeChange) match {
            case (Some( bch ), None) =>
               val before  = cache.get
               val now     = bch._2
               cache.set( now )
               change( before, now )
            case (None, Some( tch )) =>
               val before  = cache.get
//               val before  = bi.value( tch.before )
               val now     = bi.value( tch.now )
               cache.set( now )
               change( before, now )
            case (Some( bch ), Some( tch )) =>
               val before  = cache.get
               val now     = bi.value( tch.now )
               cache.set( now )
               change( before, now )
            case _ => None
         }
      }
   }
}
