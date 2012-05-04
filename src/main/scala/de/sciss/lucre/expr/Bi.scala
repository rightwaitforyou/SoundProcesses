package de.sciss.lucre.expr

import de.sciss.lucre.event.Event
import de.sciss.collection.txn.{Ordered, HASkipList}
import de.sciss.lucre.stm.{Writer, Sys}
import de.sciss.lucre.DataOutput

object Bi {
   def newVar[ S <: Sys[ S ], A ]( init: Expr[ S, A ])( implicit tx: S#Tx,
                                                        peerType: BiType[ A ]) : Var[ S, A ] =
      new NewVar[ S, A ]( tx, peerType )

   trait Var[ S <: Sys[ S ], A ] extends Bi[ S, A ] {
      def set( time: Expr[ S, Long ], value: Expr[ S, A ]) : Unit
   }

   private final class NewVar[ S <: Sys[ S ], A ]( tx0: S#Tx,
                                                   peerType: BiType[ A ])
   extends Var[ S, A ] {
      private val ordered = {
         implicit val tx         = tx0
         implicit val _peerSer   = peerType.serializer[ S ]
         implicit val ord        = Ordering.by[ (Long, Expr[ S, A ]), Long ]( _._1 )
         HASkipList.empty[ S, (Long, Expr[ S, A ])]
      }

      def get( time: Long )( implicit tx: S#Tx ) : Expr[ S, A ] = {
         // XXX TODO should be an efficient method in skiplist itself
         ordered.isomorphicQuery( new Ordered[ S#Tx, (Long, Expr[ S, A ])] {
            def compare( that: (Long, Expr[ S, A ]))( implicit tx: S#Tx ) = {
               val t = that._1
               if( t < time ) -1 else if( t > time ) 1 else 0
            }
         })._1._2
      }

      def value( time: Long )( implicit tx: S#Tx ) : A = get( time ).value

      def write( out: DataOutput ) {
         ordered.write( out )
      }

      def at( time: Expr[ S, Long ])( implicit tx: S#Tx ) : Expr[ S, A ] = peerType.newCursor[ S ]( this, time )

      def changed : Event[ S, (Span, A), Bi[ S, A ]] = sys.error( "TODO" )

      def set( time: Expr[ S, Long ], value: Expr[ S, A ]) {
         sys.error( "TODO" )
      }
   }
}
trait Bi[ S <: Sys[ S ], A ] extends Writer {
   def get( time: Long )( implicit tx: S#Tx ) : Expr[ S, A ]
   def value( time: Long )( implicit tx: S#Tx ) : A
   def at( time: Expr[ S, Long ])( implicit tx: S#Tx ) : Expr[ S, A ]
   def changed : Event[ S, (Span, A), Bi[ S, A ]]
}
