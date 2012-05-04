package de.sciss.synth.expr

import de.sciss.lucre.expr.Expr
import de.sciss.lucre.event.Event
import de.sciss.collection.txn.{Ordered, HASkipList}
import de.sciss.lucre.stm.{TxnSerializer, Sys}

object Bi {
   def newVar[ S <: Sys[ S ], A ]( init: Expr[ S, A ])( implicit tx: S#Tx,
                                                        peerSer: TxnSerializer[ S#Tx, S#Acc, Expr[ S, A ]]) : Var[ S, A ] =
      new NewVar[ S, A ]( tx, peerSer )

   trait Var[ S <: Sys[ S ], A ] extends Bi[ S, A ] {
      def set( time: Expr[ S, Long ], value: Expr[ S, A ]) : Unit
   }

   private final class NewVar[ S <: Sys[ S ], A ]( tx0: S#Tx,
                                                   peerSer: TxnSerializer[ S#Tx, S#Acc, Expr[ S, A ]])
   extends Var[ S, A ] {
      private val ordered = {
         implicit val tx         = tx0
         implicit val _peerSer   = peerSer
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

      def at( time: Expr[ S, Long ])( implicit tx: S#Tx ) : Expr[ S, A ] = sys.error( "TODO" )

      def changed : Event[ S, (Span, A), Bi[ S, A ]] = sys.error( "TODO" )

      def set( time: Expr[ S, Long ], value: Expr[ S, A ]) {
         sys.error( "TODO" )
      }
   }
}
trait Bi[ S <: Sys[ S ], A ] {
   def get( time: Long )( implicit tx: S#Tx ) : Expr[ S, A ]
   def at( time: Expr[ S, Long ])( implicit tx: S#Tx ) : Expr[ S, A ]
   def changed : Event[ S, (Span, A), Bi[ S, A ]]
}
