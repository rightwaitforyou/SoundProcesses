package de.sciss.lucre.expr
package impl

import de.sciss.lucre.stm.{TxnSerializer, Sys}
import de.sciss.lucre.{event => evt, DataOutput}
import evt.{Event, EventLike}
import de.sciss.collection.txn

object LinkedListImpl {
   import LinkedList.Var

   def newVar[ S <: Sys[ S ], Elem, U ]( eventView: Elem => EventLike[ S, U, Elem ])
                                       ( implicit tx: S#Tx, elemSerializer: TxnSerializer[ S#Tx, S#Acc, Elem ]) : Var[ S, Elem, U ] = {
      val tgt  = evt.Targets[ S ]
      val sz   = tx.newIntVar( tgt.id, 0 )
      new Impl( tgt, sz, eventView )
   }

   private final class Impl[ S <: Sys[ S ], Elem, U ]( protected val targets: evt.Targets[ S ],
                                                       sizeRef: S#Var[ Int ],
                                                       eventView: Elem => EventLike[ S, U, Elem ])
                                                     ( implicit elemSerializer: TxnSerializer[ S#Tx, S#Acc, Elem ])
   extends Var[ S, Elem, U ] {

      /* private[event] */ def select( slot: Int, invariant: Boolean ) : evt.NodeSelector[ S, _ ] = sys.error( "TODO" )

      def addLast( elem: Elem )( implicit tx: S#Tx ) {
         sys.error( "TODO" )
      }

      def addHead( elem: Elem )( implicit tx: S#Tx ) {
         sys.error( "TODO" )
      }

      def remove( elem: Elem )( implicit tx: S#Tx ) : Boolean = sys.error( "TODO" )

      def removeLast()( implicit tx: S#Tx ) {
         sys.error( "TODO" )
      }

      def removeHead()( implicit tx: S#Tx ) {
         sys.error( "TODO" )
      }

      def clear()( implicit tx: S#Tx ) {
         sys.error( "TODO" )
      }

      protected def disposeData()( implicit tx: S#Tx ) {
         sys.error( "TODO" )
         sizeRef.dispose()
      }

      protected def writeData( out: DataOutput ) {
         sys.error( "TODO" )
         sizeRef.write( out )
      }

      def isEmpty( implicit tx: S#Tx ) : Boolean = size == 0
      def nonEmpty( implicit tx: S#Tx ) : Boolean = size > 0
      def size( implicit tx: S#Tx ) : Int = sizeRef.get

      def headOption( implicit tx: S#Tx ) : Option[ Elem ] = sys.error( "TODO" )

      def lastOption( implicit tx: S#Tx ) : Option[ Elem ] = sys.error( "TODO" )

      def head( implicit tx: S#Tx ) : Elem = sys.error( "TODO" )

      def last( implicit tx: S#Tx ) : Elem = sys.error( "TODO" )

      def iterator( implicit tx: S#Tx ) : txn.Iterator[ S#Tx, Elem ] = sys.error( "TODO" )

      def collectionChanged : Event[ S, LinkedList.Collection[ S, Elem, U ], LinkedList[ S, Elem, U ]] = sys.error( "TODO" )
      def elementChanged    : Event[ S, LinkedList.Element[    S, Elem, U ], LinkedList[ S, Elem, U ]] = sys.error( "TODO" )
      def changed           : Event[ S, LinkedList.Update[     S, Elem, U ], LinkedList[ S, Elem, U ]] = sys.error( "TODO" )

      def debugList()( implicit tx: S#Tx ) : List[ Elem ] = iterator.toList
   }
}
