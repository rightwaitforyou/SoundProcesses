package de.sciss.lucre.expr
package impl

import de.sciss.lucre.stm.{TxnSerializer, Sys}
import de.sciss.lucre.{event => evt, DataInput, DataOutput}
import evt.{Event, EventLike}
import de.sciss.collection.txn
import annotation.switch

object LinkedListImpl {
   import LinkedList.Var

   private def opNotSupported : Nothing = sys.error( "Operation not supported" )

   def newVar[ S <: Sys[ S ], Elem, U ]( eventView: Elem => EventLike[ S, U, Elem ])( implicit tx: S#Tx,
      elemSerializer: TxnSerializer[ S#Tx, S#Acc, Elem ] with evt.Reader[ S, Elem ]) : Var[ S, Elem, U ] = {

      val tgt  = evt.Targets[ S ]
      val sz   = tx.newIntVar( tgt.id, 0 )
      new Impl( tgt, sz, eventView )
   }

   def serializer[ S <: Sys[ S ], Elem, U ]( eventView: Elem => EventLike[ S, U, Elem ])(
      implicit elemSerializer: TxnSerializer[ S#Tx, S#Acc, Elem ] with evt.Reader[ S, Elem ]) :
         evt.NodeSerializer[ S, LinkedList[ S, Elem, U ]] with evt.Reader[ S, LinkedList[ S, Elem, U ]] =
      new Ser[ S, Elem, U ]( eventView )

   private class Ser[ S <: Sys[ S ], Elem, U ]( eventView: Elem => EventLike[ S, U, Elem ])
                                              ( implicit elemSerializer: TxnSerializer[ S#Tx, S#Acc, Elem ] with evt.Reader[ S, Elem ])
   extends evt.NodeSerializer[ S, LinkedList[ S, Elem, U ]] with evt.Reader[ S, LinkedList[ S, Elem, U ]] {
      def read( in: DataInput, access: S#Acc, targets: evt.Targets[ S ])( implicit tx: S#Tx ) : LinkedList[ S, Elem, U ] = {
         LinkedListImpl.read( in, access, targets, eventView )
      }
   }

   private def read[ S <: Sys[ S ], Elem, U ]( in: DataInput, access: S#Acc, targets: evt.Targets[ S ], eventView: Elem => EventLike[ S, U, Elem ])
                                             ( implicit tx: S#Tx, elemSerializer: TxnSerializer[ S#Tx, S#Acc, Elem ] with evt.Reader[ S, Elem ]) : Impl[ S, Elem, U ] = {
      val sz = tx.readIntVar( targets.id, in )
      new Impl( targets, sz, eventView )
   }

   private final class Impl[ S <: Sys[ S ], Elem, U ]( protected val targets: evt.Targets[ S ],
                                                       sizeRef: S#Var[ Int ],
                                                       eventView: Elem => EventLike[ S, U, Elem ])
                                                     ( implicit elemSerializer: TxnSerializer[ S#Tx, S#Acc, Elem ] with evt.Reader[ S, Elem ])
   extends Var[ S, Elem, U ] {
      list =>

      // ---- event behaviour ----

      private object CollectionEvent
      extends evt.Trigger.Impl[ S, LinkedList.Collection[ S, Elem, U ], LinkedList.Collection[ S, Elem, U ], LinkedList[ S, Elem, U ]]
      with evt.EventImpl[ S, LinkedList.Collection[ S, Elem, U ], LinkedList.Collection[ S, Elem, U ], LinkedList[ S, Elem, U ]]
      with evt.InvariantEvent[ S, LinkedList.Collection[ S, Elem, U ], LinkedList[ S, Elem, U ]]
      with evt.Root[ S, LinkedList.Collection[ S, Elem, U ]]
      {
         protected def reader : evt.Reader[ S, LinkedList[ S, Elem, U ]] = serializer( eventView )
         def slot: Int = 1
         def node: evt.Node[ S ] = list
      }

      private object ElementEvent
      extends evt.EventImpl[ S, LinkedList.Element[ S, Elem, U ], LinkedList.Element[ S, Elem, U ], LinkedList[ S, Elem, U ]]
      with evt.InvariantEvent[ S, LinkedList.Element[ S, Elem, U ], LinkedList[ S, Elem, U ]] {
         protected def reader : evt.Reader[ S, LinkedList[ S, Elem, U ]] = serializer( eventView )
         def slot: Int = 2
         def node: evt.Node[ S ] = list

         def connect()( implicit tx: S#Tx ) {}
         def disconnect()( implicit tx: S#Tx ) {}

         def +=( elem: Elem )( implicit tx: S#Tx ) {
            eventView( elem ) ---> this
         }

         def -=( elem: Elem )( implicit tx: S#Tx ) {
            eventView( elem ) -/-> this
         }

         def pullUpdate( pull: evt.Pull[ S ])( implicit tx: S#Tx ) : Option[ LinkedList.Element[ S, Elem, U ]] = {
            sys.error( "TODO" )
//            val changes: IIdxSeq[ (Elem, U)] = pull.parents( this ).flatMap( sel => {
////               val elem = sel.devirtualize( elemReader ).node.asInstanceOf[ Elem ]
////val elem = sel.devirtualize( elemSerializer.asInstanceOf[ evt.Reader[ S, evt.Node[ S ]]]).node.
////   asInstanceOf[ Elem ]
//               val timed = sel.devirtualize( TimedSer /* timedSerializer[ S, Elem, U ]*/).node.asInstanceOf[ TimedElemImpl[ S, Elem, U ]]
//               val ch0 = timed.pullUpdate( pull ).getOrElse( IIdxSeq.empty )
//               ch0.map {
//                  case ch @ BiGroup.Moved( evt.Change( spanValOld, spanValNew )) =>
//                     removeNoFire( spanValOld, timed )
//                     addNoFire(    spanValNew, timed )
//                     timed -> ch
//
//                  case ch => timed -> ch
//               }
//            })( breakOut )
//
//            if( changes.isEmpty ) None else Some( LinkedList.Element( list, changes ))
         }
      }

      private object ChangeEvent
      extends evt.Event[ S, LinkedList.Update[ S, Elem, U ], LinkedList[ S, Elem, U ]]
      with evt.InvariantSelector[ S ] {
         protected def reader : evt.Reader[ S, LinkedList[ S, Elem, U ]] = serializer( eventView )
         def slot: Int = opNotSupported
         def node: evt.Node[ S ] = list

         def connect()( implicit tx: S#Tx ) {}
         def disconnect()( implicit tx: S#Tx ) {}

         private[lucre] def --->( r: evt.Selector[ S ])( implicit tx: S#Tx ) {
            CollectionEvent ---> r
            ElementEvent    ---> r
         }
         private[lucre] def -/->( r: evt.Selector[ S ])( implicit tx: S#Tx ) {
            CollectionEvent -/-> r
            ElementEvent    -/-> r
         }

         private[lucre] def pullUpdate( pull: evt.Pull[ S ])( implicit tx: S#Tx ) : Option[ LinkedList.Update[ S, Elem, U ]] = {
            if(   CollectionEvent.isSource( pull )) CollectionEvent.pullUpdate( pull )
            else if( ElementEvent.isSource( pull )) ElementEvent.pullUpdate(    pull )
            else None
         }

         def react( fun: LinkedList.Update[ S, Elem, U ] => Unit )
                  ( implicit tx: S#Tx ) : evt.Observer[ S, LinkedList.Update[ S, Elem, U ], LinkedList[ S, Elem, U ]] =
            reactTx( _ => fun )

         def reactTx( fun: S#Tx => LinkedList.Update[ S, Elem, U ] => Unit )
                    ( implicit tx: S#Tx ) : evt.Observer[ S, LinkedList.Update[ S, Elem, U ], LinkedList[ S, Elem, U ]] = {
            val obs = evt.Observer( serializer( eventView ), fun )
            obs.add( CollectionEvent )
            obs.add( ElementEvent )
            obs
         }

         private[lucre] def isSource( pull: evt.Pull[ S ]) : Boolean = opNotSupported
      }

      /* private[event] */ def select( slot: Int, invariant: Boolean ) : evt.NodeSelector[ S, _ ] = (slot: @switch) match {
         case 1 => CollectionEvent
         case 2 => ElementEvent
      }

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

      def collectionChanged : Event[ S, LinkedList.Collection[ S, Elem, U ], LinkedList[ S, Elem, U ]] = CollectionEvent
      def elementChanged    : Event[ S, LinkedList.Element[    S, Elem, U ], LinkedList[ S, Elem, U ]] = ElementEvent
      def changed           : Event[ S, LinkedList.Update[     S, Elem, U ], LinkedList[ S, Elem, U ]] = ChangeEvent

      def debugList()( implicit tx: S#Tx ) : List[ Elem ] = iterator.toList
   }
}
