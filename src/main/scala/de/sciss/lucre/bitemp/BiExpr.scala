package de.sciss.lucre
package bitemp

import expr.Expr
import de.sciss.lucre.{event => evt}
import de.sciss.lucre.event.Targets

object BiExpr {
   def apply[ S <: evt.Sys[ S ], A ]( time: Expr[ S, Long ], mag: Expr[ S, A ])
                                    ( implicit tx: S#Tx, magType: BiType[ A ]) : BiExpr[ S, A ] = {

      (time, mag) match {
         case (Expr.Const( timeVal ), Expr.Const( magVal )) =>
            ConstImpl( timeVal, magVal )
         case _ =>
            val targets = evt.Targets.partial[ S ] // XXX TODO partial?
            new EventImpl( targets, time, mag )
      }
   }

   implicit def serializer[ S <: evt.Sys[ S ], A ]( implicit magType: BiType[ A ]) : evt.Serializer[ S, BiExpr[ S, A ]] =
      new Ser

   private final class Ser[ S <: evt.Sys[ S ], A ]( implicit magType: BiType[ A ])
   extends evt.EventLikeSerializer[ S, BiExpr[ S, A ]] {
      def readConstant( in: DataInput )( implicit tx: S#Tx ) : BiExpr[ S, A ] = {
         val timeVal = in.readLong()
         val magVal  = magType.readValue( in )
         ConstImpl( timeVal, magVal )
      }

      def read( in: DataInput, access: S#Acc, targets: Targets[ S ])
              ( implicit tx: S#Tx ) : BiExpr[ S, A ] with evt.Node[ S ] = {
         val time = magType.longType.readExpr( in, access )
         val mag  = magType.readExpr( in, access )
         new EventImpl( targets, time, mag )
      }
   }

   private final class EventImpl[ S <: evt.Sys[ S ], A ]( protected val targets: evt.Targets[ S ],
                                                          val time: Expr[ S, Long ], val mag: Expr[ S, A ])
                                                        ( implicit magType: BiType[ A ])
   extends BiExpr[ S, A ] with expr.impl.NodeImpl[ S, (Long, A) ] {
      override def toString() = "(" + time + " -> " + mag + ")"

      def value( implicit tx: S#Tx ) : (Long, A) = timeValue -> magValue
      def timeValue( implicit tx: S#Tx ) = time.value
      def magValue(  implicit tx: S#Tx ) = mag.value

      protected def writeData( out: DataOutput ) {
         time.write( out )
         mag.write( out )
      }

      def connect()( implicit tx: S#Tx ) {
         time.changed ---> this
         mag.changed  ---> this
      }

      def disconnect()(implicit tx: S#Tx) {
         time.changed -/-> this
         mag.changed  -/-> this
      }

      def pullUpdate( pull: evt.Pull[ S ])( implicit tx: S#Tx ) : Option[ evt.Change[ (Long, A) ]] = {
         val timeEvt = time.changed
         val timeCh  = if( timeEvt.isSource( pull )) timeEvt.pullUpdate( pull ) else None
         val magEvt  = mag.changed
         val magCh   = if( magEvt.isSource( pull )) magEvt.pullUpdate( pull ) else None

         (timeCh, magCh) match {
            case (Some( tch ), Some( mch )) =>
               Some( tch zip mch )
            case (Some( tch ), None) =>
               val mv = magValue
               Some( evt.Change( tch.before -> mv, tch.now -> mv ))
            case (None, Some( mch )) =>
               val tv = timeValue
               Some( evt.Change( tv -> mch.before, tv -> mch.now ))
            case (None, None) =>
               None
         }
      }

      protected def reader : evt.Reader[ S, Expr[ S, (Long, A) ]] = serializer
   }

   private final case class ConstImpl[ S <: stm.Sys[ S ], A ]( timeVal: Long, magVal: A )( implicit magType: BiType[ A ])
   extends BiExpr[ S, A ] with expr.impl.ConstImpl[ S, (Long, A) ] {
      protected def constValue : (Long, A) = timeVal -> magVal

      def timeValue( implicit tx: S#Tx ) = timeVal
      def magValue(  implicit tx: S#Tx ) = magVal

      protected def writeData( out: DataOutput ) {
         out.writeLong( timeVal )
         magType.writeValue( magVal, out )
      }

      def time: Expr[ S, Long ] = magType.longType.newConst( timeVal )
      def mag:  Expr[ S, A    ] = magType.newConst( magVal )
   }
}
trait BiExpr[ S <: stm.Sys[ S ], A ] extends Expr[ S, (Long, A) ] {
   def time: Expr[ S, Long ]
   def mag:  Expr[ S, A ]
   def timeValue( implicit tx: S#Tx ): Long
   def magValue( implicit tx: S#Tx ): A
}