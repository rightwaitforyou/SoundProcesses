package de.sciss.synth.proc

import de.sciss.lucre.stm.{Serializer, Sys}
import de.sciss.lucre.expr.Expr
import de.sciss.synth.{cubShape, sqrShape, welchShape, sinShape, expShape, linShape, stepShape, curveShape, Env}
import de.sciss.lucre.bitemp.BiPin
import de.sciss.synth.expr.{Longs, Doubles}
import de.sciss.lucre.{Writable, DataOutput, DataInput, event => evt}
import evt.{EventLikeSerializer, Event, EventLike}
import annotation.switch

object Scan_ {
//   private final val stepShapeID    = stepShape.id
//   private final val linShapeID     = linShape.id
//   private final val expShapeID     = expShape.id
//   private final val sinShapeID     = sinShape.id
//   private final val welchShapeID   = welchShape.id
//   private final val curveShapeID   = curveShape( 0f ).id  // grmpfff...
//   private final val sqrShapeID     = sqrShape.id
//   private final val cubShapeID     = cubShape.id

   object Elem {
      sealed trait Update[ S <: Sys[ S ]]

      implicit def serializer[ S <: Sys[ S ]] : EventLikeSerializer[ S, Elem[ S ]] = anySer.asInstanceOf[ Ser[ S ]]

      private val anySer = new Ser[ I ]

      private final class Ser[ S <: Sys[ S ]] extends EventLikeSerializer[ S, Elem[ S ]] {
//         def write( elem: Elem[ S ], out: DataOutput ) { elem.write( out )}

         private def readShape( in: DataInput ) : Env.ConstShape = {
            (in.readInt(): @switch) match {
               case stepShape.id    => stepShape
               case linShape.id     => linShape
               case expShape.id     => expShape
               case sinShape.id     => sinShape
               case welchShape.id   => welchShape
               case curveShape.id   => curveShape( in.readFloat() )
               case sqrShape.id     => sqrShape
               case cubShape.id     => cubShape
            }
         }

         def readConstant( in: DataInput )( implicit tx: S#Tx ) : Elem[ S ] = {
            (in.readUnsignedByte(): @switch) match {
               case 0 =>
                  require( in.readUnsignedByte() == 3, "Expected constant Expr" )   // XXX bad...
                  val targetLevel   = Doubles.serializer[ S ].readConstant( in )
                  val shape         = readShape( in )
                  Mono( targetLevel, shape )

               case 1 =>
                  synthesis[ S ]

               case 2 =>
                  sys.error( "TODO" )
            }
            sys.error( "TODO" )
         }

         def read( in: DataInput, access: S#Acc, targets: evt.Targets[ S ])( implicit tx: S#Tx ) : Elem[ S ] = {
            sys.error( "TODO" )
         }
      }
   }
   sealed trait Elem[ S <: Sys[ S ]] extends Writable {
      def changed: EventLike[ S, Elem.Update[ S ], Elem[ S ]] = sys.error( "TODO" )
   }
   object Mono {
      def apply[ S <: Sys[ S ]]( targetLevel: Expr[ S, Double ], shape: Env.ConstShape )( implicit tx: S#Tx ) : Mono[ S ] = {
         if( targetLevel.isInstanceOf[ Expr.Const[ _, _ ]]) {
            Const( targetLevel, shape )
         } else {
            val tgt = evt.Targets.partial[ S ]
            new Mut( tgt, targetLevel, shape )
         }
      }

      def unapply[ S <: Sys[ S ]]( elem: Elem[ S ]) : Option[ (Expr[ S, Double ], Env.ConstShape) ] = {
         if( elem.isInstanceOf[ Mono[ _ ]]) {
            val mono = elem.asInstanceOf[ Mono[ S ]]
            Some( mono.targetLevel -> mono.shape )
         } else None
      }

      private final case class Const[ S <: Sys[ S ]]( targetLevel: Expr[ S, Double ], shape: Env.ConstShape )
      extends Mono[ S ] with evt.Constant[ S ] {
         override def toString = "Mono(" + targetLevel + ", " + shape + ")"
      }

      private final class Mut[ S <: Sys[ S ]]( protected val targets: evt.Targets[ S ],
                                               val targetLevel: Expr[ S, Double ], val shape: Env.ConstShape )
      extends Mono[ S ] with evt.StandaloneLike[ S, Elem.Update[ S ], Elem[ S ]] {
         override def toString = "Mono(" + targetLevel + ", " + shape + ")"

         def reader: evt.Reader[ S, Elem[ S ]] = Elem.serializer[ S ]

         def connect()( implicit tx: S#Tx ) {
            evt.Intruder.--->( targetLevel.changed, this )
         }

         def disconnect()( implicit tx: S#Tx ) {
            evt.Intruder.-/->( targetLevel.changed, this )
         }

         protected def disposeData()( implicit tx: S#Tx ) {}

         def pullUpdate( pull: evt.Pull[ S ])( implicit tx: S#Tx ) : Option[ Elem.Update[ S ]] = {
            // XXX TODO ugly. Should have object Event { def unapply( ... )}
            evt.Intruder.pullUpdate( targetLevel.changed.asInstanceOf[ evt.NodeSelector[ S, evt.Change[ Double ]]], pull ).map( u => sys.error( "TODO" ))
         }
      }
   }
   sealed trait Mono[ S <: Sys[ S ]] extends Elem[ S ] {
      def targetLevel: Expr[ S, Double ]
      def shape: Env.ConstShape

      final protected def writeData( out: DataOutput ) {
         out.writeUnsignedByte( 0 )
         targetLevel.write( out )
         out.writeInt( shape.id )
         shape match {
            case cs: curveShape => out.writeFloat( cs.curvature )
            case _ =>
         }
      }
   }
//   final case class AudioFile[ S <: Sys[ S ]]( f: File, offset: Expr[ S, Long ]) extends Elem[ S ]
//   final case class Graph[ S <: Sys[ S ]]( func: Expr[ S, SynthGraph ]) extends Elem[ S ]

   private val anySynthesis = Synthesis[ I ]()

   private def synthesis[ S <: Sys[ S ]] : Synthesis[ S ] = anySynthesis.asInstanceOf[ Synthesis[ S ]]

   final case class Synthesis[ S <: Sys[ S ]]() extends Elem[ S ] with evt.Constant[ S ] {
      protected def writeData( out: DataOutput ) {
         out.writeUnsignedByte( 1 )
      }
   }
   final case class Embedded[ S <: Sys[ S ]]( ref: Scan[ S ], offset: Expr[ S, Long ]) extends Elem[ S ] {
      def write( out: DataOutput ) {
         out.writeUnsignedByte( 2 )
         ref.write( out )
      }
   }

//   type Modifiable[ S <: Sys[ S ]] = BiPin.Expr.Modifiable[ S, Elem[ S ]]
   type Modifiable[ S <: Sys[ S ]] = BiPin.Modifiable[ S, Elem[ S ], Elem.Update[ S ]]

   object Modifiable {
      def apply[ S <: Sys[ S ]]( implicit tx: S#Tx ) : Modifiable[ S ] = {
         implicit val time = Longs
         BiPin.Modifiable( _.changed ) // ( tx, Elem.serializer[ S ], Longs )
      }

      /**
       * Extractor to check if a `Scan` is actually a `Scan.Modifiable`
       */
      def unapply[ S <: Sys[ S ]]( v: Scan[ S ]) : Option[ Modifiable[ S ]] = {
//         if( v.isInstanceOf[ Modifiable[ _ ]]) Some( v.asInstanceOf[ Modifiable[ S ]]) else None
         if( v.isInstanceOf[ BiPin.Modifiable[ _ , _ , _ ]]) Some( v.asInstanceOf[ Modifiable[ S ]]) else None
      }
   }

//   def Elems[ S <: Sys[ S ]] : BiType[ Elem[ S ]] = anyElems.asInstanceOf[ BiType[ Elem[ S ]]]
//
//   private val anyElems = new ElemsImpl[ I ]
//
//   private final class ElemsImpl[ S <: Sys[ S ]] extends BiTypeImpl[ Elem[ S ]] {
//      private val typeID = 1000
//
//      /* protected */ def readValue( in: DataInput ) : Elem[ S ] = ??? // SpanLike.read( in )
//      /* protected */ def writeValue( value: Elem[ S ], out: DataOutput ) { ??? } // value.write( out )}
//
//      def readTuple[ S1 <: Sys[ S1 ]]( cookie: Int, in: DataInput, access: S1#Acc, targets: evt.Targets[ S1 ])( implicit tx: S1#Tx ) : Ex[ S1 ] =
//         (cookie /*: @switch */) match {
//            case _ => sys.error( "Invalid cookie " + cookie )
//         }
//   }
}