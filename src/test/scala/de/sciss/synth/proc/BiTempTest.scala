package de.sciss.synth.proc

import de.sciss.confluent.Confluent
import de.sciss.lucre.stm.impl.BerkeleyDB
import java.io.File
import de.sciss.lucre.event.Change
import de.sciss.lucre.expr.{Expr, BiType, Bi}
import collection.immutable.{IndexedSeq => IIdxSeq}
import de.sciss.synth.expr.{Longs, Doubles, ExprImplicits}
import de.sciss.lucre.stm.{TxnSerializer, Cursor, Sys}
import de.sciss.lucre.{DataInput, DataOutput}

object BiTempTest extends App {
   {
      val dir        = File.createTempFile( "database", "db" )
      dir.delete()
      val store      = BerkeleyDB.factory( dir )
      implicit val s = Confluent( store )
      run[ Confluent ]
   }

   def run[ S <: Sys[ S ]]( implicit system: S, cursor: Cursor[ S ]) {
      val exprImp = new ExprImplicits[ S ]
      import exprImp._

      implicit def biSer[ A ]( implicit peer: BiType[ A ]) = Bi.serializer[ S, A ]
      implicit val doubles = Doubles
      implicit val longVarSer: TxnSerializer[ S#Tx, S#Acc, Expr.Var[ S, Long ]] = new TxnSerializer[ S#Tx, S#Acc, Expr.Var[ S, Long ]] {
         def write( v: Expr.Var[ S, Long ], out: DataOutput ) { v.write( out )}
         def read( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : Expr.Var[ S, Long ] = Longs.readVar( in, access )
      }

//      implicit val accessSer = implicitly[ TxnSerializer[ S#Tx, S#Acc, (Bi.Var[ S, Double ], IIdxSeq[ Expr.Var[ S, Long ]])]]

      println( "__STEP__ root" )
      val access = system.root { implicit tx =>
         val bi = Bi.newVar( 0.0 )
         (bi, IIdxSeq.empty[ Expr.Var[ S, Long ]])
      }

      println( "__STEP__ create bi and cursor" )
      cursor.step { implicit tx =>
         val bi = access.get._1
         bi.changed.react { tup => println( "__OBSERVE__ " + tup )}
         val biCsr = Longs.newVar[ S ]( 6000 )
         bi.at( biCsr ).changed.react {
            case Change( before, now ) => println( "__CURSOR__ " + before + " -> " + now )
         }
         access.set( (bi -> IIdxSeq( biCsr )))
      }

      println( "__STEP__ bi.set( 10000, 441.0 )" )
      cursor.step { implicit tx =>
         val bi = access.get._1
         bi.set( 10000, 441.0 )
      }

      println( "__STEP__ bi.set( 5000, 882.0 )" )
      cursor.step { implicit tx =>
         val bi = access.get._1
         bi.set( 5000, 882.0 )
      }

      println( "__STEP__ biCsr.set( 7000 )" )
      cursor.step { implicit tx =>
         val biCsr = access.get._2.head
         biCsr.set( 7000 )
      }

      println( "__STEP__ biCsr.set( 11000 )" )
      cursor.step { implicit tx =>
         val biCsr = access.get._2.head
         biCsr.set( 11000 )
      }
   }
}
