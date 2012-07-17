package de.sciss.synth.proc

import de.sciss.confluent.Confluent
import de.sciss.lucre.stm.impl.BerkeleyDB
import java.io.File
import de.sciss.lucre.event.Change
import collection.immutable.{IndexedSeq => IIdxSeq}
import de.sciss.synth.expr.{Longs, Doubles, ExprImplicits}
import de.sciss.lucre.stm.{Cursor, Sys}
import de.sciss.lucre.expr.{Chronos, Expr, BiType, BiPin}

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

      implicit def biSer[ A ]( implicit peer: BiType[ A ]) = BiPin.exprModifiableSerializer[ S, A ]
      implicit val doubles       = Doubles
      implicit val longVarSer    = Longs.varSerializer[ S ]
      implicit val doubleVarSer  = Doubles.varSerializer[ S ]

//      implicit val accessSer = implicitly[ TxnSerializer[ S#Tx, S#Acc, (Bi.Var[ S, Double ], IIdxSeq[ Expr.Var[ S, Long ]])]]

      println( "__STEP__ root" )
      val access = system.root { implicit tx =>
         val bi = BiPin.newExprModifiable( 0.0 )
         (bi, IIdxSeq.empty[ Expr.Var[ S, Long ]], IIdxSeq.empty[ Expr.Var[ S, Double ]])
      }

      println( "__STEP__ create bi and cursor" )
      cursor.step { implicit tx =>
         val bi = access.get._1
         bi.changed.react { tup => println( "__OBSERVE__ " + tup )}
         val biCsr = Longs.newVar[ S ]( 6000 )
//         implicit val ts = Chronos( biCsr )
//         bi.projection.changed.react {
//            case Change( before, now ) => println( "__CURSOR__ " + before + " -> " + now )
//         }
         access.transform( a => a.copy( _2 = a._2 :+ biCsr ))
      }

      println( "__STEP__ bi.set( 10000, 441.0 )" )
      cursor.step { implicit tx =>
         val bi = access.get._1
         bi.add( 10000, 441.0 )
      }

      println( "__STEP__ bi.set( 5000, 882.0 )" )
      cursor.step { implicit tx =>
         val bi = access.get._1
         bi.add( 5000, 882.0 )
      }

//      println( ".....lookup at 7000: " + cursor.step { implicit tx =>
//         val bi = access.get._1
//         (bi.get( 7000 ) -> bi.debugList())
//      })

      println( "__STEP__ biCsr.set( 7000 )" )
      cursor.step { implicit tx =>
         val biCsr = access.get._2.head
         biCsr.set( 7000 )
      }

      println( "__STEP__ biCsr.set( 12000 )" )
      cursor.step { implicit tx =>
         val biCsr = access.get._2.head
         biCsr.set( 12000 )
      }

      println( "__STEP__ bi.set( varTime( 11000 ), varVal( 666.0 ))" )
      cursor.step { implicit tx =>
         val bi      = access.get._1
         val varTime = Longs.newVar[ S ]( 11000 )
         val varVal  = Doubles.newVar[ S ]( 666.0 )
         bi.add( varTime, varVal )
         access.transform( a => a.copy( _2 = a._2 :+ varTime, _3 = a._3 :+ varVal ))
      }

      // XXX this one is not working yet (no events)
      println( "__STEP__ varVal.set( 777.0 )" )
      cursor.step { implicit tx =>
         val varVal = access.get._3.head
         varVal.set( 777.0 )
      }

      // XXX this one is not working yet (no events)
      println( "__STEP__ varTime.set( 9000 )" )
      cursor.step { implicit tx =>
         val varTime = access.get._2.last
         varTime.set( 9000 )
      }
   }
}
