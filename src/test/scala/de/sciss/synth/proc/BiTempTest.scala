package de.sciss.synth.proc

import de.sciss.confluent.Confluent
import de.sciss.lucre.stm.impl.BerkeleyDB
import java.io.File
import de.sciss.lucre.expr.{BiType, Bi}
import de.sciss.synth.expr.{Doubles, ExprImplicits}
import de.sciss.lucre.stm.{Cursor, Sys}

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

      val access = system.root { implicit tx =>
         Bi.newVar( 0.0 )
      }

      cursor.step { implicit tx =>
         access.get.changed.react { tup => println( "__OBSERVE__ " + tup )}
      }

      cursor.step { implicit tx =>
         val bi = access.get
         bi.set( 10000, 441.0 )
      }
   }
}
