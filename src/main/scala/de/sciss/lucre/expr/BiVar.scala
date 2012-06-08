//package de.sciss.lucre.expr
//
//import de.sciss.lucre.stm.{Writer, Disposable}
//
//
//sealed trait BiSink[ -Tx, -Time, -A ] {
//   def set( v: A )( implicit tx: Tx, time: Time ) : Unit
//}
//trait BiSource[ -Tx, -Time, +A ] extends Writer with Disposable[ Tx ] {
//   def get( implicit tx: Tx, time: Time ) : A
//}
//
//trait BiVar[ -Tx, -Time, A ] extends BiSink[ Tx, Time, A ] with BiSource[ Tx, Time, A ] {
////   def transform( f: A => A )( implicit time: Time ) : Unit
//}