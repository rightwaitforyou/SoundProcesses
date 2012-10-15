package de.sciss.synth.proc.impl

import de.sciss.lucre.{DataInput, DataOutput, stm}

object DummySerializerFactory {
   def apply[ I <: stm.Sys[ I ]] : DummySerializerFactory[ I ] = anySer.asInstanceOf[ DummySerializerFactory[ I ]]

   private val anySer = new Impl[ stm.InMemory, Nothing ]

   private class Impl[ I <: stm.Sys[ I ], A ]
   extends stm.Serializer[ I#Tx, I#Acc, A ] with DummySerializerFactory[ I ] {
      implicit def dummySerializer[ A1 ] : stm.Serializer[ I#Tx, I#Acc, A1 ] =
         this.asInstanceOf[ stm.Serializer[ I#Tx, I#Acc, A1 ]]

      def write( v: A, out: DataOutput) {}
      def read( in: DataInput, access: I#Acc )( implicit tx: I#Tx ) : A =
         sys.error( "Operation not supported" )
   }
}
trait DummySerializerFactory[ I <: stm.Sys[ I ]] {
   implicit def dummySerializer[ A ] : stm.Serializer[ I#Tx, I#Acc, A ]
}