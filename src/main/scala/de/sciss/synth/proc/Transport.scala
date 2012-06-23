package de.sciss.synth.proc

import de.sciss.lucre.expr.{Expr, Chronos}
import de.sciss.lucre.stm.{Writer, Disposable, Sys}
import collection.immutable.{IndexedSeq => IIdxSeq}
import de.sciss.lucre.event.Event

object Transport {
   def apply[ S <: Sys[ S ]]( group: ProcGroup[ S ], sampleRate: Double = 44100 )
                            ( implicit tx: S#Tx /*, longs: BiType[ Long ]*/) : Transport[ S, Proc[ S ]] =
      impl.TransportImpl( group, sampleRate )

   sealed trait Update[ S <: Sys[ S ], Elem ] { def transport: Transport[ S, Elem ]}
   sealed trait TimeUpdate[ S <: Sys[ S ], Elem ] extends Update[ S, Elem ] {
      def time: Long
      def added: IIdxSeq[ Elem ]
      def removed: IIdxSeq[ Elem ]
   }

   final case class Seek[ S <: Sys[ S ], Elem ]( transport: Transport[ S, Elem ],
                                                 time: Long, added: IIdxSeq[ Elem ], removed: IIdxSeq[ Elem ])
   extends TimeUpdate[ S, Elem ]

   final case class Advance[ S <: Sys[ S ], Elem ]( transport: Transport[ S, Elem ],
                                                    time: Long, added: IIdxSeq[ Elem ], removed: IIdxSeq[ Elem ])
   extends TimeUpdate[ S, Elem ]

   final case class Play[ S <: Sys[ S ], Elem ]( transport: Transport[ S, Elem ]) extends Update[ S, Elem ]
   final case class Stop[ S <: Sys[ S ], Elem ]( transport: Transport[ S, Elem ]) extends Update[ S, Elem ]
}
trait Transport[ S <: Sys[ S ], Elem ] extends Chronos[ S ] with Writer with Disposable[ S#Tx ] {
   def seek( time: Long )( implicit tx: S#Tx ) : Unit
   def playing( implicit tx: S#Tx ) : Expr[ S, Boolean ]
   def playing_=( expr: Expr[ S, Boolean ])( implicit tx: S#Tx ) : Unit

   def sampleRate: Double

   def changed: Event[ S, Transport.Update[ S, Elem ], Transport[ S, Elem ]]

//   def play()( implicit time: Chronos[ S ]) : Unit
//   def stop()( implicit time: Chronos[ S ]) : Unit
}