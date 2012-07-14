//package de.sciss.lucre.event
//
//import de.sciss.lucre.stm.Sys
//import de.sciss.synth.proc.Proc
//
//trait KeyedGroupEvent[ S <: Sys[ S ], A, B, U ]
//extends EventImpl[ S, Proc.ParamChange[ S ], Proc.ParamChange[ S ], Proc[ S ]]
//with InvariantEvent[ S, Proc.ParamChange[ S ], Proc[ S ]] {
////   protected def reader : Reader[ S, Proc[ S ]] = serializer // ( eventView )
////   def slot: Int = 2
////   def node: Node[ S ] = proc
//
//   private abstract class EntryNode( entry: (A, B) )
//   extends StandaloneLike[ S, (A, U), EntryNode ] {
//
//   }
//
//   def connect()( implicit tx: S#Tx ) {}
//   def disconnect()( implicit tx: S#Tx ) {}
//
//   def +=( entry: (A, B) )( implicit tx: S#Tx ) {
//      val node = new EntryNode( entry ) {}
//      elem.changed ---> node
//   }
//
//   def -=( entry: (A, B) )( implicit tx: S#Tx ) {
//      elem.changed -/-> this
//   }
//
//   def pullUpdate( pull: Pull[ S ])( implicit tx: S#Tx ) : Option[ Proc.ParamChange[ S ]] = {
//      val changes = pull.parents( this ).foldLeft( Map.empty[ String, BiPin.ExprUpdate[ S, Param ]]) { case (map, sel) =>
//         val elem = Intruder.devirtualizeNode( sel, elemSerializer.asInstanceOf[ Reader[ S, Node[ S ]]])
//            .asInstanceOf[ ParamEx ]
//         elem.changed.pullUpdate( pull ) match {
//            case Some( biUpd ) => map.getOrElse( )
//            case None => map
//         }
//      }
//
//      if( changes.isEmpty ) None else Some( Proc.ParamChange( proc, changes ))
//   }
//}
