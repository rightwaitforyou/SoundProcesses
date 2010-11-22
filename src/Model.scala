object Model {
   trait Listener[ -C, -T ] {
      def updated( v: T )( implicit c: C ) : Unit
   }

   def onCommit[ C <: TxnHolder, T ]( committed: Traversable[ T ] => Unit ) : Listener[ C, T ] =
      filterOnCommit( (_: T, _: C) => true )( committed )

   def filterOnCommit[ C <: TxnHolder, T ]( filter: Function2[ T, C, Boolean ])( committed: Traversable[ T ] => Unit ) =
      new Listener[ C, T ] {
//         val queueRef = new TxnLocal[ IQueue[ T ]] {
//            override protected def initialValue( txn: Txn ) = IQueue.empty
//         }
         def updated( update: T )( implicit c: C ) {
//            if( filter( update, c )) {
//               val txn  = c.txn
//               val q0   = queueRef.get( txn )
//               queueRef.set( q0 enqueue update )( txn )
//               if( q0.isEmpty ) {
//                  txn.beforeCommit( txn => {
//                     val q1 = queueRef.get( txn )
//                     txn.afterCommit( _ => committed( q1 ))
//                  }, Int.MaxValue )
//               }
//            }
         }
      }
}

trait Model[ C, T ] {
   import Model._

   type L = Listener[ C, T ]

   def addListener( l: Listener[ C, T ])( implicit c: ECtx ) : Unit
   def removeListener( l: Listener[ C, T ])( implicit c: ECtx ) : Unit
}
