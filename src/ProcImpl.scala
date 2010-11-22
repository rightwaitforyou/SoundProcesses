object ProcFactory {
   def apply[ S <: System ]( name: String )( implicit cp: CtxProvider[ S ]) : Proc[ S ] = {
      new Proc[ S ] {
         val sys : S = cp.sys
         val switch = sys.v( false )( cp.ctx )
      }
//      error( "FUCK YOU" )
   }
//
//   private class Impl[ S <: System ]( val switch: ) extends Proc[ S ] {
//
//   }
}