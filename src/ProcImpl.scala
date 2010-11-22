object ProcFactory {
   def apply[ S <: System, C <: CtxLike[ S ]]( name: String )( implicit s: S, c: C ) : Proc[ S ] = {
      new Proc[ S ] {
         val sys = s
         val switch = s.v( false )( c )
      }
//      error( "FUCK YOU" )
   }
//
//   private class Impl[ S <: System ]( val switch: ) extends Proc[ S ] {
//
//   }
}