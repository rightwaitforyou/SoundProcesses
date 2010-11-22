object ProcFactory {
   def apply[ C <: CtxLike, V[ _ ] <: EVar[ C, _ ]]( name: String )( implicit sys: System[ C, V ], c: C ) : Proc[ C, V ] = {
      val sw = sys.v( false )
      new Proc[ C, V ] {
         val switch = sw
      }
   }
}