object ProcImpl {
   def apply[ S <: System ]( name: String )( implicit sys: S ) : Proc[ S ] = {
      error( "NOT YET IMPLEMENTED")
   }
}