trait Proc[ S <: System ] {
   val sys : S
   def switch : sys.Var[ Boolean ]
}
