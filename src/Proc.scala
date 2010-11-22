trait Proc[ C <: CtxLike, V[ _ ] <: EVar[ C, _ ]] {
//   val sys : S
//   def switch : sys.Var[ Boolean ]
   def switch : V[ Boolean ]
}
