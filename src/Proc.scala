trait Proc[ C <: CtxLike, V[ _ ] <: EVar[ C, _ ]] {
//trait Proc[ V[ _ ] <: EVar[ _, _ ], S <: System[ _, V ]]
//   val sys : S
//   def switch : sys.Var[ Boolean ]
   def switch : V[ Boolean ]
}
