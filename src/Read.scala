trait System {
   type Var[ _ ]
}

trait ESystem extends System {
   type Var[ T ] = EVarX[ T ] // EVar[ ECtx, T ]
}

trait KSystem extends System {
   override type Var[ T ] = KVarX[ T ] // KVar[ KCtx, T ]
}

trait Proc[ S <: ESystem ] {
   val sys : S
   def switch : sys.Var[ Boolean ]
}
