package de.sciss
package synth
package proc

import lucre.{event => evt}

object Sys {
  trait Txn[S <: Sys[S]] extends evt.Txn[S] with proc.Txn
}

trait Sys[S <: Sys[S]] extends evt.Sys[S] {
  type Tx <: Sys.Txn[S]
}
