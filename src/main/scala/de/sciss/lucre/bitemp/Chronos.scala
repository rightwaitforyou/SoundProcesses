//package de.sciss.lucre
//package bitemp
//
//import event.Sys
//
//object Chronos {
//  def apply[S <: Sys[S]](t: Long): Chronos[S] = new Wrap(t)
//
//  private final case class Wrap[S <: Sys[S]](tim: Long) extends Chronos[S] {
//    def time(implicit tx: S#Tx): Long = tim
//  }
//}
//
//trait Chronos[S <: Sys[S]] {
//  def time(implicit tx: S#Tx): Long
//}