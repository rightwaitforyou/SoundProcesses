package de.sciss.synth.expr

final case class Span( start: Long, stop: Long ) {
   require( start <= stop )
   def contains( pos: Long ) : Boolean = pos >= start && pos < stop
}