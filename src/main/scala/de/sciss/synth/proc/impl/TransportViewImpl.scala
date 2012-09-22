package de.sciss.synth
package proc
package impl

import de.sciss.lucre.{stm, bitemp}
import stm.{InMemory, IdentifierMap, Sys}
import bitemp.Span
import collection.immutable.{IndexedSeq => IIdxSeq}
import de.sciss.lucre.data.SkipList

object TransportViewImpl {
   def apply[ S <: Sys[ S ]]( transport: ProcTransport[ S ])( implicit tx: S#Tx ) /* : TransportView[ S ] */ = {
      val time    = transport.time
      val flags   = tx.newInMemoryIDMap[ Flag[ S ]]
      val view    = new Impl( transport, ???, flags )
      transport.group.intersect( time ).foreach {
         case (Span.HasStart( span ), seq) =>
            seq.foreach { timed => view.add( span, timed )}
         case _ =>
      }
      view
   }

   private sealed trait Flag[ +S ] {
      def time: Long
   }
   private final case class GroupFlag( time: Long ) extends Flag[ Nothing ]
   private final case class ProcFlag[ S <: Sys[ S ]]( id: S#ID, time: Long ) extends Flag[ S ]

   private final case class NextEvent[ S <: Sys[ S ]]( nextProcTime: Long, nextGraphemeTime: Long, graphemes: IIdxSeq[ GraphemeInfo[ S#ID ]])

   private trait GraphemeInfo[ +ID ] {
      def id: ID
      def key: String
   }

   final class Impl[ S <: Sys[ S ]]( transportStale: ProcTransport[ S ], csrPos: S#Acc,
                                     flags: IdentifierMap[ S#ID, S#Tx, Flag[ S ]]) {

//      private val prevTime: I#Var[ Long ] = ???
//      private val nextTime: I#Var[ Long ] = ???

      private val waitSpan: I#Var[ Span ] = ???

      private val nextProcEvent: I#Var[ Long ] = ???
      private val nextEvent: I#Var[ NextEvent[ S ]] = ???

      private val graphemePrio: SkipList.Map[ I, Long, IIdxSeq[ GraphemeInfo[ S#ID ]]] = ???

      def play()( implicit tx: S#Tx ) {
//         scheduleNext( ??? )
      }

      private def group( implicit tx: S#Tx ) : ProcGroup[ S ] = ??? // tx.refresh( csrPos, transportStale ).group

      private def scheduleNext( oldFrame: Long, oldEvt: NextEvent[ S ])( implicit tx: S#Tx ) {
         implicit val itx: I#Tx = ???
         val searchStart   = oldFrame + 1
         val nextProc      = nextProcEvent.get

         val newEvt0       = if( oldEvt.nextProcTime <= oldFrame ) {       // have to search for the next group event
            oldEvt.copy( nextProcTime = group.nearestEventAfter( searchStart ).getOrElse( Long.MaxValue ))
         } else oldEvt

         val newEvt1       = if( oldEvt.nextGraphemeTime <= oldFrame ) {   // have to search for the next grapheme event
            val (gTime, gInfos) = graphemePrio.ceil( searchStart ).getOrElse( Long.MaxValue -> IIdxSeq.empty )
            newEvt0.copy( nextGraphemeTime = gTime, graphemes = gInfos )
         } else newEvt0

         val procMin       = group.nearestEventAfter( searchStart ).getOrElse( Long.MaxValue )
         val hasProcEvent  = procMin != Long.MaxValue
//         val innerSpan     = if( hasProcEvent ) {
//            Span( searchStart, procMin )
//         } else {
//            Span.from( searchStart )
//         }
         val /* var */ parMin = Long.MaxValue
// XXX TODO
//         group.intersect( innerSpan ).foreach { case (span, entries) =>
//            entries.foreach { case timed =>
//               val par = timed.value.par
//               par.keys.foreach { key =>
//                  par.get( key ).foreach { bi =>
//                     bi.nearestEventAfter( searchStart ) match {
//                        case Some( time ) if (time < parMin) && (span.compareStart( time ) < 0) && (span.compareStop( time ) > 0) => parMin = time
//                        case _ =>
//                     }
//                  }
//               }
//            }
//         }
         val hasParEvent = parMin != Long.MaxValue

//         if( hasProcEvent || hasParEvent ) {
//            val newFrame   = math.min( procMin, parMin )
//            val delay      = ((newFrame - oldFrame) * microsPerSample).toLong
//            val v          = validVar.get
//            val logical    = cpuTime.get( tx.peer )
//            val jitter     = System.nanoTime()/1000 - logical
//            val effective  = math.max( 0L, delay - jitter )
//if( VERBOSE ) println( "::: scheduled: delay = " + delay + ", effective = " + effective + ", new frame = " + newFrame )
//            STMTxn.afterCommit( _ => {
//               pool.schedule( new Runnable {
//                  def run() { cursor.step { implicit tx =>
//                     val self = tx.refresh[ ProcTransport[ S ]]( csrPos, me )
//                     self.eventReached( v, logical + delay, oldFrame, newFrame, hasProcEvent, hasParEvent )
//                  }}
//               }, effective, TimeUnit.MICROSECONDS )
//            })( tx.peer )
//         }
      }

      def add( span: Span.HasStart, timed: TimedProc[ S ])( implicit tx: S#Tx ) {
         implicit val itx: I#Tx = ???
         val w = waitSpan.get
         if( w.touches( span )) {
            // interrupt scheduled task

//            val p = timed.value
//            p.scans.iterator.foreach {
//               case (key, scan) => scan.source match {
//                  case Some( Scan.Link.Grapheme( grapheme )) =>
//                     grapheme.nearestEventAfter( span.start )   // XXX
//                  case _ =>
//               }
//            }
         } else {
            // see if it appears earlier than the next region
         }
      }
   }
}
