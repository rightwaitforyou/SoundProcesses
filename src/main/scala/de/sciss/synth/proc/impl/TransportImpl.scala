package de.sciss.synth.proc
package impl

import de.sciss.lucre.stm.{Source, Cursor, Sys}
import de.sciss.lucre.expr.{BiGroup, SpanLike, Span, Expr}
import de.sciss.lucre.{DataInput, DataOutput}
import de.sciss.lucre.{event => evt}
import evt.Event
import de.sciss.synth.expr.Booleans
import de.sciss.collection.txn
import collection.immutable.{IndexedSeq => IIdxSeq}
import java.util.concurrent.{TimeUnit, ScheduledExecutorService, Executors}
import concurrent.stm.{Txn => STMTxn, TxnLocal => STMTxnLocal}

object TransportImpl {
   private val VERBOSE = false

   def apply[ S <: Sys[ S ], A ]( group: ProcGroup[ S ], sampleRate: Double, self: => Source[ S#Tx, A ])
                                ( implicit tx: S#Tx, cursor: Cursor[ S ],
                                  selfView: A => Transport[ S, Proc[ S ]]) : Transport[ S, Proc[ S ]] = {
      val targets    = evt.Targets[ S ]
      val id         = targets.id
      val playingVar = Booleans.newVar[ S ]( Booleans.newConst( false ))
      val validVar   = tx.newIntVar( id, 0 )
      val lastTime   = tx.newLongVar( id, 0L )
      new Impl( targets, group, sampleRate, playingVar, validVar, lastTime, self )
   }

   def serializer[ S <: Sys[ S ], A ]( self: => Source[ S#Tx, A ])
                                     ( implicit cursor: Cursor[ S ],
                                       selfView: A => Transport[ S, Proc[ S ]]) : evt.NodeSerializer[ S, Transport[ S, Proc[ S ]]] =
      new Ser[ S, A ]( self )

   private final class Ser[ S <: Sys[ S ], A ]( self: => Source[ S#Tx, A ])( implicit cursor: Cursor[ S ],
                                                                             selfView: A => Transport[ S, Proc[ S ]])
   extends evt.NodeSerializer[ S, Transport[ S, Proc[ S ]]] {
      def read( in: DataInput, access: S#Acc, targets: evt.Targets[ S ])( implicit tx: S#Tx ) : Transport[ S, Proc[ S ]] = {
         val id         = targets.id
         val group      = ProcGroupX.read( in, access )
         val sampleRate = in.readDouble()
         val playingVar = Booleans.readVar( in, access )
         val validVar   = tx.readIntVar( id, in )
         val lastTime   = tx.readLongVar( id, in )
         new Impl( targets, group, sampleRate, playingVar, validVar, lastTime, self )
      }
   }

   private lazy val pool : ScheduledExecutorService = {        // system wide scheduler
      val res = Executors.newScheduledThreadPool( 1 )
      sys.addShutdownHook( shutdownScheduler() )
      res
   }
   private val cpuTime = STMTxnLocal( System.nanoTime()/1000 ) // system wide wall clock in microseconds

   def shutdownScheduler() {
     println( "Shutting down scheduler thread pool" )
     pool.shutdown()
   }

//   private def flatSpans[ S <: Sys[ S ]]( in: (SpanLike, IIdxSeq[ (Expr[ S, SpanLike ], Proc[ S ])])) : IIdxSeq[ (SpanLike, Proc[ S ])] = {
//      val span = in._1
//      in._2.map { case (_, proc) => (span, proc) }
//   }

   private def flatSpans[ S <: Sys[ S ]]( in: (SpanLike, IIdxSeq[ BiGroup.TimedElem[ S, Proc[ S ]]])) : IIdxSeq[ (SpanLike, BiGroup.TimedElem[ S, Proc[ S ]])] = {
      val span = in._1
      in._2.map { span -> _ }
   }

   private final class Impl[ S <: Sys[ S ], A ]( protected val targets: evt.Targets[ S ],
                                                 group: ProcGroup[ S ],
                                                 val sampleRate: Double, playingVar: Expr.Var[ S, Boolean ],
                                                 validVar: S#Var[ Int ], lastTime: S#Var[ Long ],
                                                 self: => Source[ S#Tx, A ])
                                               ( implicit cursor: Cursor[ S ], selfView: A => Transport[ S, Proc[ S ]])
   extends Transport[ S, Proc[ S ]]
   with evt.Trigger.Impl[ S, Transport.Update[ S, Proc[ S ]], Transport.Update[ S, Proc[ S ]], Transport[ S, Proc[ S ]]]
   with evt.StandaloneLike[ S, Transport.Update[ S, Proc[ S ]], Transport[ S, Proc[ S ]]]
   with evt.Root[ S, Transport.Update[ S, Proc[ S ]]]
   {
      private val microsPerSample = 1000000 / sampleRate

      private type Elem = Proc[ S ]

      override def toString() = "Transport" + id

      protected def writeData( out: DataOutput ) {
         group.write( out )
         out.writeDouble( sampleRate )
         playingVar.write( out )
         validVar.write( out )
         lastTime.write( out )
      }

      protected def disposeData()( implicit tx: S#Tx ) {
         playingVar.dispose()
         validVar.dispose()
         lastTime.dispose()
      }

      def iterator( implicit tx: S#Tx ) : txn.Iterator[ S#Tx, (SpanLike, BiGroup.TimedElem[ S, Elem ])] =
         group.intersect( time ).flatMap( flatSpans )

      def seek( time: Long )( implicit tx: S#Tx ) {
         advance( playing = false, lastTime.get, time, true, true )
      }

      private def advance( playing: Boolean, oldFrame: Long, newFrame: Long,
                           hasProcEvent: Boolean, hasParEvent: Boolean )( implicit tx: S#Tx ) {
if( VERBOSE ) println( "::: advance(" + playing + ", " + oldFrame + ", " + newFrame + ")" )
         if( newFrame == oldFrame ) return
         lastTime.set( newFrame )

         val (removed, added) = if( hasProcEvent ) {
            val (remStart, remStop, addStart, addStop) = if( newFrame > oldFrame ) {
               // ... those which end in the interval (LRP, t] && begin <= LRP must be removed ...
               // ... those which begin in the interval (LRP, t] && end > t must be added ...
               val skipInt = Span( oldFrame + 1, newFrame + 1 )
               (Span.until( oldFrame + 1 ), skipInt, skipInt, Span.from( newFrame + 1 ))
            } else {
               // ... those which begin in the interval (t, LRP] && end > LRP must be removed ...
               // ... those which end in the interval (t, LRP] && begin <=t must be added ...
               val skipInt = Span( newFrame + 1, oldFrame + 1 )
               (skipInt, Span.from( oldFrame + 1 ), Span.until( newFrame + 1 ), skipInt)
            }

            (group.rangeSearch( remStart, remStop ).flatMap( flatSpans ).toIndexedSeq,
             group.rangeSearch( addStart, addStop ).flatMap( flatSpans ).toIndexedSeq)
         } else (IIdxSeq.empty, IIdxSeq.empty)

         val params = if( hasParEvent  ) {
            var res = IIdxSeq.empty[ (SpanLike, BiGroup.TimedElem[ S, Proc[ S ]], Map[ String, Param ])]
            group.intersect( newFrame ).foreach { case (span, entries) =>
               entries.foreach { case timed =>
                  val par = timed.value.par
                  var map = Map.empty[ String, Param ]
                  par.keys.foreach { key =>
                     par.get( key ).foreach { bi =>
                        val oldEx = bi.at( oldFrame )
                        val newEx = bi.at( newFrame )
                        if( oldEx != newEx ) {
                           map += key -> newEx.value
                        }
                     }
                  }
                  if( map.nonEmpty ) res :+= (span, timed, map)
               }
            }
            res
         } else IIdxSeq.empty

         if( removed.nonEmpty || added.nonEmpty || params.nonEmpty ) {
            fire( Transport.Advance( this, playing, newFrame, added, removed, params ))
         }
      }

      def playing( implicit tx: S#Tx ) : Expr[ S, Boolean ] = playingVar.get

      def playing_=( expr: Expr[ S, Boolean ])( implicit tx: S#Tx ) {
         val wasPlaying = playingVar.get.value
         val isPlaying  = expr.value
         playingVar.set( expr )
         if( wasPlaying != isPlaying ) {
            if( isPlaying ) play() else stop()
         }
      }

      private def play()( implicit tx: S#Tx ) {
         scheduleNext()
         fire( Transport.Play( this ))
      }

      private def stop()( implicit tx: S#Tx ) {
         validVar.transform( _ + 1 )
         fire( Transport.Stop( this ))
      }

      private def scheduleNext()( implicit tx: S#Tx ) {
         val oldFrame      = lastTime.get
         val searchStart   = oldFrame + 1
         val procMin       = group.nearestEventAfter( searchStart ).getOrElse( Long.MaxValue )
         val hasProcEvent  = procMin != Long.MaxValue
         val innerSpan     = if( hasProcEvent ) {
            Span( searchStart, procMin )
         } else {
            Span.from( searchStart )
         }
         var parMin = Long.MaxValue
         group.intersect( innerSpan ).foreach { case (span, entries) =>
            entries.foreach { case timed =>
               val par = timed.value.par
               par.keys.foreach { key =>
                  par.get( key ).foreach { bi =>
                     bi.nearestEventAfter( searchStart ) match {
                        case Some( time ) if (time < parMin) && (span.compareStart( time ) < 0) && (span.compareStop( time ) > 0) => parMin = time
                        case _ =>
                     }
                  }
               }
            }
         }
         val hasParEvent = parMin != Long.MaxValue

         if( hasProcEvent || hasParEvent ) {
            val newFrame   = math.min( procMin, parMin )
            val delay      = ((newFrame - oldFrame) * microsPerSample).toLong
            val v          = validVar.get
            val logical    = cpuTime.get( tx.peer )
            val jitter     = System.nanoTime()/1000 - logical
            val effective  = math.max( 0L, delay - jitter )
if( VERBOSE ) println( "::: scheduled: delay = " + delay + ", effective = " + effective + ", new frame = " + newFrame )
            STMTxn.afterCommit( _ => {
               pool.schedule( new Runnable {
                  def run() { cursor.step { implicit tx =>
                     selfView( self.get ).eventReached( v, logical + delay, oldFrame, newFrame, hasProcEvent, hasParEvent )
                  }}
               }, effective, TimeUnit.MICROSECONDS )
            })( tx.peer )
         }
      }

      def eventReached( valid: Int, newLogical: Long, oldFrame: Long, newFrame: Long,
                        hasProcEvent: Boolean, hasParEvent: Boolean )( implicit tx: S#Tx ) {
         // if the transport had been stopped between scheduling and actual
         // execution of the scheduled Runnable, then we will find a
         // discrepancy in validVar. In that case, just don't do anything.
         if( valid == validVar.get ) {
            cpuTime.set( newLogical )( tx.peer )
            advance( playing = true, oldFrame = oldFrame, newFrame = newFrame, hasProcEvent, hasParEvent )
            scheduleNext()
         }
      }

      def time( implicit tx: S#Tx ) : Long = lastTime.get   // XXX // Expr[ S, Long ] = timeExpr

      // ---- event stuff ----

      def changed : Event[ S, Transport.Update[ S, Proc[ S ]], Transport[ S, Proc[ S ]]] = this

      protected def reader: evt.Reader[ S, Transport[ S, Proc[ S ]]] = serializer[ S, A ]( self )
   }
}
