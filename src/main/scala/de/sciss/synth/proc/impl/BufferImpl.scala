package de.sciss.synth.proc
package impl

import de.sciss.synth.{Buffer => SBuffer }

private[proc] final case class BufferImpl( server: Server, peer: SBuffer )( closeOnDisposal: Boolean )
extends ResourceImpl with Buffer.Modifiable {
//   val isAlive:    State = State(                this, "isAlive", init = true )
//   val isOnline:   State = State.and( isAlive )( this, "isOnline", init = false )
//   val hasContent: State = State(                this, "hasContent", init = false )

   def id: Int = peer.id

//   def alloc( numFrames: Int, numChannels: Int = 1 )( implicit tx: ProcTxn ) {
//      tx.add( msg          = peer.allocMsg( numFrames, numChannels ),
//              change       = Some( (RequiresChange, isOnline, true) ),
//              dependencies = Map( isAlive -> true ),
//              audible      = false
//      )
//   }
//
   def allocRead( path: String, startFrame: Long, numFrames: Int )( implicit tx: Txn ) {
      require( isOnline )
      require( startFrame <= 0x7FFFFFFFL, "Cannot encode start frame >32 bit (" + startFrame + ")" )
      val frameI = startFrame.toInt
      tx.addMessage( this, peer.allocReadMsg( path, startFrame = frameI, numFrames = numFrames ), audible = false )
   }

//
//   def record( path: String, fileType: AudioFileType, sampleFormat: SampleFormat )( implicit tx: ProcTxn ) {
//      tx.add( msg          = peer.writeMsg( path, fileType, sampleFormat, startFrame = 0, numFrames = 0, leaveOpen = true ),
//              change       = Some( (Always, hasContent, true) ),
//              dependencies = Map( isOnline -> true ), // hasContent is a bit misleading...
//              audible      = false
//      )
//   }


   def read( path: String, fileStartFrame: Long, numFrames: Int, bufStartFrame: Int )( implicit tx: Txn ) {
      require( isOnline )
      require( fileStartFrame <= 0x7FFFFFFFL, "Cannot encode start frame >32 bit (" + fileStartFrame + ")" )
      val frameI = fileStartFrame.toInt
      tx.addMessage( this, peer.readMsg( path, fileStartFrame = frameI, numFrames = numFrames, leaveOpen = false ),
                     audible = false )
   }

   def zero()( implicit tx: Txn ) {
      require( isOnline )
      tx.addMessage( this, peer.zeroMsg, audible = false )
   }

   def dispose()( implicit tx: Txn ) {
      require( isOnline )
      if( closeOnDisposal ) {
         tx.addMessage( this, peer.closeMsg, audible = false )
      }
      tx.addMessage( this, peer.freeMsg( release = false ), audible = false )
      server.freeBuffer( peer.id )
      disposed()
   }
}