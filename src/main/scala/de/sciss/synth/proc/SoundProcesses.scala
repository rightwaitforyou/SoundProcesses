/*
 *  SoundProcesses.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2013 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc

import java.util.concurrent.{Executors, ScheduledExecutorService}

object SoundProcesses {
   var poolSize : Option[ Int ] = None

   private[proc] def isPowerOfTwo( value: Int ) = (value & (value-1)) == 0

   private[proc] def validateCueBufferSize( value: Int ) {
      require( isPowerOfTwo( value ) && value >= 8192 && value <= 131072,
         "Must be a power of two and in (8192, 131072) : " + value )
   }

   private var cueBufSz = 32768
   def cueBufferSize : Int = cueBufSz
   def cueBufferSize_=( value: Int ) {
      validateCueBufferSize( value )
      cueBufSz = value
   }

   lazy val pool : ScheduledExecutorService = {                // system wide scheduler
      val res = poolSize match {
         case Some( sz ) => Executors.newScheduledThreadPool( sz )
         case _          => Executors.newSingleThreadScheduledExecutor()
      }
      sys.addShutdownHook( shutdownScheduler() )
      res
   }

   private def shutdownScheduler() {
     log( "Shutting down scheduler thread pool" )
     pool.shutdown()
   }
}