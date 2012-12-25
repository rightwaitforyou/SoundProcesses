/*
 *  AuralPresentation.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2012 Hanns Holger Rutz. All rights reserved.
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

import de.sciss.lucre.{stm, event => evt}
import stm.Disposable
import impl.{AuralPresentationImpl => Impl}

object AuralPresentation {
   // ---- implementation forwards ----

   def run[ S <: Sys[ S ], I <: stm.Sys[ I ]]( transport: ProcTransport[ S ], aural: AuralSystem[ S ])
                                             ( implicit tx: S#Tx, bridge: S#Tx => I#Tx, /* cursor: Cursor[ S ], */
                                               artifactStore: ArtifactStore[ S ]) : AuralPresentation[ S ] =
      Impl.run[ S, I ]( transport, aural )

   private[proc] trait Running[ S <: Sys[ S ]] {
      /**
       * Queries the number of channel associated with a scanned input.
       * Throws a control throwable when no value can be determined, making
       * the ugen graph builder mark the querying graph element as incomplete
       * (missing information).
       *
       * @param timed   the process whose graph is currently built
       * @param time    the time at which to query the scan
       * @param key     the scan key
       * @param tx      the current transaction
       * @return        the number of channels for the scan input at the given time
       */
      def scanInNumChannels( timed: TimedProc[ S ], time: Long, key: String )( implicit tx: S#Tx ) : Int // Scan_.Value[ S ]

      final case class MissingInfo( source: TimedProc[ S ], key: String ) extends Throwable
   }
}
trait AuralPresentation[ S <: Sys[ S ]] extends Disposable[ S#Tx ] {
   def group( implicit tx: S#Tx ) : Option[ Group ]
}
