/*
 *  Scan.scala
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

package de.sciss.synth
package proc

import de.sciss.lucre.{event => evt, DataInput, data}
import impl.{ScanImpl => Impl}
import evt.{Event, Sys}

object Scan {
   object Link {
      implicit def grapheme[ S <: Sys[ S ]]( link: proc.Grapheme[ S ]) : Grapheme[ S ] = Grapheme( link )
      implicit def scan[     S <: Sys[ S ]]( link: proc.Scan[     S ]) : Scan[     S ] = Scan(     link )

      final case class Grapheme[ S <: Sys[ S ]]( peer: proc.Grapheme[ S ]) extends Link[ S ] {
         def id = peer.id
         override def toString = peer.toString
      }
      final case class Scan[     S <: Sys[ S ]]( peer: proc.Scan[     S ]) extends Link[ S ] {
         def id = peer.id
         override def toString = peer.toString
      }
   }
   sealed trait Link[ S <: Sys[ S ]] { def id: S#ID }

   def apply[ S <: Sys[ S ]]( implicit tx: S#Tx ) : Scan[ S ] = Impl.apply

   def read[ S <: Sys[ S ]]( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : Scan[ S ] = Impl.read( in, access )

   implicit def serializer[ S <: Sys[ S ]] : evt.Serializer[ S, Scan[ S ]] = Impl.serializer

   sealed trait Update[ S <: Sys[ S ]] { def scan: Scan[ S ]}
   sealed trait SinkUpdate[ S <: Sys[ S ]] extends Update[ S ] { def sink: Link[ S ]}
   final case class SinkAdded[     S <: Sys[ S ]]( scan: Scan[ S ], sink: Link[ S ]) extends SinkUpdate[ S ] {
      override def toString = "[" + scan + " ---> " + sink + "]"
   }
   final case class SinkRemoved[   S <: Sys[ S ]]( scan: Scan[ S ], sink: Link[ S ]) extends SinkUpdate[ S ] {
      override def toString = "[" + scan + " -/-> " + sink + "]"
   }
   final case class SourceChanged[ S <: Sys[ S ]]( scan: Scan[ S ], source: Option[ Link[ S ]]) extends Update[ S ] {
      override def toString = "[" + scan + " <--- " + source + "]"
   }
   final case class SourceUpdate[ S <: Sys[ S ]]( scan: Scan[ S ], source: Grapheme.Update[ S ]) extends Update[ S ]
}

/**
 * A scan represents a real-time signal which can either function as a reader linked to another scan
 * which functions as its source or a grapheme, or it functions as a writer sinking into a grapheme
 * or another scan. Scans are situated with a process (`Proc`) and identified by a unique name, also
 * known as key. A scan can write to any number of targets, but may only be synchronised to one
 * source. If not synchronised to a source, the owner process' graph may feed a signal into it.
 */
trait Scan[ S <: Sys[ S ]] extends evt.Node[ S ] {
   import Scan._

   def sinks( implicit tx: S#Tx ) : data.Iterator[ S#Tx, Link[ S ]]
   // for now, links are not in t_p; this is probably fine, because
   // we have graphemes for such a 'declarative' view, and the scan as needle is really
   // more the 'procedural' approach
   def addSink(    sink: Link[ S ])( implicit tx: S#Tx ) : Boolean
   def removeSink( sink: Link[ S ])( implicit tx: S#Tx ) : Boolean

//   private[proc] def wasRemoved()( implicit tx: S#Tx ) : Unit

   def source( implicit tx: S#Tx ) : Option[ Link[ S ]]
   def source_=( link: Option[ Link[ S ]])( implicit tx: S#Tx ) : Unit

   def changed: Event[ S, Scan.Update[ S ], Scan[ S ]]

   // called in the implementation from addSink( Link.Scan( _ )). the difference
   // to source_= is that this method should not establish the opposite connection
   // by calling addSink on the source, as this would result in an infinite feedback.
   // still, this method should fire an Scan.SourceChanged event.
   private[proc] def setScanSource( source: Scan[ S ])( implicit tx: S#Tx ) : Unit

//   def key: String
//   def proc: Proc[ S ]
}
