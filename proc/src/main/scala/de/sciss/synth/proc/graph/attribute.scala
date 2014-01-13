/*
 *  attribute.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2014 Hanns Holger Rutz. All rights reserved.
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
package graph

object attribute {
  private[proc] def controlName(key: String): String = "$attr_"  + key

  private final case class In(key: String, default: Double) extends GE.Lazy with ScalarRated {

    override def productPrefix  = "attribute$In"
    override def toString       = s"""attribute("$key").ir($default)"""

    def makeUGens: UGenInLike = {
      UGenGraph.builder match {
        case b: UGenGraphBuilder[_] =>
          b.addAttributeIn(key)
          val ctlName = controlName(key)
          ctlName.ir(default).expand

        case _ => UGenGraphBuilder.outsideOfContext()
      }
    }
  }
}
final case class attribute(key: String) {
  def ir: GE = ir(0.0)
  def ir(default: Double): GE = attribute.In(key, default)
}