/*
 *  Action.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2014 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth
package proc
package graph

import de.sciss.synth.proc.UGenGraphBuilder.Input

/** A graph element that executes an action upon receiving a trigger.
  *
  * @param trig   the trigger input signal
  * @param key    a key into the process' attribute map. the value peer stored
  *               at that location should be of type `proc.Action`
  */
final case class Action(trig: GE, key: String) extends Lazy.Expander[Unit] with HasSideEffect {
  protected def makeUGens: Unit = {
    val b = UGenGraphBuilder.get
    b.requestInput(Input.Action(key))
    // println(s"FOUND TRIGGER $trig")
    // de.sciss.synth.ugen.DC.kr(123).poll(trig, "test")
    // trig.poll(1, "trig")
    impl.ActionResponder.makeUGen(trig, key)
  }
}
