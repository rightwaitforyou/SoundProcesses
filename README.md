## SoundProcesses

### statement

SoundProcesses is an extension for ScalaCollider to describe, create and manage sound processes. It is (C)opyright 2010&ndash;2012 by Hanns Holger Rutz. All rights reserved. SoundProcesses is released under the [GNU General Public License](http://github.com/Sciss/SoundProcesses3/blob/master/licenses/SoundProcesses-License.txt) and comes with absolutely no warranties. To contact the author, send an email to `contact at sciss.de`.

### compilation

SoundProcesses builds with sbt 0.11.3 and Scala 2.9.2. The dependencies should be downloaded automatically from maven central repository.

### REPL example

Everything under construction...

    val p = t { implicit tx => pr() }
    t { implicit tx => group.add( p )}
    t { implicit tx => p.graph = { Out.ar( 0, RHPF.ar((BrownNoise.ar(Seq(0.5,0.5))-0.49).max(0) * 20, 5000, 1))}}
    t { implicit tx => p.stop() }
    t { implicit tx => p.play() }
