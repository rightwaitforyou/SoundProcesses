## SoundProcesses

### statement

SoundProcesses is an extension for ScalaCollider to describe, create and manage sound processes in the Scala programming language. It is (C)opyright 2010&ndash;2013 by Hanns Holger Rutz. All rights reserved. SoundProcesses is released under the [GNU General Public License](http://github.com/Sciss/SoundProcesses3/blob/master/licenses/SoundProcesses-License.txt) and comes with absolutely no warranties. To contact the author, send an email to `contact at sciss.de`.

### building

SoundProcesses builds with sbt 0.12 and Scala 2.10. The dependencies should be downloaded automatically from maven central repository.

## linking

The following dependency is necessary:

    resolvers += "Oracle Repository" at "http://download.oracle.com/maven"
    
    "de.sciss" %% "soundprocesses" % v

The current version `v` is `"1.4.+"`.

### REPL example

Everything under construction... __Note__: the following is obsolete:

    val p = t { implicit tx => pr() }
    t { implicit tx => group.add( p )}
    t { implicit tx => p.graph = { Out.ar( 0, RHPF.ar((BrownNoise.ar(Seq(0.5,0.5))-0.49).max(0) * 20, 5000, 1))}}
    t { implicit tx => p.stop() }
    t { implicit tx => p.play() }
