## SoundProcesses - TEST BRANCH -

### statement

SoundProcesses is an extension for ScalaCollider to describe, create and manage sound processes in the Scala programming language. It is (C)opyright 2010&ndash;2012 by Hanns Holger Rutz. All rights reserved. SoundProcesses is released under the [GNU General Public License](http://github.com/Sciss/SoundProcesses3/blob/master/licenses/SoundProcesses-License.txt) and comes with absolutely no warranties. To contact the author, send an email to `contact at sciss.de`.

### cycle bug branch

This branch demonstrates multiple bugs with scalac under Scala 2.10.0-M7 (they will also occur in some form on Scala 2.9.2).

All project dependencies are on sonatype and will appear soon in maven central.

The following should happen:

#### Step 1

    $ sbt clean
    ...

    $ sbt test:compile
    ...
    Compiling 59 Scala sources to /Users/hhrutz/Documents/devel/SoundProcesses3/target/scala-2.10.0-M7/classes...
    ...
    [info] Compiling 11 Scala sources to /Users/hhrutz/Documents/devel/SoundProcesses3/target/scala-2.10.0-M7/test-classes...
    [error] /Users/hhrutz/Documents/devel/SoundProcesses3/src/test/scala/de/sciss/synth/proc/VisTest.scala:75: not found: value ProcGroup
    [error]       val g = ProcGroup.Modifiable[ S ]
    [error]               ^
    [error] /Users/hhrutz/Documents/devel/SoundProcesses3/src/test/scala/de/sciss/synth/proc/VisTest.scala:92: not found: value ProcGroup
    [error]    def group( implicit tx: S#Tx ) : ProcGroup.Modifiable[ S ]    = access.get._1
    [error]                                     ^
    [error] /Users/hhrutz/Documents/devel/SoundProcesses3/src/test/scala/de/sciss/synth/proc/VisTest.scala:210: not found: value v
    [error]             case BiPin.Modifiable( v ) => v.add( time, freq )
    [error]                                           ^
    [error] three errors found
    [error] (test:compile) Compilation failed

This is the first bug: Scalac does not find `object ProcGroup` in package `de.sciss.synth.proc`, it seems somehow shadowed (?) by `type ProcGroup` in package object `proc`.

#### Step 2

comment out line 9 in file `src/main/scala/de/sciss/synth/proc/package.scala` (`type ProcGroup`). Compile again:

    $ sbt test:compile
    ...
    [info] Compiling 1 Scala source to /Users/hhrutz/Documents/devel/SoundProcesses3/target/scala-2.10.0-M7/classes...
    [error] /Users/hhrutz/Documents/devel/SoundProcesses3/src/main/scala/de/sciss/synth/proc/package.scala:14: illegal cyclic reference involving type Scan
    [error]    type Scan[ S <: Sys[ S ]] = BiPin.Expr[ S, Scan.Elem[ S ]]
    [error]                                               ^
    [error] one error found
    [error] (compile:compile) Compilation failed

This is the second bug (sometimes you will get the cyclic error with type `Proc` or `ProcGroup` I think). Again this seems an interference between `object Scan` and type alias `Scan` in package object `proc`.

