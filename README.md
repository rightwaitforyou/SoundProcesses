# SoundProcesses

## statement

SoundProcesses is an extension for ScalaCollider to describe, create and manage sound processes in the Scala programming language. It is (C)opyright 2010&ndash;2015 by Hanns Holger Rutz. All rights reserved. SoundProcesses is released under the [GNU General Public License](http://github.com/Sciss/SoundProcesses/blob/master/licenses/SoundProcesses-License.txt) and comes with absolutely no warranties. To contact the author, send an email to `contact at sciss.de`.

Further reading:

 - Rutz, H. H., "SoundProcesses: A New Computer Music Framework," in Proceedings of the ICMC|SMC|2014 Joint Conference, Athens 2014.

## building

SoundProcesses builds with sbt 0.13 and Scala 2.11, 2.10. The dependencies should be downloaded automatically from maven central repository.

## linking

The following dependency is necessary:

    "de.sciss" %% "soundprocesses" % v

The current version `v` is `"2.14.1"`.

The following sub modules are available:

    "de.sciss" %% "lucrebitemp"     % v          // bi-temporal data structures
    "de.sciss" %% "lucresynth"      % v          // transactional layer for ScalaCollider
    "de.sciss" %% "lucresynth-expr" % v          // standard expression types
    "de.sciss" %% "soundprocesses-core"     % v  // everything but views and compiler
    "de.sciss" %% "soundprocesses-views"    % v  // common swing views
    "de.sciss" %% "soundprocesses-compiler" % v  // compiler integration

For compiling the tests, the following additional resolver may be needed:

    resolvers += "Oracle Repository" at "http://download.oracle.com/maven"

## usage

Project is still experimental, and documentation is still missing. There is a graphical front-end [Mellite](https://github.com/Sciss/Mellite) (also experimental)...
