name := "soundprocesses"

version := "0.40-SNAPSHOT"

organization := "de.sciss"

homepage := Some( url( "https://github.com/Sciss/SoundProcesses3" ))

description := "A framework for creating and managing ScalaCollider based sound processes"

licenses := Seq( "GPL v2+" -> url( "http://www.gnu.org/licenses/gpl-2.0.txt" ))

scalaVersion := "2.9.2"

libraryDependencies ++= Seq(
   "de.sciss" %% "scalacollider" % "0.34",
   "de.sciss" %% "temporalobjects" % "0.32",
   "de.sciss" %% "lucreexpr" % "0.10",
   "org.scalatest" %% "scalatest" % "1.7.2" % "test"
)

retrieveManaged := true

scalacOptions ++= Seq( "-deprecation", "-unchecked" )   // "-Xelide-below", "INFO"

initialCommands in console := """import de.sciss.synth._; import ugen._; import proc._
import de.sciss.lucre.stm.InMemory
type S = InMemory
implicit val system: S = de.sciss.lucre.stm.InMemory()
implicit val procSer = Proc.serializer[ S ]
implicit val procGroupSer = ProcGroup.serializer[ S ]
val access = system.root { implicit tx => ProcGroup.empty[ S ]}
def group( implicit tx: S#Tx ) : ProcGroup[ S ] = access.get
def t[ A ]( fun: S#Tx => A ) : A = system.step( fun )
def pr()( implicit tx: S#Tx ) : S#Entry[ Proc[ S ]] = { val v = tx.newVar( tx.newID(), Proc[ S ]() ); system.asEntry( v )}
implicit def getProc( p: S#Entry[ Proc[ S ]])( implicit tx: S#Tx ) : Proc[ S ] = p.get
Auralization.run( access )
"""
