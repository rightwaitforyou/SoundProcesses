package de.sciss.synth.proc

import concurrent.stm.Ref

trait ResourceManagement {
   def messageTimeStamp : Ref[ Int ]
}