package de.sciss.synth.proc

import de.sciss.synth.expr.Strings

object Implicits {
  implicit final class RichProc[S <: Sys[S]](val proc: Proc[S]) extends AnyVal {
    def name(implicit tx: S#Tx): String =
      proc.attributes[Attribute.String](ProcKeys.attrName).fold("<unnamed>")(_.value)

    def name_=(value: String)(implicit tx: S#Tx): Unit = {
      proc.attributes.put(ProcKeys.attrName, Attribute.String(Strings.newConst(value)))
    }
  }
}