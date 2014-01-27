package de.sciss.lucre.synth.expr

object IntVec extends VecLikeType[Int] {
  final val element = Ints

  final val typeID = 0x2000 | Ints.typeID
}

object LongVec extends VecLikeType[Long] {
  final val element = Longs

  final val typeID = 0x2000 | Longs.typeID
}

object DoubleVec extends VecLikeType[Double] {
  final val element = Doubles

  final val typeID = 0x2000 | Doubles.typeID
}
