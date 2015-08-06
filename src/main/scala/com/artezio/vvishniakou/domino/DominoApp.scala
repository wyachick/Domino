package com.artezio.vvishniakou.domino

import com.artezio.vvishniakou.domino.model.Domino

object DominoApp extends App {
  for (i <- 2 to 28; j <- 1 to 100) {
    val seq = Domino.generator(i)
    println()
    println(seq)
    println("\tUsed All - " + Domino.mayBeUseAll(seq))
    println("\tLine - " + Domino.mayBeLine(seq))
    println("\tRing - " + Domino.mayBeRing(seq))
  }
}
