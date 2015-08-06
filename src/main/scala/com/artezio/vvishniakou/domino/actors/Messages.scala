package com.artezio.vvishniakou.domino.actors

import com.artezio.vvishniakou.domino.model.Domino

object Messages {

  sealed trait Message

  case class Process(row: Seq[Domino]) extends Message
  case class Try(acc: List[Domino], ends: Map[Int, Int], curRow: Seq[Domino], level: Int) extends Message
  case class Result(result: Boolean) extends Message


}
