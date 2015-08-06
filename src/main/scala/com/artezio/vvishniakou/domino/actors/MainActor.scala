package com.artezio.vvishniakou.domino.actors

import akka.actor.{Actor, Props}
import akka.event.Logging
import com.artezio.vvishniakou.domino.actors.Messages.{Process, Result, Try}

class MainActor extends Actor {

  private var countChilds = 0
  private var res = false
  val log = Logging(context.system, this)

  def receive = {
    case Process(row) =>
      for (domino <- row) {
        val child = context.actorOf(Props[TryActor], "TryActor_" + countChilds)
        countChilds += 1
        val tryMessage = Try(List(domino), domino.getEnds, row.takeWhile(_ != domino) ++ row.dropWhile(_ != domino).tail, 0)
        child ! tryMessage
      }
    case Result(result) =>
      res = res || result
      countChilds -= 1
      if (countChilds == 0) {
        log.info("Result - " + res)
         ActorDominoApp.shutdown()
      }

  }
}
