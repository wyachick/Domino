package com.artezio.vvishniakou.domino.actors

import akka.actor.{Actor, Props}
import akka.event.Logging
import com.artezio.vvishniakou.domino.actors.Messages.{Result, Try}
import com.artezio.vvishniakou.domino.model._

class TryActor extends Actor {
  private var countChilds = 0
  private var res = false
  val log = Logging(context.system, this)

  def receive = {
    case Try(acc, ends, curRow, level) =>
      if (curRow.isEmpty)
        sender() ! Result(true)
      else
      if (curRow.forall(
      {
        case Pair(l, r) => ends(l) == 0 && ends(r) == 0
        case Double(n) => ends(n) == 0
      }
      ))
        sender() ! Result(false)
      else {
        for (domino <- curRow) {
          domino match {
            case Pair(l, r) =>
              if (ends(l) != 0) {

                val child = context.actorOf(Props[TryActor], self.path.name + "_" + countChilds)
                countChilds += 1
                val newEnds = for ((k, v) <- ends) yield {
                  if (k == l) (k, v - 1)
                  else
                  if (k == r) (k, v + 1)
                  else
                    (k, v)
                }
                val tryMessage = Try(domino :: acc, newEnds, curRow.takeWhile(_ != domino) ++ curRow.dropWhile(_ != domino).tail, level + 1)
                child ! tryMessage
              }
              if (ends(r) != 0) {
                val child = context.actorOf(Props[TryActor], self.path.name + "_" + countChilds)
                countChilds += 1
                val newEnds = for ((k, v) <- ends) yield {
                  if (k == r) (k, v - 1)
                  else
                  if (k == l) (k, v + 1)
                  else
                    (k, v)
                }
                val tryMessage = Try(domino :: acc, newEnds, curRow.takeWhile(_ != domino) ++ curRow.dropWhile(_ != domino).tail, level + 1)
                child ! tryMessage
              }

            case Double(n) =>
              if (acc.exists(_.contain(n))) {
                val child = context.actorOf(Props[TryActor], self.path.name + "_" + countChilds)
                countChilds += 1
                val newEnds = for ((k, v) <- ends) yield {
                  if (k == n) (k, v + 2)
                  (k, v)
                }
                val tryMessage = Try(domino :: acc, newEnds, curRow.takeWhile(_ != domino) ++ curRow.dropWhile(_ != domino).tail, level + 1)
                child ! tryMessage
              }
          }
        }
      }

    case Result(result) =>
      res = res || result
      countChilds -= 1
      if (countChilds == 0) {
        context.actorSelection(self.path.parent) ! Result(res)
      }

  }
}
