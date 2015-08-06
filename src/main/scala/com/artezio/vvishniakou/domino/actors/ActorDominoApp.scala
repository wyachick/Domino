package com.artezio.vvishniakou.domino.actors

import akka.actor.{ActorSystem, Props}
import akka.util.Timeout
import com.artezio.vvishniakou.domino.actors.Messages.Process
import com.artezio.vvishniakou.domino.model.Domino

import scala.concurrent.duration._

object ActorDominoApp extends App{
 implicit val system = ActorSystem("finder-system")
 implicit val timeout = Timeout(5.seconds)

 val mainActor = system.actorOf(Props[MainActor])

 def shutdown(): Unit = {
   system.stop(mainActor)
   system.shutdown()
 }

  mainActor ! Process(Seq(
    Domino(1, 2),
    Domino(1, 1),
    Domino(1, 3),
    Domino(1, 4),
    Domino(3, 4),
    Domino(3, 3)))
}
