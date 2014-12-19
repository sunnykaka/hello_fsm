package com.skkfun.fsm

import akka.actor.{ActorSystem}
import org.slf4j.LoggerFactory
import util.Random

/**
 * User: Liub
 * Date: 2014/12/15
 */
object Main extends App{

  val log = LoggerFactory.getLogger(classOf[Main])

  log.info("start")

  val xerathProperty = Property(2000, 100, 700, "xerath");
  val r = Random
  val goblinPropertyList = List.tabulate(5)(n => Property(r.nextInt(1000) + 500, r.nextInt(100) + 200, r.nextInt(100) + 200, "goblin" + n));

  val system = ActorSystem()

  val sceneManager = system.actorOf(SceneManager.props(xerathProperty, goblinPropertyList))

  sceneManager ! Start

}

class Main