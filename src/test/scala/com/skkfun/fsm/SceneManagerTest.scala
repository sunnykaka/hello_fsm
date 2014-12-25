package com.skkfun.fsm

import akka.actor.FSM.StateTimeout
import akka.actor.{ActorRefFactory, ActorRef, FSM}
import akka.testkit.TestFSMRef
import akka.util.Timeout

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration._
import scala.util.Random


/**
 * User: Liub
 * Date: 2014/12/22
 */
class SceneManagerTest extends AbstractTest{

  implicit val timeout = Timeout(5.seconds)

  trait PlayerBuilder {
    val xerathProperty = Property(2000, 100, 700, "xerath");
    val r = Random
    val goblinPropertyList = List.tabulate(5)(n => Property(r.nextInt(1000) + 500, r.nextInt(100) + 200, r.nextInt(100) + 200, "goblin" + n));
    val sceneManager = TestFSMRef(
      new SceneManager(
        new EntityFactory(xerathProperty, goblinPropertyList) {
          override def createChampion(context: ActorRefFactory) = {
            system.actorOf(Champion.props(championProperty))
//            TestFSMRef(new Champion(championProperty))
          }

          override def createGoblins(context: ActorRefFactory) = {
            ListBuffer(goblinPropertyList.map((p) => system.actorOf(Goblin.props(p))): _*)
//            ListBuffer(goblinPropertyList.map((p) => TestFSMRef(new Goblin(p))): _*)
          }
        }
      )
    )
  }

  "player win in battle" should "act" in new PlayerBuilder {

    loop(1)(i => {

      sceneManager ! Start
      sceneManager.stateName shouldBe Ready

//      sceneManager ! StateTimeout
//      while(!sceneManager.stateName.isInstanceOf[CoordinateBattle.type]) {
//        sceneManager ! StateTimeout
//      }

      //pause to waiting sub actor stop
      Thread.sleep(1000000L)

    })

  }


}
