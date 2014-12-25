package com.skkfun.fsm

import akka.actor._
import akka.testkit.{TestActorRef, TestFSMRef}
import akka.util.Timeout

import scala.concurrent.duration._


/**
 * User: Liub
 * Date: 2014/12/22
 */
class GoblinTest extends AbstractTest{

  implicit val timeout = Timeout(5.seconds)

  trait PlayerBuilder {
    val p = new Property(1000, 100, 100, "测试1")
    val player = PlayerWrapper(TestFSMRef(new Goblin(p)))
  }

  "player win in battle" should "act" in new PlayerBuilder {

    loop(10)(i => {

      //initial state should be Patrol
      player.stateName shouldBe Patrol

      //send ReadyForBattle event
      player.sendReadyForBattle()
      var hpLeft = player.property.maxHp
      //send SelectSkill event
      player.checkSelectSkill()
      //send FightResult event
      hpLeft = player.checkFightResult(-100, isExpectDead = false, hpLeft)

      player.checkSelectSkill()
      hpLeft = player.checkFightResult(-300, isExpectDead = false, hpLeft)

      //send Win event
      player ! Win
      player.stateName shouldBe Patrol

      //send ReadyForBattle event again
      player.sendReadyForBattle()
      hpLeft = player.underlyingActor.hp
      player.checkSelectSkill()
      //send FightResult event
      hpLeft = player.checkFightResult(-800, isExpectDead = false, hpLeft)

      //hp <= 20% maxHp, backhaul
      player ! Win
      player.stateName shouldBe Patrol

    })

  }

  //"player lose in battle"
  "player lose in battle" should "act2" in new PlayerBuilder {

    loop(1)(i => {

      //initial state should be Patrol
      player.stateName shouldBe Patrol

      //send ReadyForBattle event
      player.sendReadyForBattle()
      var hpLeft = player.property.maxHp
      //send SelectSkill event
      player.checkSelectSkill()
      //send FightResult event
      hpLeft = player.checkFightResult(-100, isExpectDead = false, hpLeft)

      //send SelectSkill event again,
      player.checkSelectSkill()
      //send FightResult event, player died in this round
      hpLeft = player.checkFightResult(-1000, isExpectDead = true, hpLeft)



      //send Lose event
      player ! Lose

      val watcher = TestActorRef[WatcherActor](Props(new WatcherActor(player)))
      watcher.underlyingActor.state shouldBe "terminated"

    })

  }

  class WatcherActor(watchee: ActorRef) extends Actor {

    var state = "initial"
    context.watch(watchee)

    override def receive: Receive = {
      case "start" =>
        state = "start"
      case _: Terminated =>
        state = "terminated"
    }

  }


  case class PlayerWrapper(player: TestFSMRef[State, Data, Goblin]) {

    var property: Property = null

    def sendReadyForBattle(): Unit = {
      player ! ReadyForBattle
      player.stateName shouldBe Battle

      expectMsgPF() {
        case PlayerReady(_: ActorRef, p: Property) =>
          property = p
          p.maxHp should be > 0
      }

    }

    def checkSelectSkill(): Unit = {
      player ! SelectSkill
      expectMsgPF() {
        case SkillSelected(skill: Skill) =>
          skill.skillPower(property) should be >= 0
      }
    }

    def checkFightResult(reduceHp: Int, isExpectDead: Boolean, hp: Int): Int = {
      player ! FightResult(reduceHp)
      expectMsgPF() {
        case FightResultReceived(h, _, dead) =>
          val hpLeft = hp + reduceHp
          h shouldBe hpLeft
          dead shouldBe isExpectDead
          hpLeft
      }
    }

  }

  implicit def wrapperToPlayer(playerWrapper: PlayerWrapper): TestFSMRef[State, Data, Goblin] = playerWrapper.player


}
