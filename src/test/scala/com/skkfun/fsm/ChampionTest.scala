package com.skkfun.fsm

import akka.actor.{Actor, FSM, ActorRef}
import akka.testkit.TestFSMRef
import akka.util.Timeout
import concurrent.duration._


/**
 * User: Liub
 * Date: 2014/12/22
 */
class ChampionTest extends AbstractTest{

  implicit val timeout = Timeout(5.seconds)

  trait PlayerBuilder {
    val p = new Property(1000, 100, 100, "测试1")
    val player = PlayerWrapper(TestFSMRef(new Champion(p)))
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

      //send Win event
      player ! Win
      player.stateName shouldBe Patrol

      //send ReadyForBattle event again
      player.sendReadyForBattle()
      player.underlyingActor.hp shouldBe hpLeft
      player.checkSelectSkill()
      //send FightResult event
      hpLeft = player.checkFightResult(-800, isExpectDead = false, hpLeft)

      //hp <= 20% maxHp, backhaul
      player ! Win
      player.stateName shouldBe Backhaul

      player ! FSM.StateTimeout
      player.stateName shouldBe Bath

      player.takeBath()

    })

  }

  //"player lose in battle"
  "player lose in battle" should "act2" in new PlayerBuilder {

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

      //send SelectSkill event again,
      player.checkSelectSkill()
      //send FightResult event, player died in this round
      hpLeft = player.checkFightResult(-1000, isExpectDead = true, hpLeft)

      //send Lose event
      player ! Lose
      player.stateName shouldBe Died

      player ! FSM.StateTimeout
      player.stateName shouldBe Bath

      player.takeBath()

    })

  }


  case class PlayerWrapper(player: TestFSMRef[State, Data, Champion]) {

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

    def takeBath(): Unit = {

      val i = util.Random.nextInt(2)
      if(i % 2 == 0) {
        //Player received ReadForBattle Event while in Bath in a half prob.
        player ! ReadyForBattle
        player.stateName shouldBe Bath
      }

      var event: BaseEvent = receiveOne(0.seconds).asInstanceOf[BaseEvent]
      while(event == null) {
        player ! FSM.StateTimeout
        event = receiveOne(0.seconds).asInstanceOf[BaseEvent]
      }

      if(i % 2 == 0) {
        event shouldBe a [PlayerReady]
        player.stateName shouldBe Battle
        //set state to Patrol, so next loop can start normally
        player.setState(Patrol)
      } else {
        event shouldBe a [ChampionRevive.type]
        player.stateName shouldBe Patrol
      }
      player.underlyingActor.alive shouldBe true
      player.underlyingActor.hp shouldBe property.maxHp

    }

  }

  implicit def wrapperToPlayer(playerWrapper: PlayerWrapper): TestFSMRef[State, Data, Champion] = playerWrapper.player

}
