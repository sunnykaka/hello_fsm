package com.skkfun.fsm

import akka.actor.{FSM, Props, ActorRef}
import concurrent.duration._


/**
 * User: Liub
 * Date: 2014/12/15
 */
class Goblin(property: Property) extends Entity(property) with FSM[State, Data]{

  override val guard = Guard(1, 80)
  override val magicSkill = MagicSkill(1)
  override val physicalSkill = PhysicalSkill(1)
  override val ult = Ult(3)

  var lastSender: ActorRef = null

  startWith(Patrol, Uninitialized)

  when(Patrol) {
    case Event(ReadyForBattle, _) =>
      lastSender = sender()
      goto(Battle)
  }

  when(Battle) {
    case Event(SelectSkill, _) =>
      val skill = selectSkill()
      lastSender ! SkillSelected(skill)
      stay()

    case Event(FightResult(hpChangeValue), _) =>
      underAttack(hpChangeValue)
      lastSender ! FightResultReceived(hp, headCount, !alive)
      stay()

    case Event(Lose, _) =>
      stop()

    case Event(Win, _) =>
      if(!alive) {
        stop()
      } else {
        log.debug("哥布林胜利,回满血")
        hp = property.maxHp
        goto(Patrol)
      }

  }

  onTransition {
    case Patrol -> Battle =>
      lastSender ! PlayerReady(self, property)

  }



}

object Goblin {
  def props(property: Property): Props = Props(classOf[Goblin], property)
}