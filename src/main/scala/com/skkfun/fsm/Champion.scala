package com.skkfun.fsm

import akka.actor.{ActorRef, FSM, Props}
import concurrent.duration._

/**
 * User: Liub
 * Date: 2014/12/15
 */
class Champion(property: Property) extends Entity(property) with FSM[State, Data]{
  import property._

  var lastSender: ActorRef = null
  override val guard = Guard(1, 80)
  override val magicSkill = MagicSkill(1)
  override val physicalSkill = PhysicalSkill(1)
  override val ult = Ult(3)
  var bossCalledYou = false

  def healSelfInBath(): Unit = {
    hp = (hp + (maxHp * 0.05)).toInt.min(maxHp)
  }

  def isHpFull = hp == maxHp

  def needBackhual = {
    hp < maxHp * 0.5
  }

  startWith(Patrol, Uninitialized)

  when(Patrol) {
    case Event(ReadyForBattle, _) =>
      lastSender = sender
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
      log.debug("英雄复活中...")
      goto(Died)
    case Event(Win, _) =>
      headCount += 1
      if(!alive) {
        log.debug("英雄复活中...")
        goto(Died)
      } else if (needBackhual){
        log.debug("英雄回程中...")
        goto(Backhaul)
      } else{
        log.debug("英雄剩余血量%d, 继续战斗".format(hp))
        goto(Patrol)
      }

  }

  when(Died, 30.seconds) {
    case Event(StateTimeout, _) =>
      log.debug("英雄已经复活,正在泡澡...")
      goto(Bath)
  }

  when(Backhaul, 10.seconds) {
    case Event(StateTimeout, _) =>
      log.debug("英雄已经回程,正在泡澡...")
      goto(Bath)
  }

  when(Bath, 1.second) {
    case Event(StateTimeout, _) => {
      val now = hp
      healSelfInBath()
      log.debug("回复生命%d".format(hp - now))
      if (isHpFull) {
        log.debug("生命已满,继续战斗")
        if(bossCalledYou) {
          lastSender ! PlayerReady(self, property)
          goto(Battle)
        } else {
          lastSender ! ChampionRevive
          goto(Patrol)
        }
      }
      else stay()
    }
  }

  onTransition {
    case Patrol -> Battle =>
      bossCalledYou = false
      lastSender ! PlayerReady(self, property)

    case _ -> Bath =>
      alive = true
  }

  whenUnhandled {
    case Event(ReadyForBattle, _) =>
      lastSender = sender
      bossCalledYou = true
      log.debug("泡澡中,勿打扰")
      stay()
  }


}

object Champion {
  def props(property: Property): Props = Props(classOf[Champion], property)
}