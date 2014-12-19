package com.skkfun.fsm

import akka.actor.{ActorLogging, ActorRef}
import com.twitter.util.{Duration, Stopwatch}


//event
sealed trait BaseEvent
case object Lose extends BaseEvent
case object Win extends BaseEvent

case object Start extends BaseEvent
case object ReadyForBattle extends BaseEvent
case object SelectSkill extends BaseEvent
case class PlayerReady(player: ActorRef, property: Property) extends BaseEvent
case class SkillSelected(skill: Skill) extends BaseEvent
case class FightResult(hpChangeValue: Int) extends BaseEvent
case class FightResultReceived(hp: Int, headCount: Int, dead: Boolean) extends BaseEvent
case object ChampionRevive extends BaseEvent

//state
sealed trait State
case object Bath extends State
case object Patrol extends State
case object Battle extends State
case object Died extends State
case object Backhaul extends State

case object Waiting extends State
case object Ready extends State
case object TryMeetEnemy extends State
case object CoordinateBattle extends State
case object InBattle extends State
case object WaitForFightResultReceived extends State
case object WaitForChampionRevive extends State

//data
sealed trait Data
case object Uninitialized extends Data
case class Enemy(enemy: Option[ActorRef]) extends Data
case class FightBattle(championWrapper: Option[PlayerWrapper], goblinWrapper: Option[PlayerWrapper], round: Int = 1) extends Data {

  val sw = Stopwatch.start()
  def spendTime = sw().inMilliseconds

  def isAllSkillReceived = {
    championWrapper.get.skill != null && goblinWrapper.get.skill != null
  }

  def isAllFightResultReceived = {
    championWrapper.get.dead.isDefined && goblinWrapper.get.dead.isDefined
  }

  def nextRound() = {
    FightBattle(
      Some(PlayerWrapper(championWrapper.get.player, championWrapper.get.property)),
      Some(PlayerWrapper(goblinWrapper.get.player, goblinWrapper.get.property)),
      round = round + 1
    )
  }

}
case class PlayerWrapper(player: ActorRef, property: Property) {
  var skill: Skill = null
  var lastHpChangeValue: Int = 0
  var dead: Option[Boolean] = None
  var hp: Int = 0
  var headCount: Int = 0
}



//player skill
abstract class Skill(power: Int) {
  def attack(property: Property, enemySkill: Skill): Int = {
    val damage = skillPower(property) * power
    enemySkill match {
      case guard: Guard => damage * (100 - guard.damageReduce).max(0) / 100
      case _ => damage
    }
  }

  def skillPower(property: Property): Int
}
case class Ult(power: Int) extends Skill(power) {
  override def skillPower(property: Property): Int = property.ad + property.ap
}
case class MagicSkill(power: Int) extends Skill(power) {
  override def skillPower(property: Property): Int = property.ap
}
case class PhysicalSkill(power: Int) extends Skill(power) {
  override def skillPower(property: Property): Int = property.ad
}
case class Guard(power: Int, damageReduce: Int) extends Skill(power) {
  override def skillPower(property: Property): Int = 0
}

/**
 * User: Liub
 * Date: 2014/12/15
 */
abstract class Entity(property: Property) {
  import property._

  var hp: Int = property.maxHp
  var alive: Boolean = true
  var headCount: Int = 0

  val guard: Guard
  val magicSkill: MagicSkill
  val physicalSkill: PhysicalSkill
  val ult: Ult

  override def toString = {
    s"$property, hp: $hp"
  }

  def underAttack(hpChangeValue: Int): Unit = {
    if(hpChangeValue < 0) {
      hp += hpChangeValue
      if(hp <= 0) alive = false
    }
  }


  def selectSkill(): Skill = {
    val guardProb = 20
    val ultProb = ((ap + ad) / 10).max(10).min(20)
    val adOrApProb = 100 - guardProb - ultProb
    val adProb = (adOrApProb * (ad.toDouble / (ad + ap))).toInt
    val apProb = adOrApProb - adProb
    require(guardProb + ultProb + adProb + apProb == 100)

    val prob = util.Random.nextInt(100) + 1
    prob match {
      case p if p <= guardProb => this.guard
      case p if p <= guardProb + ultProb => this.ult
      case p if p <= guardProb + ultProb + adProb => this.physicalSkill
      case p if p <= guardProb + ultProb + adProb + apProb => this.magicSkill
    }
  }

}

case class Property(maxHp: Int, ad: Int, ap: Int, name: String) {
  override def toString = {
    s"name: $name, maxHp: $maxHp, ad: $ad, ap: $ap"
  }

}
