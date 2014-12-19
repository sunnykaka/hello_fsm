package com.skkfun.fsm

import akka.actor._
import com.twitter.util.Stopwatch
import concurrent.duration._
import scala.collection.mutable.ListBuffer

/**
 * User: Liub
 * Date: 2014/12/15
 */
class SceneManager(championProperty: Property, goblinPropertyList: List[Property]) extends FSM[State, Data]{

//  val log = Logging(context.system, this)

  val champion = context.actorOf(Champion.props(championProperty))
  val goblins = ListBuffer(goblinPropertyList.map((p) => context.actorOf(Goblin.props(p))) : _*)
  val r = util.Random
  var goblinInBattle: ActorRef = null
  val sw = Stopwatch.start()

  log.debug("我们的英雄: " + championProperty)
  log.debug("我们的敌人数量: " + goblinPropertyList.size)
  if(log.isDebugEnabled) goblinPropertyList.foreach((p: Property) => log.debug(p.toString))

  startWith(Waiting, Uninitialized)

  when(Waiting) {
    case Event(Start, _) =>
      goto(Ready) using Enemy(None)
  }

  when(Ready, 15.seconds) {
    case Event(StateTimeout, enemy) =>
      def isMeetEnemy() = r.nextInt(10) <= 9

      if(isMeetEnemy()) {
        goblinInBattle = enemy match {
          case Enemy(Some(goblin: ActorRef)) => goblin
          case Enemy(None) => goblins.remove(r.nextInt(goblins.size))
        }
        log.debug("let's start a battle")
        goto(CoordinateBattle) using FightBattle(None, None)
      } else {
        log.debug("patrolling...")
        stay
      }

  }


  when(CoordinateBattle) {

    case Event(PlayerReady(player, property), FightBattle(None, None, _)) =>
      if(player == champion) {
        stay using FightBattle(Some(PlayerWrapper(player, property)), None)
      } else if(player == goblinInBattle) {
        stay using FightBattle(None, Some(PlayerWrapper(player, property)))
      } else {
        log.error("error message where")
        stay
      }

    case Event(PlayerReady(_, _), FightBattle(None, Some(playerWrapper), _)) =>
      goto(InBattle) using FightBattle(Some(PlayerWrapper(champion, championProperty)), Some(playerWrapper))

    case Event(PlayerReady(_, goblinProperty), FightBattle(Some(playerWrapper), None, _)) =>
      goto(InBattle) using FightBattle(Some(playerWrapper), Some(PlayerWrapper(goblinInBattle, goblinProperty)))


//    case Event(PlayerReady(`champion`, championProperty), FightBattle(None, None, 1)) =>
//      stay using FightBattle(Some(PlayerWrapper(champion, championProperty)), None)
//
//    case Event(PlayerReady(`goblinInBattle`, goblinProperty), FightBattle(None, None)) =>
//      stay using FightBattle(None, Some(PlayerWrapper(goblinInBattle, goblinProperty)))
//
//    case Event(PlayerReady(`champion`, championProperty), FightBattle(None, Some(PlayerWrapper(goblinInBattle, goblinProperty)))) =>
//      goto(InBattle) using FightBattle(Some(PlayerWrapper(champion, championProperty)), Some(PlayerWrapper(goblinInBattle, goblinProperty)))
//
//    case Event(PlayerReady(`goblinInBattle`, goblinProperty), FightBattle(Some(PlayerWrapper(champion, championProperty)), None)) =>
//      goto(InBattle) using FightBattle(Some(PlayerWrapper(champion, championProperty)), Some(PlayerWrapper(goblinInBattle, goblinProperty)))
  }

  when(InBattle) {
    case Event(SkillSelected(skill), fightBattle: FightBattle) =>
      def calculateFightResult() = {
        val championWrapper = fightBattle.championWrapper.get
        val goblinWrapper = fightBattle.goblinWrapper.get
        val championDamage = championWrapper.skill.attack(championWrapper.property, goblinWrapper.skill)
        val goblinDamage = goblinWrapper.skill.attack(goblinWrapper.property, championWrapper.skill)
        log.debug("%s使用了%s技能, 对%s造成了%d伤害".format(championWrapper.property.name, championWrapper.skill, goblinWrapper.property.name, championDamage))
        log.debug("%s使用了%s技能, 对%s造成了%d伤害".format(goblinWrapper.property.name, goblinWrapper.skill, championWrapper.property.name, goblinDamage))
        (championDamage, goblinDamage)
      }

      if(sender == champion) {
        fightBattle.championWrapper.get.skill = skill
      } else if(sender == goblinInBattle) {
        fightBattle.goblinWrapper.get.skill = skill
      }
      if(fightBattle.isAllSkillReceived) {
        log.debug("===============================================")
        log.debug("Round %d start!".format(fightBattle.round))
        val (championDamage, goblinDamage) = calculateFightResult()
        fightBattle.championWrapper.get.lastHpChangeValue = -goblinDamage
        fightBattle.goblinWrapper.get.lastHpChangeValue = -championDamage

        goto(WaitForFightResultReceived)

      } else {

        stay
      }

  }

  when(WaitForFightResultReceived) {
    case Event(FightResultReceived(hp, headCount, dead), fightBattle: FightBattle) =>

      def gameOver(championWin: Boolean): State ={
        val championName = fightBattle.championWrapper.get.property.name
        log.debug("===============================================")
        log.debug("===============================================")
        log.debug("游戏结束! 用时: %d ms, %s战斗结果: %s".format(sw().inMilliseconds, championName, if(championWin) "胜利" else "失败"))
        if(championWin) {
          log.debug("%s还剩余%d血量".format(championName, fightBattle.championWrapper.get.hp))
        }
        log.debug("%s斩杀了%d个敌人".format(championName, fightBattle.championWrapper.get.headCount))

        stop()
      }

      val championWrapper = fightBattle.championWrapper.get
      val goblinWrapper = fightBattle.goblinWrapper.get
      if(sender == champion) {
        championWrapper.dead = Some(dead)
        championWrapper.headCount = headCount
        championWrapper.hp = hp
      } else if(sender == goblinInBattle) {
        goblinWrapper.dead = Some(dead)
        goblinWrapper.headCount = headCount
        goblinWrapper.hp = hp
      }
      if(fightBattle.isAllFightResultReceived) {
        val championDead = championWrapper.dead.get
        val goblinDead = goblinWrapper.dead.get
        log.debug("Round %d Over,用时%d ms".format(fightBattle.round, fightBattle.spendTime))
        if(!championDead && !goblinDead) {
          log.debug("本局没有决出胜负, %s剩余血量%d, %s剩余血量%d".format(championWrapper.property.name, championWrapper.hp, goblinWrapper.property.name, goblinWrapper.hp))
          goto(InBattle) using fightBattle.nextRound()

        } else {
          if(championDead) {
            log.debug("%s壮烈牺牲".format(championWrapper.property.name))
            championWrapper.player ! Lose
            goblinWrapper.player ! Win
          }
          if(goblinDead) {
            log.debug("哥布林%s挂了".format(goblinWrapper.property.name))
            goblinWrapper.player ! Lose
            championWrapper.player ! Win
          }
          var s: State = null
          if(goblinDead && goblins.isEmpty) {
            s = gameOver(true)
          } else if(championDead) {
            s = goto(WaitForChampionRevive) using Enemy( if(goblinDead) None else Some(goblinWrapper.player) )
          } else {
            s = goto(Ready) using Enemy(None)
          }
          s
        }
      } else {

        stay using fightBattle
      }


  }

  when(WaitForChampionRevive) {
    case Event(ChampionRevive, _) =>
      log.debug("%s复活啦".format(championProperty.name))
      goto(Ready)
  }

  onTransition {
    case Ready -> CoordinateBattle =>
      champion ! ReadyForBattle
      goblinInBattle ! ReadyForBattle

    case CoordinateBattle -> InBattle =>
      (nextStateData: @unchecked) match {
        case FightBattle(Some(PlayerWrapper(champion, championProperty)), Some(PlayerWrapper(goblinInBattle, goblinProperty)), _) =>
          champion ! SelectSkill
          goblinInBattle ! SelectSkill
      }

    case InBattle -> WaitForFightResultReceived =>
      (nextStateData: @unchecked) match {
        case FightBattle(Some(championWrapper @ PlayerWrapper(champion, championProperty)), Some(goblinWrapper @ PlayerWrapper(goblinInBattle, goblinProperty)), _) =>
          champion ! FightResult(championWrapper.lastHpChangeValue)
          goblinInBattle ! FightResult(goblinWrapper.lastHpChangeValue)
      }

      //TODO 测试这个case能不能和CoordinateBattle -> InBattle合并
    case WaitForFightResultReceived -> InBattle =>
      (nextStateData: @unchecked) match {
        case FightBattle(Some(PlayerWrapper(champion, championProperty)), Some(PlayerWrapper(goblinInBattle, goblinProperty)), _) =>
          champion ! SelectSkill
          goblinInBattle ! SelectSkill
      }

  }

}

object SceneManager {

  def props(champion: Property, goblins: List[Property]): Props = Props(classOf[SceneManager], champion, goblins)


}