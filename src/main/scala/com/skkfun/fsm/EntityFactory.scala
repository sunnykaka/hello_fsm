package com.skkfun.fsm

import akka.actor.ActorRefFactory

import scala.collection.mutable.ListBuffer

/**
 * User: Liub
 * Date: 2014/12/22
 */
class EntityFactory(val championProperty: Property, val goblinPropertyList: List[Property]) {
  def createChampion(context: ActorRefFactory) = {
    context.actorOf(Champion.props(championProperty))
  }

  def createGoblins(context: ActorRefFactory) = {
    ListBuffer(goblinPropertyList.map((p) => context.actorOf(Goblin.props(p))) : _*)
  }
}

object EntityFactory {
}
