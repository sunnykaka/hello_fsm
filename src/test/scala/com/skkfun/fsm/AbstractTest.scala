package com.skkfun.fsm

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.{FlatSpecLike, BeforeAndAfterAll, Matchers}

/**
 * User: Liub
 * Date: 2014/12/22
 */
class AbstractTest extends TestKit(ActorSystem("TestActorSystem")) with FlatSpecLike with Matchers with BeforeAndAfterAll with ImplicitSender {


  override def afterAll() {
    system.shutdown()
  }

  def loop(times: Int)(f: (Int) => Unit) {
    (1 to times).foreach(f)
  }


}
