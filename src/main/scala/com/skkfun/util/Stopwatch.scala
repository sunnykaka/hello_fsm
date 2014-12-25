package com.skkfun.util

trait Stopwatch {
  type Elapsed = () => Long

  def start(): Elapsed
}

object Stopwatch extends Stopwatch {

  def start(): Elapsed = {
    val timeFn = () => System.currentTimeMillis()
    val off = timeFn()
    () => timeFn() - off
  }

}