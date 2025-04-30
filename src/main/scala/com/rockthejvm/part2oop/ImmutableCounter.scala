package com.rockthejvm.part2oop

object ImmutableCounter {
  def main(args: Array[String]): Unit = {
    val counter = new Counter(5)
    val incCounter = counter.increment(1)
    val decCounter = counter.decrement(1)
    counter.print()
    incCounter.print()
    decCounter.print()
    counter.increment(1).print()
    counter.increment(1)
    counter.print()
  }

}

class Counter(initCount: Int) {
  def increment(n: Int): Counter = new Counter(initCount + n)
  def decrement(n: Int): Counter = new Counter(initCount - n)
  def print(): Unit ={
    println(this.initCount)
  }
}
