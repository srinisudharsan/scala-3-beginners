package com.rockthejvm.part2oop

import scala.annotation.tailrec
import scala.collection.immutable


object Exceptions {

  def doSoCrash(): Unit = {
    def doSoCrashInternal(i: Int): Int = {
      if (i == 100000)
        return i
      1 + doSoCrashInternal(i+1)
    }
    doSoCrashInternal(1)
  }

  def doOomCrash(): Unit = {
    def doOomCrashInternal(i: Int): Int = {
      if (i == 1000)
        return i
      val list = List.tabulate(10000000)(i => "Let's mae a pretty long string" + i)
      println(list.length)
      1 + doOomCrashInternal(i + 1)
    }

    doOomCrashInternal(1)
  }

  def main(args: Array[String]): Unit = {
    //doSoCrash()
    doOomCrash()
  }

}
