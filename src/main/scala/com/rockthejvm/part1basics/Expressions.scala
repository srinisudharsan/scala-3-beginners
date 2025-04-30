package com.rockthejvm.part1basics

object Expressions {
  val expVal = 40 + 2
  val anIf = if (expVal == 43) 34 else 45
  val aCodeBlock = {
    val a = 10
    val b = 15
    val c = 32
    c + b
  }

  val someValue = {
    2 < 3
  }
  val newValue = if (someValue) 35 else 98

  val printVal = println("Scala")

  def main(args: Array[String]): Unit = {
    println(expVal);
    println(anIf);
    println(aCodeBlock)
    println(someValue)
    println(newValue)
    println(printVal)
  }

}
