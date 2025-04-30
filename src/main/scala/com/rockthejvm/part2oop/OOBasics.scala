package com.rockthejvm.part2oop

object OOBasics {

  // .age won't work without val
  class Person(val name: String, age: Int = 0) {
    val cloneAge: Int = age
    def greet(guestName: String): Unit = {
      println(s"Hello $guestName, I am ${this.name}. I am ${this.cloneAge} years old")
    }

    def this(name: String)= {
      this(name, 1)
    }
  }

  val aPerson = new Person("V1", 2)
  def main(args: Array[String]): Unit = {
    println(aPerson.name)
    println(aPerson.greet("Friend"))
  }

}
