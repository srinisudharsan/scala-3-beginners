package com.rockthejvm.part2oop

object MethodNotations {
  def main(args: Array[String]): Unit = {
    val mary = new Person("mary", "inception", 20)
    println((mary+"loose").name)
    println((mary+"loose").favMovie)
    val plusMary = +mary
    println(plusMary.name)
    println(plusMary.favMovie)
    println(plusMary.age)
    println(mary(2))
  }
}

class Person(val name: String, val favMovie: String, val age: Int) {
  def +(nickName: String): Person = new Person(s"$name the $nickName", favMovie, age)
  def unary_+ : Person = new Person(this.name, favMovie, age+1)
  def apply(mvTime: Int): String = s"$name watched $favMovie $mvTime Times"
}
