package com.rockthejvm.part3fp

var superAdder : Int => Int => Int = x => y => x+y;
object AnonFunc {
  def main(args: Array[String]): Unit = {
    var superAdder_1 = superAdder(2)
    println(superAdder_1)
    println(superAdder_1(4))
  }
}
