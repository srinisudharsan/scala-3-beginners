package com.rockthejvm.part3fp

object WhatIsAFn {

  var concat = new Function2[String, String, String] {
    override def apply(v1: String, v2: String): String = s"$v1$v2"
  }


  val calculatorFactory: Int => (Int, Int) => Int = {
    case 1 => (a, b) => a + b
    case 2 => (a, b) => a - b
    case 3 => (a, b) => a * b
    case _ => (a, b) => a / b
  }


  def main(args: Array[String]): Unit = {
    println(concat("Hello", "Friend"))
    println(calculatorFactory(1)(1,2))
    println(calculatorFactory(4)(6,2))
  }

}
