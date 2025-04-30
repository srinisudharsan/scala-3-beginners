package com.rockthejvm.part1basics

import scala.annotation.tailrec

def greeting(name: String, age: Int): String = {
  "My name is " + name + " and I am " + age + "years old"
}

def factorial(num: Int): Int = {
  if(num <= 0) 0
  else if(num == 1) 1
  else num * factorial(num - 1)
}

def fibnocci(num: Int): Int = {
  if(num <= 0) 0
  else if(num == 1) 1
  else if(num == 2) 1
  else fibnocci(num - 1) + fibnocci(num - 2)
}

@tailrec
def isPrimeRecurse(num1: Int, num2: Int): Boolean = {
  if (num2 <= 1) true
  else if (num1%num2 == 0) false
  else isPrimeRecurse(num1, num2 - 1)
}

def isPrime(num: Int): Boolean = {
  if(num<=0) false
  else if(num <= 3) false
  else isPrimeRecurse(num, num/2)
}

object Functions {
  def main(args: Array[String]): Unit = {
    println(greeting("Varsha", 3))
    println(factorial(5))
    println(fibnocci(5))
    println(isPrime(4))
    println(isPrime(5))
    println(isPrime(6))
    println(isPrime(7))
  }
}
