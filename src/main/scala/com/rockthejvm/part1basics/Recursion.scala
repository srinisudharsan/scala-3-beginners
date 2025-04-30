package com.rockthejvm.part1basics

import scala.annotation.tailrec


@tailrec
def factorialv2tailrec(num: Int, accumulator: Int): Int = {
  if(num <= 0) 0
  else if(num == 1) accumulator
  else factorialv2tailrec(num-1, accumulator*num)
}

@tailrec
def sumBetween(a: Int, b: Int, accumulator: Int): Int = {
  if(a > b) 0
  else if(a == b) accumulator
  else sumBetween(a, b-1, accumulator + b)
}

def concat(str: String, n: Int): String = {
  @tailrec
  def concatTailRec(str: String, n: Int, output: String): String = {
    if(n <=0 ) output
    else concatTailRec(str, n - 1, output + str);
  }
  concatTailRec(str, n, "");
}

def fibnocciv2(num: Int): Int = {
  def fibnocciTailRec(num: Int, idx: Int, curr: Int, prev: Int): Int = {
    if (num <= 0) 0
    else if (num == 1) 1
    else if (num == 2) 1
    else if(idx == num) curr
    else fibnocciTailRec(num, idx + 1, curr+prev, curr)
  }
  fibnocciTailRec(num, 3, 2, 1)
}

object Recursion {
  def main(args: Array[String]): Unit = {
   // println(greeting("Varsha", 3))
    println(factorialv2tailrec(12, 1))
    println(sumBetween(2, 5, 0))
    println(concat("abc", 3))
    println(fibnocciv2(6))
  }
}
