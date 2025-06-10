package com.rockthejvm.part4scalapower

import scala.annotation.tailrec

object PatternMatching {

  /**
    * Exercise
    * show(Sum(Number(2), Number(3))) = "2 + 3"
    * show(Sum(Sum(Number(2), Number(3)), Number(4)) = "2 + 3 + 4"
    * show(Prod(Sum(Number(2), Number(3)), Number(4))) = "(2 + 3) * 4"
    * show(Sum(Prod(Number(2), Number(3)), Number(4)) = "2 * 3 + 4"
    */

  sealed trait Expr

  case class Number(n: Int) extends Expr

  case class Sum(e1: Expr, e2: Expr) extends Expr

  case class Prod(e1: Expr, e2: Expr) extends Expr
  
  def show(expr: Expr): String = expr match {
    case Sum(e1, e2) => "(" + show(e1) + "+" + show(e2) + ")"
    case Prod(e1, e2) => "(" + show(e1) + "*" + show(e2) + ")"
    case Number(n) => s"$n"
  }
  def main(args: Array[String]): Unit = {
    println(show(Prod(Sum(Number(1),Number(2)), Prod(Number(3), Number(4)))))
  }

}
