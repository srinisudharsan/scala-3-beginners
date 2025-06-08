package com.rockthejvm.part3fp

object HocAndCurry {


  def toCurry(nonCurriedFunc: (Int, Int)=> Int) : Int => Int => Int= {
    x=>y=>nonCurriedFunc(x,y)
  }

  def unCurry(curried: Int=>Int=>Int): (Int, Int) => Int = (x,y)=>curried(x)(y)

  def compose(f: Int=>Int, g: Int=>Int) : Int=>Int = (x)=> f(g(x))
  def andThen(f: Int=>Int, g: Int=>Int) : Int=>Int = (x)=> g(f(x))

  def main(args: Array[String]): Unit = {
    var nonCurriedFunc: (Int, Int)=>Int = (x,y)=>x+y
    var curried = toCurry(nonCurriedFunc)
    toCurry(nonCurriedFunc)
    println
  }
}
