package com.rockthejvm.practice

import scala.annotation.tailrec

abstract class Llist {
  def head: Int
  def tail: Llist
  def isEmpty: Boolean
  def add(element: Int): Llist
  override def toString() = super.toString
}

class LlistImpl(headVal: Int = -1, tailVal: Llist = null) extends Llist {
  def isEmpty: Boolean = (headVal == -1 && tailVal == null)
  def add(element: Int): Llist = {
    new LlistImpl(element, this)
  }
  def head: Int = headVal
  def tail: Llist = tailVal
  override def toString(): String = {
    if (isEmpty) {
      return "List is null"
    }
    toStringHelper("", this)
  }

  @tailrec
  private def toStringHelper(runningString: String, currNode: Llist): String = {
    if (currNode.isEmpty) {
      return s"$runningString -> |"
    } else if (currNode.tail == null) {
      return s"$runningString -> ${currNode.head} -> |"
    }
    toStringHelper(if (runningString == "") s"${currNode.head}" else s"$runningString -> ${currNode.head}", currNode.tail)
  }
}

object LlistTest {
  def main(args: Array[String]): Unit = {
    var myList: Llist = new LlistImpl()
    println("Created a new list")
    println("List Empty: " + myList.isEmpty)
    println("List: " + myList.toString())
    myList = myList.add(1).add(2).add(3)
    println("Added 3 elements")
    println("List Empty: " + myList.isEmpty)
    println("List: " + myList.toString())
  }
}

