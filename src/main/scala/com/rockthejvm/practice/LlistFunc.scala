package com.rockthejvm.practice

object LlistFunc

import scala.annotation.tailrec

// Comments on how the list implementation differs from the one in course
// The course uses a separate class extending Llist to definte an empty list where head=null, tail=null, isEmpty=true etc
// The to string method defines a helper method within itself
// Only replaced predicate to function. Can replace transform if needed too.

abstract class LlistFunc[A] {
  def head: A
  def tail: LlistFunc[A]
  def isEmpty: Boolean
  def add(element: A): LlistFunc[A] = {
    LlistImplFunc(element, this)
  }
  def map[B](transformer: A => B): LlistFunc[B]
  def filter(predicate: A => Boolean): LlistFunc[A]
  def flatMap[B](transformer: A => LlistFunc[B]): LlistFunc[B]
}

// Consumers have to be careful
case class EmptyListFunc[A]() extends LlistFunc[A]{
  override def isEmpty: Boolean = true
  override def head:A = throw new NoSuchElementException
  override def tail:LlistFunc[A] = throw new NoSuchElementException
  override def toString():String = "Null"
  override def map[B](transformer: A => B): LlistFunc[B] = throw new NoSuchMethodException
  override def filter(predicate: A => Boolean): LlistFunc[A] = throw new NoSuchMethodException
  override def flatMap[B](transformer: A => LlistFunc[B]): LlistFunc[B] = throw new NoSuchMethodException
}

case class LlistImplFunc[A](head: A, tail: LlistFunc[A]) extends LlistFunc[A] {
  def isEmpty: Boolean = false

  override def filter(predicate: A => Boolean): LlistFunc[A] = {
    // simpler way wihtout reverse
    // if (predicate.test(header)) new LlistImpl(head, tail.filter(predicate))
    @tailrec
    def filterHelper(currNode: LlistFunc[A], currFilteredList: LlistFunc[A]): LlistFunc[A] = {
      if(currNode.isEmpty)
        return currFilteredList
      filterHelper(currNode.tail, if (predicate(currNode.head)) currFilteredList.add(currNode.head) else currFilteredList)
    }

    reverseList(filterHelper(this, EmptyListFunc[A]()), EmptyListFunc[A]())
  }

  override def map[B](transformer: A => B): LlistFunc[B] = {
    // simpler way without reverse
    // new LlistImpl(transformer.transform(head), tail.map(transformer))
    @tailrec
    def mapHelper(currNode: LlistFunc[A], currMappedList: LlistFunc[B]): LlistFunc[B] = {
      if (currNode.isEmpty)
        return currMappedList

      mapHelper(currNode.tail, currMappedList.add(transformer(currNode.head)))
    }

    reverseList(mapHelper(this, new EmptyListFunc[B]()), new EmptyListFunc[B]())
  }

  override def flatMap[B](transformer: A => LlistFunc[B])={
    @tailrec
    def flatMapHelper(currNode: LlistFunc[A], currFlatMappedList: LlistFunc[B]): LlistFunc[B] = {
      if(currNode.isEmpty)
        return currFlatMappedList
      val transformedList = transformer(currNode.head)

      @tailrec
      def concatList(mainList: LlistFunc[B], listToAdd: LlistFunc[B]): LlistFunc[B] = {
        if (listToAdd.isEmpty)
          return mainList
        concatList(mainList.add(listToAdd.head), listToAdd.tail)
      }

      flatMapHelper(currNode.tail, concatList(currFlatMappedList, reverseList(transformedList, new EmptyListFunc[B])))
    }

    reverseList(flatMapHelper(this, new EmptyListFunc[B]), new EmptyListFunc[B])
  }

  override def toString(): String = {
    if (isEmpty) {
      return "List is null"
    }
    toStringHelper("", this)
  }

  @tailrec
  private def toStringHelper(runningString: String, currNode: LlistFunc[A]): String = {
    if (currNode.isEmpty) {
      return s"$runningString -> |"
    } else if (currNode.tail == null) {
      return s"$runningString -> ${currNode.head} -> |"
    }
    toStringHelper(if (runningString == "") s"${currNode.head}" else s"$runningString -> ${currNode.head}", currNode.tail)
  }

  @tailrec
  private def reverseList[B](currList: LlistFunc[B], newList: LlistFunc[B]): LlistFunc[B] = {
    if(currList.isEmpty)
      return newList
    reverseList(currList.tail, newList.add(currList.head))
  }
}

var evenPredicateFunc : Int => Boolean = _ % 2 == 0
var stringToIntTransformerFunc :String => Int = _.toInt
var doublerFunc: Int => Int = _ * 2
var plusOneTransformerFunc : Int => LlistFunc[Int] =  x => new EmptyListFunc[Int]().add(x + 1).add(x)

object LlistTestFunc {
  def find[A](list: Llist [A], predicate: Predicate[A]): A = {
    if (list.isEmpty)
      throw new RuntimeException("Element Not found")
    if (predicate.test(list.head))
      return list.head
    find(list.tail, predicate)
  }

  def main(args: Array[String]): Unit = {
    var myList: LlistFunc[Int] = new EmptyListFunc[Int]()
    println("Created a new list")
    println("List Empty: " + myList.isEmpty)
    println("List: " + myList.toString())
    var myList_v2 = myList.add(1).add(2).add(3).add(4)
    println("Added 3 elements")
    println("List Empty: " + myList_v2.isEmpty)
    println("List: " + myList_v2.toString())

    // testing filter
    var evenList = myList_v2.filter(evenPredicateFunc)
    println("Filtered List: " + evenList.toString())

    //testing doubler
    var doubleList = myList_v2.map(doublerFunc)
    println("Doubled List: "+doubleList.toString())

    // testing map
    var strList = new EmptyList[String]().add("1").add("2").add("3")
    println("Str List: " + strList.toString())
    var intList = strList.map(new StringToIntTransformer())
    println("Int List: " + intList.toString())

    //testing flatmap
    var plusOne = myList_v2.flatMap(plusOneTransformerFunc)
    println("Plus One transformer: "+ plusOne.toString())
  }
}
