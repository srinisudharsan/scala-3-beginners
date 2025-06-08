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

  def withFilter(predicate: A => Boolean): LlistFunc[A] = filter(predicate)

  def flatMap[B](transformer: A => LlistFunc[B]): LlistFunc[B]

  def foreach(lambda: A => Unit): Unit = {
    @tailrec
    def foreachHelper(list: LlistFunc[A]): Unit = {
      if (!list.isEmpty) {
        lambda(list.head)
        foreachHelper(list.tail)
      }
    }

    foreachHelper(this)
  }

  def sort(comparator: (A,A) => Int): LlistFunc[A]
  def zipWith[B](listToZip: LlistFunc[A], lambda: (A,A) => B) : LlistFunc[B]
  def foldLeft[B](start: B)(lambda: (A,B)=>B): B
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
  override def sort(comparator: (A, A) => Int): LlistFunc[A] = this
  override def zipWith[B](listToZip: LlistFunc[A], lambda: (A, A) => B): LlistFunc[B] = new EmptyListFunc[B]()

  override def foldLeft[B](start: B)(lambda: (A, B) => B): B = throw new NoSuchElementException()
}

case class LlistImplFunc[A](head: A, tail: LlistFunc[A]) extends LlistFunc[A] {
  def isEmpty: Boolean = false

  override def filter(predicate: A => Boolean): LlistFunc[A] = {
    // simpler way wihtout reverse
    // if (predicate.test(header)) new LlistImpl(head, tail.filter(predicate))
    @tailrec
    def filterHelper(currNode: LlistFunc[A], currFilteredList: LlistFunc[A]): LlistFunc[A] = {
      if (currNode.isEmpty)
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

  override def flatMap[B](transformer: A => LlistFunc[B]) = {
    @tailrec
    def flatMapHelper(currNode: LlistFunc[A], currFlatMappedList: LlistFunc[B]): LlistFunc[B] = {
      if (currNode.isEmpty)
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
    if (currList.isEmpty)
      return newList
    reverseList(currList.tail, newList.add(currList.head))
  }

  override def sort(comparator: (A, A) => Int): LlistFunc[A] = {
    //list so far will be in reverse order.
    // eventually reverse the list
    def insertIntoCleanList(cleanList: LlistFunc[A], listSoFar: LlistFunc[A], elem: A): LlistFunc[A] = {
      // we reached the end of the list meaning elem should be at the last. Make it the head and reverse
      if (cleanList.isEmpty)
        return reverseList(new LlistImplFunc[A](elem, listSoFar), new EmptyListFunc[A]())
      // We found the place where elem needs to be inserted
      // in reverse list funciton, new list is the clean reversed one. Consider clean list aas newList and
      // list so far as the list to be reversed
      if (comparator(cleanList.head, elem) > 0)
        return reverseList(new LlistImplFunc[A](elem, listSoFar), cleanList)
      // keep searching
      insertIntoCleanList(cleanList.tail, new LlistImplFunc[A](cleanList.head, listSoFar), elem)
    }

    @tailrec
    def sortHelper(cleanList: LlistFunc[A], currList: LlistFunc[A]): LlistFunc[A] = {
      if (currList.isEmpty)
        return cleanList
      sortHelper(insertIntoCleanList(cleanList, new EmptyListFunc[A](), currList.head), currList.tail)
    }

    sortHelper(new LlistImplFunc[A](this.head, new EmptyListFunc[A]), this.tail)
  }

  override def zipWith[B](listToZip: LlistFunc[A], lambda: (A, A) => B): LlistFunc[B] = {

    @tailrec
    def zipWithHelper(listToZip: LlistFunc[A], currList: LlistFunc[A], zippedList: LlistFunc[B]): LlistFunc[B] = {
      if((currList.isEmpty && !listToZip.isEmpty) || (!currList.isEmpty && listToZip.isEmpty))
        throw new IllegalArgumentException("List to zip size does not match")
      if (currList.isEmpty)
        return reverseList(zippedList, new EmptyListFunc[B]())
      zipWithHelper(listToZip.tail, currList.tail, new LlistImplFunc[B](lambda(listToZip.head, currList.head), zippedList))
    }

    zipWithHelper(listToZip, this, new EmptyListFunc[B])
  }

  override final def foldLeft[B](start: B)(lambda: (A, B) => B): B = {
    if(this.tail.isEmpty)
     return  lambda(this.head, start)
    this.tail.foldLeft(lambda(this.head, start))(lambda)
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

    var myList_v3 = new EmptyListFunc[Int]().add(1).add(3).add(4).add(2).add(4)

    var sortedList = myList_v3.sort((x: Int,y: Int) => x-y)
    println("Sorted List: foreach: "+ sortedList.foreach(x=>println(x)))
    println("Sorted List: "+ sortedList.toString())
    var toZipList = new EmptyListFunc[Int]().add(1).add(2).add(3).add(4).add(4).sort((x,y)=>x-y)
    println("ToZipList: " + toZipList.toString())
    var zippedList = sortedList.zipWith(toZipList, (x,y) => x*y)
    println("ZippedList " + zippedList.toString())
    var toFold = new EmptyListFunc[Int]().add(1).add(2).add(3).add(4).sort((x,y)=>x-y)
    println("Folded: "+toFold.foldLeft(0)((x,y)=>x+y))

    for{
      sorted <- sortedList if sorted % 2 == 0
      tozip <- toZipList if tozip % 2 == 0
    }println(s"$sorted:$tozip")
  }
}
