package com.rockthejvm.practice

import scala.annotation.tailrec

trait Predicate[A] {
  def test(valToTest: A): Boolean
}

trait Transformer[A,B] {
  def transform(value: A): B
}

// Comments on how the list implementation differs from the one in course
// The course uses a separate class extending Llist to definte an empty list where head=null, tail=null, isEmpty=true etc
// The to string method defines a helper method within itself

abstract class Llist[A] {
  def head: A
  def tail: Llist[A]
  def isEmpty: Boolean
  def add(element: A): Llist[A] = {
    LlistImpl(element, this)
  }
  def map[B](transformer: Transformer[A,B]): Llist[B]
  def filter(predicate: Predicate[A]) : Llist[A]
  def flatMap[B](transformer: Transformer[A,Llist[B]]): Llist[B]
}

// Consumers have to be careful
case class EmptyList[A]() extends Llist[A]{
  override def isEmpty: Boolean = true
  override def head:A = throw new NoSuchElementException
  override def tail:Llist[A] = throw new NoSuchElementException
  override def toString():String = "Null"
  override def map[B](transformer: Transformer[A,B]): Llist[B] = throw new NoSuchMethodException
  override def filter(predicate: Predicate[A]): Llist[A] = throw new NoSuchMethodException
  override def flatMap[B](transformer: Transformer[A, Llist[B]]): Llist[B] = throw new NoSuchMethodException
}

case class LlistImpl[A](head: A, tail: Llist[A]) extends Llist[A] {
  def isEmpty: Boolean = false

  override def filter(predicate: Predicate[A]): Llist[A] = {
    // simpler way wihtout reverse
  // if (predicate.test(header)) new LlistImpl(head, tail.filter(predicate))
    @tailrec
    def filterHelper(currNode: Llist[A], currFilteredList: Llist[A]): Llist[A] = {
      if(currNode.isEmpty)
        return currFilteredList
      filterHelper(currNode.tail, if (predicate.test(currNode.head)) currFilteredList.add(currNode.head) else currFilteredList)
    }

    reverseList(filterHelper(this, EmptyList[A]()), EmptyList[A]())
  }

  override def map[B](transformer: Transformer[A,B]): Llist[B] = {
    // simpler way without reverse
    // new LlistImpl(transformer.transform(head), tail.map(transformer))
    @tailrec
    def mapHelper(currNode: Llist[A], currMappedList: Llist[B]): Llist[B] = {
      if (currNode.isEmpty)
        return currMappedList

      mapHelper(currNode.tail, currMappedList.add(transformer.transform(currNode.head)))
    }

    reverseList(mapHelper(this, new EmptyList[B]()), new EmptyList[B]())
  }

  override def flatMap[B](transformer: Transformer[A, Llist[B]])={
    @tailrec
    def flatMapHelper(currNode: Llist[A], currFlatMappedList: Llist[B]): Llist[B] = {
      if(currNode.isEmpty)
        return currFlatMappedList
      val transformedList = transformer.transform(currNode.head)

      @tailrec
      def concatList(mainList: Llist[B], listToAdd: Llist[B]): Llist[B] = {
        if (listToAdd.isEmpty)
          return mainList
        concatList(mainList.add(listToAdd.head), listToAdd.tail)
      }

      flatMapHelper(currNode.tail, concatList(currFlatMappedList, reverseList(transformedList, new EmptyList[B])))
    }

    reverseList(flatMapHelper(this, new EmptyList[B]), new EmptyList[B])
  }

  override def toString(): String = {
    if (isEmpty) {
      return "List is null"
    }
    toStringHelper("", this)
  }

  @tailrec
  private def toStringHelper(runningString: String, currNode: Llist[A]): String = {
    if (currNode.isEmpty) {
      return s"$runningString -> |"
    } else if (currNode.tail == null) {
      return s"$runningString -> ${currNode.head} -> |"
    }
    toStringHelper(if (runningString == "") s"${currNode.head}" else s"$runningString -> ${currNode.head}", currNode.tail)
  }

  @tailrec
  private def reverseList[B](currList: Llist[B], newList: Llist[B]): Llist[B] = {
    if(currList.isEmpty)
      return newList
    reverseList(currList.tail, newList.add(currList.head))
  }
}

class EvenPredicate extends Predicate[Int] {
  override def test(valToTest: Int): Boolean = ((valToTest % 2) == 0)
}

class Divideby5Predicate extends Predicate[Int] {
  override def test(valToTest: Int): Boolean = ((valToTest % 5) == 0)
}

class StringToIntTransformer extends Transformer[String, Int]{
  override def transform(value: String): Int = value.toInt
}

class Doubler extends Transformer[Int, Int]{
  override def transform(value: Int): Int = value*2
}

class PlusOneTransformer extends Transformer[Int, Llist[Int]]{
  override def transform(value: Int): Llist[Int] = new EmptyList[Int]().add(value+1).add(value)

}

object LlistTest {
  def find[A](list: Llist [A], predicate: Predicate[A]): A = {
    if (list.isEmpty)
      throw new RuntimeException("Element Not found")
    if (predicate.test(list.head))
      return list.head
    find(list.tail, predicate)
  }

  def main(args: Array[String]): Unit = {
    var myList: Llist[Int] = new EmptyList[Int]()
    println("Created a new list")
    println("List Empty: " + myList.isEmpty)
    println("List: " + myList.toString())
    var myList_v2 = myList.add(1).add(2).add(3).add(4)
    println("Added 3 elements")
    println("List Empty: " + myList_v2.isEmpty)
    println("List: " + myList_v2.toString())

    // testing filter
    var evenList = myList_v2.filter(new EvenPredicate())
    println("Filtered List: " + evenList.toString())

    //testing doubler
    var doubleList = myList_v2.map(new Doubler)
    println("Doubled List: "+doubleList.toString())

    // testing map
    var strList = new EmptyList[String]().add("1").add("2").add("3")
    println("Str List: " + strList.toString())
    var intList = strList.map(new StringToIntTransformer())
    println("Int List: " + intList.toString())

    //testing flatmap
    var plusOne = myList_v2.flatMap(new PlusOneTransformer())
    println("Plus One transformer: "+ plusOne.toString())

    // testing exceptions
    println(find(myList_v2, new EvenPredicate))
    println(find(myList_v2, new Divideby5Predicate))

  }
}

