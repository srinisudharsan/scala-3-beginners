package com.rockthejvm.practice

abstract class Maybe[A] {
  def map[B](transformer: A=>B):  Maybe[B]
  def filter(predicate: A=>Boolean): Maybe[A]
  def withFilter(predicate: A=>Boolean): Maybe[A] = filter(predicate)
  def flatMap[B](transformer: A=>Maybe[B]): Maybe[B]
  def isEmpty: Boolean
}

class EmptyMayBe[A]() extends Maybe[A]{

  override def isEmpty = true

  override def map[B](transformer: A => B): Maybe[B] = new EmptyMayBe[B]

  override def filter(predicate: A => Boolean): Maybe[A] = this

  override def flatMap[B](transformer: A => Maybe[B]): Maybe[B] = new EmptyMayBe[B]
}

class MayBeImpl[A](val value:A) extends Maybe[A]{

  override def map[B](transformer: A => B): Maybe[B] =  new MayBeImpl[B](transformer(value))

  override def filter(predicate: A => Boolean): Maybe[A] = if (predicate(value)) this else new EmptyMayBe[A]()

  override def flatMap[B](transformer: A => Maybe[B]): Maybe[B] = transformer(value)

  override def isEmpty: Boolean = false
}
