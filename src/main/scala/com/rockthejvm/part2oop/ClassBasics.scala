package com.rockthejvm.part2oop

object ClassBasics {
  class Author(val firstName: String, val lastName: String, val yob: Int) {
    def fullName(): String = s"$firstName $lastName"
  }
  class Book(val name: String, val yor: Int, val author: Author) {
    def authorAge(): Int = this.yor - this.author.yob
    def isWrittenBy(author: Author): Boolean = author.fullName().equals(this.author.fullName())
      && author.yob == this.author.yob
    def copy(newYor: Int): Book = new Book(this.name, newYor, this.author)
  }
  def main(args: Array[String]): Unit = {
    val charlesDickens = new Author("Charles", "Dickens", 1920)
    val ernstHemmingway = new Author("Ernst", "Hemmingway", 1910)
    println(charlesDickens.fullName())
    println(charlesDickens.yob)
    val novel = new Book("His book", 1940, charlesDickens)
    println(novel.authorAge())
    println(novel.isWrittenBy(charlesDickens))
    println(novel.isWrittenBy(ernstHemmingway))
    println(novel.yor)
    val newVersion = novel.copy(1942)
    println(newVersion.authorAge())
    println(newVersion.isWrittenBy(charlesDickens))
    println(newVersion.isWrittenBy(ernstHemmingway))
    println(newVersion.yor)

  }

}
