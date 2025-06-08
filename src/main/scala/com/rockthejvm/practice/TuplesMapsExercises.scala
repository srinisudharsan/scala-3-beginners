package com.rockthejvm.practice

import scala.annotation.tailrec
import scala.language.postfixOps

object TuplesMapsExercises {

  var socialNetwork: Map[String, Set[String]] = Map()

  def addPerson(name: String):Unit = {
    if(!socialNetwork.contains(name)) {
      socialNetwork = socialNetwork + (name -> Set())
    }
  }

  def addFriend(person1: String, person2: String): Unit = {
    if(!socialNetwork.contains(person1))
      addPerson(person1)
    if(!socialNetwork.contains(person2))
      addPerson(person2)
    socialNetwork = socialNetwork.updated(person1, socialNetwork(person1) + person2)
    socialNetwork = socialNetwork.updated(person2, socialNetwork(person2) + person1)
  }

  def unfriend(person1: String, person2: String): Unit = {
    socialNetwork = socialNetwork.updated(person1, socialNetwork(person1) - person2)
    socialNetwork = socialNetwork.updated(person2, socialNetwork(person2) - person1)
  }

  def removePerson(person: String): Unit = {
    socialNetwork = socialNetwork.map((k,v) => (k, v - person))
    socialNetwork = socialNetwork - person
  }

  def peopleWith0Friends: Int = socialNetwork.count((k,v) => v.isEmpty)
  def nFriends(person: String): Int = if (socialNetwork.contains(person)) socialNetwork(person).size else -1
  def maxFriends(): Set[String] = {
    var peopleToNumFriends = socialNetwork.map((k, v) => (k, v.size))
    var maxFriends = 0
    var peopleWithMaxFriends: Set[String] = Set()
    for{
      personToNumFriends <- peopleToNumFriends
    }yield{
      if (personToNumFriends._2 > maxFriends) {
        maxFriends = personToNumFriends._2
        peopleWithMaxFriends = Set(personToNumFriends._1)
      } else if (personToNumFriends._2 == maxFriends)
        peopleWithMaxFriends  = peopleWithMaxFriends + personToNumFriends._1
    }
    peopleWithMaxFriends
  }

  def hasSocialConnection(person1: String, person2: String): Boolean = {

    @tailrec
    def hasSocialConnectionHelper(person1: String, person1RunningSet: Set[String],
                                  person2: String, person2RunningSet: Set[String],
                                  visitedPersons: Set[String]): Boolean = {
      var visitedPersonsNew:Set[String] = visitedPersons + person1 + person2
      if(socialNetwork(person1).isEmpty || socialNetwork(person2).isEmpty)
        return false
      if (socialNetwork(person1).contains(person2))
        return true
      if (person1RunningSet.intersect(person2RunningSet).nonEmpty)
        return true
      if(socialNetwork(person1).subsetOf(visitedPersons) && socialNetwork(person2).subsetOf(visitedPersons))
        return false
      var person1RunningSetNew = person1RunningSet | socialNetwork(person1)
      var person2RunningSetNew = person2RunningSet | socialNetwork(person2)
      hasSocialConnectionHelper(person1RunningSetNew.diff(visitedPersonsNew).head,
        person1RunningSetNew,
        person2RunningSetNew.diff(visitedPersonsNew).head,
        person2RunningSetNew,
        visitedPersonsNew)
    }
    if(!socialNetwork.contains(person2) || !socialNetwork.contains(person1))
      return false
    hasSocialConnectionHelper(person1, Set(), person2, Set(), Set())
  }



  def main(args: Array[String]): Unit = {

    addPerson("Sudh")
    addPerson("Rag")
    addFriend("Sudh", "Neej")
    addFriend("Sudh", "Rag")
    addFriend("Rag", "Deep")
    addFriend("Rag", "Sar")
    addPerson("Sat")
    addFriend("Deep", "Nid")
    println("Nfriends" + nFriends("Rag"))
    println("MaxFriends" + maxFriends())
    println("0 Friends" + peopleWith0Friends)
    println(hasSocialConnection("Sudh", "Nid"))
    println(hasSocialConnection("Sudh", "Sat"))

  }

}
