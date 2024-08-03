package com.rockthejvm.part1recap



import com.rockthejvm.part1recap.ContextualAbstractionsScala2.JSONSerializer

import scala.concurrent.duration.DurationInt

object ContextualAbstractionsScala2 {

  case class Person(name: String) {
    def greet(): String = s"Hi, my name is $name"
  }

  implicit class ImpersonableString(name: String)  {
    def greet(): String =
      Person(name).greet()
  }

  val greeting = "Peter".greet() // ImpersonableString("Peter").greet()

  // duration
  34.second

  // implicit arguments and values
  def increment(x: Int)(implicit amount: Int) = x + amount

  implicit val myamount = 100
//  implicit val myamount2 = 100



  def multiply(x: Int)(implicit amount2: Int) = x * amount2



  // more complex logic

  trait JSONSerializer[T] {
    def toJSON(value:T): String
  }

  def convertToJSON[T](value: T)(implicit serializer: JSONSerializer[T]) = {
    serializer.toJSON(value)
  }

  implicit val personSerializer: JSONSerializer[Person] = new JSONSerializer[Person] {
    override def toJSON(person: Person): String = "{\""+"name"+"\":"+s"\"${person.name}\"}"
  }

  // ORRRRR

//  implicit object personSerializer extends JSONSerializer[Person] {
//    override def toJSON(person: Person): String = "{\""+"name"+"\":"+s"\"${person.name}\"}"
//  }

  // now, implicit defs

  implicit def createListSerializer[T](implicit serializer: JSONSerializer[T]): JSONSerializer[List[T]] =
    new JSONSerializer[List[T]] {
    override def toJSON(list: List[T]): String = s"[${list.map(serializer.toJSON).mkString(",")}]"
  }

  // it will detect an implicit personSerializer JSONSerializer[Person]
  // creates an implicit new JSONSerializer[List[Person], when a list of Person, write.

  // imiplicit defs can be used for implicit conversions, not recommended.

  case class Cat(name: String)  {
    def meow(): String = s"meow im $name"
  }

  implicit def stringToCat(word: String): Cat = {
    Cat(word)
  }

  val aCat: Cat = "march"
  "march".meow()

  // best use to do implicit class conversion


  // not nice,

  def main(args: Array[String]): Unit = {

    println(increment(20))
    println(increment(20))

    val john = Person("john")
    val alice = Person("Alice")
    println(convertToJSON(john))
    // convertToJSON(john)(personSerializer) = el json
    println(convertToJSON(List(john,alice)))
  }
}

object TypeClassesScala2 {
  case class Person(name: String, age: Int)

  // 1. Type class definition
  trait JSONSerializer[T] {
    def toJson(value: T): String

  }

  // 2. type class instances

//  implicit val stringSerializer = new JSONSerializer[String] {
//    override def toJson(value: String): String = s"\"$value\""
//  }

  // ORRRRRRR

  implicit object StringSerializer extends JSONSerializer[String] {
    override def toJson(value: String): String = s"\"$value\""
  }

  implicit object IntSerializer extends JSONSerializer[Int] {
    override def toJson(value: Int): String = s"\"$value\""
  }

  implicit object PersonSerializer extends JSONSerializer[Person] {

    override def toJson(value: Person): String =
      s"""
         | {"name: ${value.name}},"
         | {"age: ${value.age}"}
         |""".stripMargin
  }

  implicit def createListSerializer[T](implicit serializer: JSONSerializer[T]): JSONSerializer[List[T]] =
    new JSONSerializer[List[T]] {
      override def toJson(list: List[T]): String = s"[${list.map(serializer.toJson).mkString(",")}]"
    }







  // part 3 offer api, with a conversion


  def ConvertToJson[T](value: T)(implicit serializer: JSONSerializer[T]): String = {
    serializer.toJson(value)
  }

  def ConvertListToJson[T](list: List[T])(implicit serializer: JSONSerializer[T]): String = {
    list.map(serializer.toJson).mkString("[",",","]")
  }



  // part4 - add extension methods to your exiting types.

  object JSONSyntax {
    implicit class JSONSerializable[T](value:T)(implicit serializer: JSONSerializer[T]) {
      def toJson: String = serializer.toJson(value)
    }
  }

  def main(args: Array[String]): Unit = {
    ConvertToJson(Person("john", 30))
    ConvertToJson("asdf")
    ConvertToJson(54)
    ConvertListToJson(
      List(
        Person("john", 30),
        Person("Alice", 20)
      )
    )

    import JSONSyntax.JSONSerializable

    println(Person("john", 30).toJson)
    println(
      List(
      Person("john", 30),
      Person("Alice", 20)
      ).toJson
    )

    1 to 5


  }



}

