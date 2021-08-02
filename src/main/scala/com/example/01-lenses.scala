package com.example

import zio.optics._

object Lenses {

  final case class Person(name: String, age: Int)
  object Person {
    val name: Lens[Person, String] =
      Lens(
        person => Right(person.name),
        newName => person => Right(person.copy(name = newName))
      )

    val age: Lens[Person, Int] =
      Lens(
        person => Right(person.age),
        newAge => person => Right(person.copy(age = newAge))
      )
  }

  val person1 = Person("Juanito", 25)

  // Get `name` from person1
  val name1: Either[Nothing, String] = Person.name.get(person1)
  // Right(Juanito)

  // Get `age` from person1
  val age1: Either[Nothing, Int] = Person.age.get(person1)
  // Right(25)

  // Change `name` in person1 to Pepito
  val person2: Either[Nothing, Person] = Person.name.set("Pepito")(person1)
  // Right(Person(Pepito, 25))

  // Change `age` in person1 to 27
  val person3: Either[Nothing, Person] = Person.age.set(27)(person1)
  // Right(Person(Juanito, 27))

  // Increase `age` of person1 by 5
  val person4: Either[Nothing, Person] = Person.age.update(person1)(_ + 5)
  // Right(Person(Juanito, 30))

  val first  = Lens.first  // Get the first element of a Tuple2
  val second = Lens.second // Get the second element of a Tuple2
}
