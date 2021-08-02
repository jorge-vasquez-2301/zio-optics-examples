package com.example

import zio.Chunk
import zio.optics._

object ComposingOptics {
  // Example 1
  final case class Person(fullName: String, address: Address)
  object Person {
    val fullName: Lens[Person, String] =
      Lens(
        person => Right(person.fullName),
        newFullName => person => Right(person.copy(fullName = newFullName))
      )

    val address: Lens[Person, Address] =
      Lens(
        person => Right(person.address),
        newAddress => person => Right(person.copy(address = newAddress))
      )
  }

  final case class Address(city: String, street: Street)
  object Address {
    val city: Lens[Address, String] =
      Lens(
        address => Right(address.city),
        newCity => address => Right(address.copy(city = newCity))
      )

    val street: Lens[Address, Street] =
      Lens(
        address => Right(address.street),
        newStreet => address => Right(address.copy(street = newStreet))
      )
  }

  final case class Street(name: String, number: Int)
  object Street {
    val name: Lens[Street, String] =
      Lens(
        street => Right(street.name),
        newName => street => Right(street.copy(name = newName))
      )

    val number: Lens[Street, Int] =
      Lens(
        street => Right(street.number),
        newNumber => street => Right(street.copy(number = newNumber))
      )
  }

  def setStreetNumber(person: Person, newStreetNumber: Int): Either[Nothing, Person] = {
    val streetNumber: Lens[Person, Int] = Person.address >>> Address.street >>> Street.number
    streetNumber.set(newStreetNumber)(person)
  }

  // Example 2
  val streetNumber: Lens[Person, Int]                      = Person.address >>> Address.street >>> Street.number
  val fullNameAndStreetNumber: Lens[Person, (String, Int)] = Person.fullName zip streetNumber

  // Example 3
  final case class OPerson(fullName: Option[String], address: Option[OAddress])
  object OPerson {
    val fullName: Lens[OPerson, Option[String]] =
      Lens(
        person => Right(person.fullName),
        newFullName => person => Right(person.copy(fullName = newFullName))
      )

    val address: Lens[OPerson, Option[OAddress]] =
      Lens(
        person => Right(person.address),
        newAddress => person => Right(person.copy(address = newAddress))
      )
  }
  final case class OAddress(city: Option[String], street: Option[OStreet])
  object OAddress {
    val city: Lens[OAddress, Option[String]] =
      Lens(
        address => Right(address.city),
        newCity => address => Right(address.copy(city = newCity))
      )

    val street: Lens[OAddress, Option[OStreet]] =
      Lens(
        address => Right(address.street),
        newStreet => address => Right(address.copy(street = newStreet))
      )
  }
  final case class OStreet(name: Option[String], number: Option[Int])
  object OStreet {
    val name: Lens[OStreet, Option[String]] =
      Lens(
        street => Right(street.name),
        newName => street => Right(street.copy(name = newName))
      )

    val number: Lens[OStreet, Option[Int]] =
      Lens(
        street => Right(street.number),
        newNumber => street => Right(street.copy(number = newNumber))
      )
  }

  def setStreetNumberOptional(person: OPerson, newStreetNumber: Int): Either[OpticFailure, OPerson] = {
    val streetNumber =
      OPerson.address >>> Prism.some >>>
        OAddress.street >>> Prism.some >>>
        OStreet.number >>> Prism.some
    streetNumber.set(newStreetNumber)(person)
  }

  // Example 4
  sealed trait ContactInfo
  object ContactInfo {
    final case class Phone(number: Int) extends ContactInfo
    object Phone {
      val number: Lens[Phone, Int] =
        Lens(
          phone => Right(phone.number),
          newNumber => phone => Right(phone.copy(number = newNumber))
        )
    }
    final case class Email(address: String) extends ContactInfo
    object Email {
      val address: Lens[Email, String] =
        Lens(
          email => Right(email.address),
          newAddress => phone => Right(phone.copy(address = newAddress))
        )
    }

    val phone: Prism[ContactInfo, Phone] =
      Prism(
        {
          case p: Phone => Right(p)
          case _        => Left(OpticFailure("Not a Phone"))
        },
        Right(_)
      )

    val email: Prism[ContactInfo, Email] =
      Prism(
        {
          case e: Email => Right(e)
          case _        => Left(OpticFailure("Not an Email"))
        },
        Right(_)
      )
  }

  val phoneNumber: Optional[ContactInfo, Int]     = ContactInfo.phone >>> ContactInfo.Phone.number
  val emailAddress: Optional[ContactInfo, String] = ContactInfo.email >>> ContactInfo.Email.address

  // Example 5
  val phoneNumberOrEmailAddress = phoneNumber orElse emailAddress

  // Example 6
  final case class Order(itemId: Long, quantity: Long)
  object Order {
    val quantity: Lens[Order, Long] =
      Lens(
        order => Right(order.quantity),
        newQuantity => order => Right(order.copy(quantity = newQuantity))
      )
  }

  final case class Customer(name: String, orders: Map[Long, Order])
  object Customer {
    val orders: Lens[Customer, Map[Long, Order]] =
      Lens(
        customer => Right(customer.orders),
        newOrders => customer => Right(customer.copy(orders = newOrders))
      )
  }

  val customers: Map[Long, Customer] = Map(
    1000L -> Customer("Mary Lopez", Map(1L    -> Order(1, 100), 2L -> Order(2, 200))),
    2000L -> Customer("David Adams", Map(1L   -> Order(3, 300), 2L -> Order(4, 400))),
    3000L -> Customer("Brian Johnson", Map(1L -> Order(5, 500), 2L -> Order(6, 600)))
  )

  def setQuantity(
    customers: Map[Long, Customer],
    customerId: Long,
    orderId: Long,
    newQuantity: Long
  ): Either[OpticFailure, Map[Long, Customer]] = {
    val quantityOptic =
      Optic.key[Long, Customer](customerId) >>> Customer.orders >>> Optic.key(orderId) >>> Order.quantity
    quantityOptic.set(newQuantity)(customers)
  }

  // Example 7
  def getPeopleNamesFromNewYork(people: Chunk[Person]): Either[OpticFailure, Chunk[String]] = {
    val newYorkNames = Traversal.filter[Person](_.address.city == "New York").foreach(Person.fullName)
    newYorkNames.get(people)
  }

  val people = Chunk(
    Person("John Adams", Address("New York", Street("Some street", 100))),
    Person("Juanita Perez", Address("Los Angeles", Street("Another street", 500))),
    Person("Lucy Smith", Address("New York", Street("Some street", 200))),
    Person("Andrew Johns", Address("San Diego", Street("The street", 600)))
  )

  getPeopleNamesFromNewYork(people)
  // Right(Chunk(John Adams,Lucy Smith))
}
