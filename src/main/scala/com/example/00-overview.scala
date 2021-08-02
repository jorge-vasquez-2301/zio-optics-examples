package com.example

object Example1 {

  // Example 1.a
  final case class Person(fullName: String, address: Address)
  final case class Address(city: String, street: Street)
  final case class Street(name: String, number: Int)

  def setStreetNumber(person: Person, newStreetNumber: Int): Person =
    person.copy(
      address = person.address.copy(
        street = person.address.street.copy(
          number = newStreetNumber
        )
      )
    )

  // Example 1.b
  final class MutablePerson(var fullName: String, var address: MutableAddress)
  final class MutableAddress(var city: String, var street: MutableStreet)
  final class MutableStreet(var name: String, var number: Int)

  def setStreetNumberMutable(person: MutablePerson, newStreetNumber: Int): Unit =
    person.address.street.number = newStreetNumber

  def setStreetNumberMutableNull(person: MutablePerson, newStreetNumber: Int): Unit =
    if (person != null) {
      if (person.address != null) {
        if (person.address.street != null) {
          person.address.street.number = newStreetNumber
        } else {
          throw new Exception("Null street!")
        }
      } else {
        throw new Exception("Null address!")
      }
    } else {
      throw new Exception("Null person!")
    }

  // Example 1.c
  final case class OPerson(fullName: Option[String], address: Option[OAddress])
  final case class OAddress(city: Option[String], street: Option[OStreet])
  final case class OStreet(name: Option[String], number: Option[Int])

  def setStreetNumberOptional(person: OPerson, newStreetNumber: Int): Either[String, OPerson] =
    person.address match {
      case Some(address) =>
        address.street match {
          case Some(street) =>
            Right(
              OPerson(
                fullName = person.fullName,
                address = Some(OAddress(address.city, Some(OStreet(street.name, Some(newStreetNumber)))))
              )
            )
          case None => Left("Empty street")
        }
      case None => Left("Empty address")
    }
}

object Example2 {
  final case class Order(itemId: Long, quantity: Long)
  final case class Customer(name: String, orders: Map[Long, Order])

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
  ): Either[String, Map[Long, Customer]] =
    customers.get(customerId) match {
      case Some(customer) =>
        customer.orders.get(orderId) match {
          case Some(order) =>
            Right(
              customers.updated(
                customerId,
                customer.copy(
                  orders = customer.orders.updated(
                    orderId,
                    order.copy(quantity = newQuantity)
                  )
                )
              )
            )
          case None =>
            Left(s"Order with ID $orderId does not exist")
        }
      case None =>
        Left(s"Customer with ID $customerId does not exist")
    }
}
