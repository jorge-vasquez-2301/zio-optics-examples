package com.example

import zio.optics._

object PolymorphicOptics {
  final case class Item[C <: Currency](description: String, price: BigDecimal, currency: C)
  object Item {
    def currency[Old <: Currency, New <: Currency]: ZLens[Item[Old], Item[New], Old, New] =
      ZLens(
        item => Right(item.currency),
        newCurrency => item => Right(item.copy(currency = newCurrency))
      )
  }

  sealed trait Currency
  object Currency {
    case object Dollar extends Currency
    case object Euro   extends Currency

    type Dollar = Dollar.type
    type Euro   = Euro.type
  }

  val item1: Item[Currency.Dollar] = Item("Some book", 10.0, Currency.Dollar)

  // Get `currency` from item1
  val currency1: Either[Nothing, Currency.Dollar] = Item.currency.get(item1)
  // Right(Currency.Dollar)

  // Change `currency` in item1 to Euro
  val item2: Either[Nothing, Item[Currency.Euro]] = Item.currency.set(Currency.Euro)(item1)
  // Right(Item(Some book,10.0,Euro))
}
