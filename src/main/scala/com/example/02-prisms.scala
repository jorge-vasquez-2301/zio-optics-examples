package com.example

import zio.optics._

object Prisms {
  sealed trait Json
  object Json {
    final case object JNull                     extends Json
    final case class JStr(v: String)            extends Json
    final case class JNum(v: Double)            extends Json
    final case class JArr(v: Vector[Json])      extends Json
    final case class JObj(v: Map[String, Json]) extends Json

    type JNull = JNull.type

    val jNull: Prism[Json, JNull] =
      Prism(
        {
          case json: JNull => Right(json)
          case _           => Left(OpticFailure("Not a JNull"))
        },
        Right(_)
      )

    val jStr: Prism[Json, JStr] =
      Prism(
        {
          case json: JStr => Right(json)
          case _          => Left(OpticFailure("Not a JStr"))
        },
        Right(_)
      )

    val jNum: Prism[Json, JNum] =
      Prism(
        {
          case json: JNum => Right(json)
          case _          => Left(OpticFailure("Not a JNum"))
        },
        Right(_)
      )

    val jArr: Prism[Json, JArr] =
      Prism(
        {
          case json: JArr => Right(json)
          case _          => Left(OpticFailure("Not a JArr"))
        },
        Right(_)
      )

    val jObj: Prism[Json, JObj] =
      Prism(
        {
          case json: JObj => Right(json)
          case _          => Left(OpticFailure("Not a JObj"))
        },
        Right(_)
      )
  }

  val json1 = Json.JNum(100)
  val json2 = Json.JStr("hello")

  // Get a JNum from json1
  val jNum1: Either[OpticFailure, Json.JNum] = Json.jNum.get(json1)
  // Right(JNum(100.0))

  // Get a JNum from json2
  val jNum2: Either[OpticFailure, Json.JNum] = Json.jNum.get(json2)
  // Left(OpticFailure(Not a JNum))

  // Set Json to JStr, notice we don't need the whole!
  val json3: Either[Nothing, Json] = Json.jStr.set(Json.JStr("some text"))
  // Right(JStr(some text))

  // Update json1 duplicating its value
  val json4: Either[OpticFailure, Json] = Json.jNum.update(json1) {
    case Json.JNum(x) => Json.JNum(x * 2)
  }
  // Right(JNum(200.0))

  // Update json2 duplicating its value
  val json5: Either[OpticFailure, Json] = Json.jNum.update(json2) {
    case Json.JNum(x) => Json.JNum(x * 2)
  }
  // Left(OpticFailure(Not a JNum))

  val cons  = Prism.cons  // Access the :: case of a List
  val left  = Prism.left  // Access the Left case of an Either
  val right = Prism.right // Access the Right case of an Either
  val none  = Prism.none  // Access the None case of an Option
  val some  = Prism.some  // Access the Some case of an Option
}
