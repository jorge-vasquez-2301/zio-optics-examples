package com.example

import zio.optics.toptics._
import zio.stm._

object EffectfulTransactionalOptics {
  val tmap: USTM[TMap[String, Either[String, Int]]] =
    TMap.make(
      ("test1", Left("hello")),
      ("test2", Right(1))
    )

  def updateMap(tmap: TMap[String, Either[String, Int]], key: String): STM[String, TMap[String, Either[String, Int]]] =
    for {
      optionEither <- tmap.get(key)
      either       <- STM.fromOption(optionEither) <> STM.fail(s"tmap does not contain key $key")
      _ <- either match {
            case Left(_)    => STM.fail("Not an Int")
            case Right(int) => tmap.put(key, Right(int + 1))
          }
    } yield tmap

  def updateMap2(
    tmap: TMap[String, Either[String, Int]],
    key: String
  ): STM[OpticFailure, TMap[String, Either[String, Int]]] = {
    val optic: Optional[TMap[String, Either[String, Int]], Int] = TOptics.key(key) >>> Optic.right
    optic.update(tmap)(_ + 1)
  }
}
