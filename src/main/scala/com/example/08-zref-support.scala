package com.example

import zio.optics._
import zio.{ Chunk, Ref, Task, ZIO }

object ZRefSupport {
  val chunk: Ref[Chunk[Int]] = ???

  chunk.at(10) // Access the specified index of `chunk`
  chunk.filter(_ % 2 == 0) // Filter elements of a `chunk`
  chunk.slice(2, 5) // Access a slice of `chunk`

  val list: Ref[List[Int]] = ???

  list.cons // Access the `::` case of `list`
  list.head // Access the head of `list`
  list.tail // Access the tail of `list`

  val tuple: Ref[(Int, String)] = ???

  tuple.first  // Access the first element of `tuple`
  tuple.second // Access the second element of `tuple`

  val map: Ref[Map[String, Int]] = ???

  map.key("someKey") // Access the given key of `map`

  val either: Ref[Either[String, Int]] = ???

  either.left  // Access the Left case of `either`
  either.right // Access the Right case of `either`

  val option: Ref[Option[Int]] = ???

  option.none // Access the None case of `option`
  option.some // Access the Some case of `option`
}

object Example extends App {
  type Title       = String
  type Description = String

  val movieDescriptionsByTitleRef: Ref[Map[Title, Description]] = ???

  def updateMovieDescription(title: Title, newDescription: Description): Task[Unit] =
    movieDescriptionsByTitleRef.modify { movieDescriptionsByTitle =>
      movieDescriptionsByTitle.get(title) match {
        case Some(_) =>
          (ZIO.unit, movieDescriptionsByTitle + (title -> newDescription))
        case None =>
          (
            ZIO.fail(new NoSuchElementException(s"Movie with title $title not found")),
            movieDescriptionsByTitle
          )
      }
    }.flatten

  def updateMovieDescription2(title: String, newDescription: String): Task[Unit] =
    movieDescriptionsByTitleRef.key(title).update(_ => newDescription)
}
