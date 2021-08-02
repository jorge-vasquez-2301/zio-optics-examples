package com.example

import zio.Chunk
import zio.optics._

object Traversals {
  val filterEvenNumbers: Traversal[Chunk[Int], Int] = Traversal.filter(_ % 2 == 0)

  val items = Chunk.fromIterable(1 to 10)

  // Get all even numbers from items
  val evenNumbers = filterEvenNumbers.get(items)
  // Right(Chunk(2,4,6,8,10))
}
