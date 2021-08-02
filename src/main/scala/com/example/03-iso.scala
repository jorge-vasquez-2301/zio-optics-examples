package com.example

import zio.optics._

object Isos {
  final case class Color(rgb: Int)
  object Color {
    val iso: Iso[Color, Int] =
      Iso(
        color => Right(color.rgb),
        rgb => Right(Color(rgb))
      )
  }

  val color1 = Color(255)

  // Get the rgb value of color1
  val rgb1: Either[Nothing, Int] = Color.iso.get(color1)
  // Right(255)

  // Create a Color with rgb=4
  val color2: Either[Nothing, Color] = Color.iso.set(4)
  // Right(Color(4))

  val identity = Iso.identity
}
