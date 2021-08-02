package com.example

import zio.optics._

object Optionals {
  sealed trait ContactInfo
  object ContactInfo {
    final case class Phone(number: Int)     extends ContactInfo
    final case class Email(address: String) extends ContactInfo

    val phoneNumber: Optional[ContactInfo, Int] =
      Optional(
        {
          case Phone(number) => Right(number)
          case _             => Left(OpticFailure("Contact info does not contain a phone number!"))
        },
        newPhoneNumber =>
          contactInfo =>
            contactInfo match {
              case Phone(_) => Right(Phone(newPhoneNumber))
              case _        => Left(OpticFailure("Can't set phone number"))
            }
      )

    val emailAddress: Optional[ContactInfo, String] =
      Optional(
        {
          case Email(address) => Right(address)
          case _              => Left(OpticFailure("Contact info does not contain an email address!"))
        },
        newEmailAddress =>
          contactInfo =>
            contactInfo match {
              case Email(_) => Right(Email(newEmailAddress))
              case _        => Left(OpticFailure("Can't set email address"))
            }
      )
  }

  val contactInfo1 = ContactInfo.Phone(12345)
  val contactInfo2 = ContactInfo.Email("test1@test.com")

  // Get phone number from contactInfo1
  val phoneNumber1: Either[OpticFailure, Int] = ContactInfo.phoneNumber.get(contactInfo1)
  // Right(12345)

  // Get phone number from contactInfo2
  val phoneNumber2: Either[OpticFailure, Int] = ContactInfo.phoneNumber.get(contactInfo2)
  // Left(Contact info does not contain a phone number!)

  // Get email address from contactInfo1
  val emailAddress1: Either[OpticFailure, String] = ContactInfo.emailAddress.get(contactInfo1)
  // Left(Contact info does not contain an email address!)

  // Get email address from contactInfo2
  val emailAddress2: Either[OpticFailure, String] = ContactInfo.emailAddress.get(contactInfo2)
  // Right(test1@test.com)

  // Set new phone number to contactInfo1
  val contactInfo3: Either[OpticFailure, ContactInfo] = ContactInfo.phoneNumber.set(67890)(contactInfo1)
  // Right(ContactInfo.Phone(67890))

  // Set new phone number to contactInfo2
  val contactInfo4: Either[OpticFailure, ContactInfo] = ContactInfo.phoneNumber.set(67890)(contactInfo2)
  // Left(Can't set phone number)

  // Set new email address to contactInfo1
  val contactInfo5: Either[OpticFailure, ContactInfo] = ContactInfo.emailAddress.set("test2@test.com")(contactInfo1)
  // Left(Can't set email address)

  // Set new email address to contactInfo2
  val contactInfo6: Either[OpticFailure, ContactInfo] = ContactInfo.emailAddress.set("test2@test.com")(contactInfo2)
  // Right(ContactInfo.Email("test2@test.com"))

  // Update phone number of contactInfo1, increasing it by 1
  val phoneNumber3: Either[OpticFailure, ContactInfo] = ContactInfo.phoneNumber.update(contactInfo1)(_ + 1)
  // Right(ContactInfo.Phone(12346))

  // Update phone number of contactInfo2, increasing it by 1
  val phoneNumber4: Either[OpticFailure, ContactInfo] = ContactInfo.phoneNumber.update(contactInfo2)(_ + 1)
  // Left(Contact info does not contain a phone number!)

  // Update email address from contactInfo1, setting it to an empty string
  val emailAddress3: Either[OpticFailure, ContactInfo] = ContactInfo.emailAddress.update(contactInfo1)(_ => "")
  // Left(Contact info does not contain an email address!)

  // Update email address from contactInfo2, setting it to an empty string
  val emailAddress4: Either[OpticFailure, ContactInfo] = ContactInfo.emailAddress.update(contactInfo2)(_ => "")
  // Right(ContactInfo.Email(""))

  val head  = Optional.head         // Access the head of a List
  val tail  = Optional.tail         // Access the tail of a List
  val myKey = Optional.key("myKey") // Access a key of a Map
  val third = Optional.at(3)        // Access an index of a ZIO Chunk
}
