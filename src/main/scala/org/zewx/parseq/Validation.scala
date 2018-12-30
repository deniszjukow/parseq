package org.zewx.parseq

import cats.Foldable
import cats.data.{Kleisli, NonEmptyList}
import cats.instances.either._
import cats.instances.list._
import cats.instances.map._
import cats.instances.string._
import cats.syntax.either._


object Validation {

  type Error = (String, String)
  type ErrorOr[A] = Either[NonEmptyList[Error], A]
  type Data = Map[String, String]
  type KeyValue = (String, String)
  type Check = Kleisli[ErrorOr, KeyValue, KeyValue]

  def predicateCheck(predicate: String => Boolean, errorMessage: String): Check = Kleisli[ErrorOr, KeyValue, KeyValue] {
    case (k, v) =>
      if (predicate(v)) (k -> v).rightNel[Error]
      else (k, errorMessage).leftNel[KeyValue]
  }

  def allDigits: Check = predicateCheck(_.matches("[0-9]+"), "must be all digits")

  def startsWith(v: String): Check = predicateCheck(_.startsWith(v), s"must start with [$v]")

  def endsWith(v: String): Check = predicateCheck(_.endsWith(v), s"must end with [$v]")

  def notEmpty: Check = predicateCheck(_.nonEmpty, s"must not be empty")

  def contains(v: String): Check = predicateCheck(_.contains(v), s"must contain [$v]")

  type Validator = List[(String, Check)] => Data => ErrorOr[Data]

  def validator(checks: List[(String, Check)])(data: Map[String, String]): ErrorOr[Data] = {
    val listOfValidated = checks.map {
      case (name, check) =>
        data.get(name).map { value =>
          check.run(name -> value).map { case (n, v) => Map(n -> v) }
        }.getOrElse {
          (name -> "missing value").leftNel[Data]
        }.toValidated
    }

    Foldable[List].fold(listOfValidated).toEither
  }

  def main(args: Array[String]): Unit = {

    val data = Map(
      "phone" -> "179",
      "email" -> "a@b.com"
    )

    val checks: List[(String, Check)] = List(
      ("phone", notEmpty andThen allDigits andThen startsWith("1") andThen endsWith("9")),
      ("email", notEmpty andThen contains("@"))
    )

    val v: Validator = validator
    println(v(checks)(data))
  }
}
