package org.zewx.parseq

import cats.Foldable
import cats.syntax.validated._
import cats.data.Validated
import cats.instances.string._
import cats.instances.map._
import cats.instances.list._

import scala.collection.immutable
import scala.util.Try

object ValidatedDemo {

  type Data = (String, String)

  type Validation[A] = Validated[List[String], A]

  def isEmail(name: String)(data: immutable.Map[String, String]): Validation[Data] = {
    data.get(name).map(value =>
      (name -> value).valid[List[String]].ensure(List(s"Parameter [$name] should contain letter [@]"))(_ => value.contains("@")))
      .getOrElse(List(s"Parameter [$name] is required").invalid[Data])
  }

  def isPhone(name: String)(data: immutable.Map[String, String]): Validation[Data] = {
    data.get(name).map(value =>
      (name -> value).valid[List[String]].ensure(List(s"Parameter [$name] should contain digits only"))(_ => Try(value.toInt).fold(_ => false, _ => true)))
      .getOrElse(List(s"Parameter [$name] is required").invalid[Data])
  }

  def main(args: Array[String]): Unit = {

    val data = immutable.Map("email" -> "a@b.com", "phone" -> "123")

    val checks: List[immutable.Map[String, String] => Validation[Data]] = List(
      isEmail("email"),
      isPhone("phone")
    )

    val result = checks.map(check => check(data).map(immutable.Map(_)))
    Foldable[List].fold(result).bimap(_.foreach(println), println(_))
  }
}
