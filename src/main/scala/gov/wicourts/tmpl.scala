package gov.wicourts.tmpl

import scala.language.higherKinds
import scalaz.Applicative
import scalaz.Bind
import scalaz.Contravariant

trait Coder[F[_]] {

  // Primitives
  def boolean: F[Boolean]
  def int: F[Int]
  def decimal: F[BigDecimal]
  def string: F[String]

  def list[A](a: F[A]): F[List[A]]
  def option[A](a: F[A]): F[Option[A]]

  // Select a subfield of an object
  def field[A](name: String, a: F[A]): F[A]

}

trait CanFail[F[_], E] {
  def fail[A](err: E): F[A]
}

trait Decoder[F[_]]
    extends Coder[F]
    with CanFail[F, String]
    with Applicative[F]
    with Bind[F] {

  // Helper function for decoding enums
  final def enum[A](
    descr: String,
    values: (String, A)*
  ): F[A] =
    bind(string)(v =>
      values
        .collectFirst { case (s, e) if s == v => point(e) }
        .getOrElse(fail(s"This value must be a valid $descr"))
    )

}

trait Encoder[F[_]]
    extends Coder[F]
    with Contravariant[F]
