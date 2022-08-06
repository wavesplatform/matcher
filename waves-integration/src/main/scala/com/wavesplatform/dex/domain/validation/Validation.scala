package com.wavesplatform.dex.domain.validation

//noinspection ScalaStyle
case class Validation(status: Boolean, labels: Set[String] = Set.empty) {

  def messages(): String = labels.mkString(", ")

  def &&(r: => Validation): Validation =
    if (!this.status) this
    else if (!r.status) r
    else Validation(status = true)

  def :|(l: String): Validation = if (!this.status) copy(labels = labels + l) else this

  def toEither: Either[String, Unit] = if (status) Right(()) else Left(messages())
}

//noinspection ScalaStyle
class ExtendedBoolean(b: => Boolean) {
  def :|(l: String): Validation = Validation(b) :| l
}

case object Validation {

  implicit def booleanOperators(b: => Boolean): ExtendedBoolean = new ExtendedBoolean(b)
  implicit def result2Boolean(x: Validation): Boolean = x.status

  implicit def fromEi[A](ei: Either[String, Unit]): Validation = ei match {
    case Left(err) => Validation(status = false, Set(err))
    case Right(_) => Validation(status = true)
  }

  val success: Validation = Validation(status = true)
  val failure: Validation = Validation(status = false)
  def failure(l: String): Validation = Validation(status = false, Set(l))

}
