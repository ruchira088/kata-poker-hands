package poker.utils

import poker.exceptions.EmptyOptionException

import scala.util.{Failure, Success, Try}

object ScalaUtils
{
  def toTry[A](option: Option[A], exception: => Exception = EmptyOptionException): Try[A] = option match
    {
      case Some(value) => Success(value)
      case None => Failure(exception)
    }

  def sequence[A](list: List[Try[A]]): Try[List[A]] = list match
    {
      case Nil => Success(List.empty)
      case x :: xs => for {
          value <- x
          rest <- sequence(xs)
        } yield value +: rest
    }
}
