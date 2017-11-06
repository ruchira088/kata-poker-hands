import cards.{CardSuite, CardValue}

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object TestApp
{
  def main(args: Array[String]): Unit =
  {
    println(CardValue.parseCardValue("10"))
  }
}
