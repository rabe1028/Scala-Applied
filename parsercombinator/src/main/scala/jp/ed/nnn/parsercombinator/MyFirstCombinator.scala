package jp.ed.nnn.parsercombinator

import scala.annotation.tailrec

abstract class MyFirstCombinator {
  sealed trait ParseResult[+T]
  case class Success[+T](value: T, next: String) extends ParseResult[T]
  case object Failure extends ParseResult[Nothing]

  type Parser[+T] = String => ParseResult[T]

  def string(literal: String): Parser[String] = input => {
    if(input.startsWith(literal)) {
      Success(literal, input.substring(literal.length))
    } else {
      Failure
    }
  }

  def oneOf(chars: Seq[Char]) : Parser[String] = input => {
    if (input.nonEmpty && chars.contains(input.head)) {
      Success(input.head.toString, input.tail)
    } else {
      Failure
    }
  }

  // Challenge 11-2
  def rep[T](parser: Parser[T]): Parser[List[T]] = input => {
    @tailrec
    def parseRec(acc: List[T], i: String): ParseResult[List[T]] = {
      parser(i) match {
        case Success(v, n) => parseRec(acc :+ v, n)
        case Failure => Success(acc, i)
      }
    }

    parseRec(List.empty[T], input)
  }


  /**
   * string parser
   * @param literal 文字列
   * @return
   */
  def s(literal: String): Parser[String] = string(literal)

  def select[T, U >: T](left: => Parser[T], right: => Parser[U]): Parser[U] = input => {
    left(input) match {
      case success@Success(_, _) => success
      case Failure => right(input)
    }
  }

  def combine[T, U](left: Parser[T], right: Parser[U]): Parser[(T,U)] = input =>
    left(input) match {
      case Success(value1, next1) =>
        right(next1) match {
          case Success(value2, next2) =>
            Success((value1, value2), next2)
          case Failure => Failure
        }
      case Failure => Failure
    }

  def map[T, U](parser: Parser[T], function: T => U): Parser[U] = input => {
    parser(input) match {
      case Success(value, next) => Success(function(value), next)
      case Failure => Failure
    }
  }
}
