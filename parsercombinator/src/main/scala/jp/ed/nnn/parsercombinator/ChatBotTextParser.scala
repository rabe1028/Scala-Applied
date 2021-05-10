package jp.ed.nnn.parsercombinator

import scala.::
import scala.util.parsing.combinator._

object ChatBotTextParser extends JavaTokenParsers {

  def chatBot: Parser[ChatBot] =  "(" ~ "chatbot" ~ commandList ~ ")" ^^ {t => ChatBot(t._1._2)}

  def commandList: Parser[List[Command]] = rep(command)

  def command: Parser[Command] = replyCommand | timeCommand | dateCommand

  def replyCommand: Parser[ReplyCommand] =
    "(" ~ "reply" ~ string ~ replyList ~ ")" ^^ {t => ReplyCommand(t._1._1._2.r, t._1._2) }

  def replyList: Parser[List[String]] = "(" ~ rep(string) ~ ")" ^^ { t => t._1._2}

  def timeCommand: Parser[TimeCommand] =
    "(" ~ "time" ~ string ~ digits ~ digits ~ string ~ replyList ~ ")" ^^
      {t => TimeCommand(
        t._1._1._1._1._1._2.r,
        t._1._1._1._1._2.toInt,
        t._1._1._1._2.toInt,
        t._1._1._2,
        t._1._2) }

  def dateCommand: Parser[DateCommand] =
    (("(" ~> "date" ~> string) ~ month ~ day ~ string ~ replyList) <~ ")" ^^
      {t => DateCommand(
        t._1._1._1._1.r,
        t._1._1._1._2,
        t._1._1._2,
        t._1._2,
        t._2
      )
    }

  def digits: Parser[String] = "[0-9]+".r

  def month: Parser[Int] = ("[0-9]+".r | "1[0-2]+".r) ^^ {_.toInt}

  def day: Parser[Int] = ("[0-9]+".r | "1[0-9]+".r | "2[0-9]+".r | "3[0-1]+".r) ^^ {_.toInt}

  def string: Parser[String] =  stringLiteral ^^ { s => s.substring(1, s.length - 1) }

  def apply(input: String): ParseResult[ChatBot] = parseAll(chatBot, input)

}