package org.skyluc.tef

import scala.io.Source
import scala.annotation.tailrec
import scala.util.Try
import scala.util.Failure
import scala.util.Success

object Tef {

  def apply(input: Source): TefDocument = {
    val tokens = tokenize(input)
    val res = createTree(tokens)
    res.get
  }

  private def tokenize(input: Source): List[Token] = {

    def isNameChar(c: Char): Boolean = {
      c != ' ' && c != '\t' && c != '\n'
    }

    def isValueChar(c: Char): Boolean = {
      c != '\n'
    }

    def isWhitespaceChar(c: Char): Boolean = {
      c == ' ' || c == '\t'
    }

    def rightToken(searchPrefix: Boolean, searchValue: Boolean, baseToken: Token, c: Char): Token = {
      if (searchPrefix)
        baseToken
      else if (searchValue)
        TokenValue(c.toString)
      else
        TokenName(c.toString)
    }

    // TODO: make it a stream?
    @tailrec
    def tokenize(searchPrefix: Boolean, searchValue: Boolean, currentToken: Token, tokens: List[Token]): List[Token] = {

      if (input.hasNext) {
        val next = input.next
        currentToken match {
          case TokenName(c) if (isNameChar(next)) =>
            tokenize(true, true, TokenName(c + next), tokens)
          case TokenValue(c) if (isValueChar(next)) =>
            tokenize(true, false, TokenValue(c + next), tokens)
          case TokenWhitespace(c) if (isWhitespaceChar(next)) =>
            tokenize(searchPrefix, searchValue, TokenWhitespace(c + next), tokens)
          case _ =>
            val newTokens = if (currentToken == null) {
              tokens
            } else {
              tokens :+ currentToken
            }
            next match {
              case ' ' =>
                tokenize(searchPrefix, searchValue, TokenWhitespace(" "), newTokens)
              case '\t' =>
                tokenize(searchPrefix, searchValue, TokenWhitespace("\t"), newTokens)
              case '\n' =>
                tokenize(true, false, TokenNewLine, newTokens)
              case '+' =>
                tokenize(false, false, rightToken(searchPrefix, searchValue, TokenElementDecl, next), newTokens)
              case '-' =>
                tokenize(false, false, rightToken(searchPrefix, searchValue, TokenElementEnd, next), newTokens)
              case '.' =>
                tokenize(false, false, rightToken(searchPrefix, searchValue, TokenAttribute, next), newTokens)
              case '#' =>
                tokenize(false, true, rightToken(searchPrefix, searchValue, TokenComment, next), newTokens)
              case '=' =>
                tokenize(false, true, rightToken(searchPrefix, searchValue, TokenAddValue, next), newTokens)
              case _ =>
                tokenize(true, false, if (searchValue)
                  TokenValue(next.toString)
                else
                  TokenName(next.toString), newTokens)
            }
        }
      } else if (currentToken == null) {
        tokens
      } else {
        tokens :+ currentToken
      }
    }

    tokenize(true, false, null, List())
  }

  private def createTree(tokens: List[Token]): Try[TefDocument] = {
    val res = tokens.foldLeft(new ConsumeDocument(Vector()): ConsumeToken)((ct, token) => ct.consume(token))
    res.result
  }

  private abstract class ConsumeToken {
    def consume(token: Token): ConsumeToken
    def result: Try[TefDocument]
  }

  private trait ContainsElements {
    def addElement(element: TefElement): ConsumeToken
  }
  
  private trait ContainsAttributes {
    def addAttribute(attribute: TefAttribute): ConsumeToken
  }
  
  private class ConsumeError(e: Exception) extends ConsumeToken {
    def consume(token: Token) = this
    def result = Failure(e)
  }

  private class ConsumeDocument(elements: Vector[TefElement]) extends ConsumeToken with ContainsElements {

    // from ConsumeToken

    override def consume(token: Token) = token match {
      case TokenElementDecl =>
        new ConsumeElementName(this)
      case w: TokenWhitespace =>
        this
      case TokenNewLine | TokenComment =>
        this
      case _ =>
        new ConsumeError(new Exception("bad"))
    }
    override def result = Success(new TefDocument(elements.to[List]))

    // from ContainsElements

    override def addElement(element: TefElement) = new ConsumeDocument(elements :+ element)
  }
  
  private class ConsumeElementName(container: ContainsElements) extends ConsumeToken {
    def consume(token: Token) = token match {
      case TokenName(name) =>
        new ConsumeElementValue(name, container)
      case w: TokenWhitespace =>
        this
      case _ =>
        new ConsumeError(new Exception("bad"))
    }
    def result = Failure(???)
  }
  
  private class ConsumeElementValue(name: String, container: ContainsElements) extends ConsumeToken {
    def consume(token: Token) = token match {
      case TokenValue(value) =>
        new ConsumeElementAfterValue(name, Some(value), container)
      case TokenNewLine =>
        new ConsumeElementAfterValue(name, None, container)
      case w: TokenWhitespace =>
        this
      case _ =>
        new ConsumeError(new Exception("bad"))
    }
    def result = Failure(???)
  }
  
  private class ConsumeElementAfterValue(name: String, value: Option[String], container: ContainsElements) extends ConsumeToken {
    def consume(token: Token) = token match {
      case TokenAddValue =>
        new ConsumeElementAddValue(name, value, container)
      case TokenElementDecl | TokenAttribute | TokenElementEnd =>
        new ConsumeElementContent(name, value, Vector(), Vector(), container).consume(token)
      case TokenNewLine | TokenComment =>
        this
      case w: TokenWhitespace =>
        this
      case _ =>
        new ConsumeError(new Exception("bad"))
    }
    def result = Failure(???)
  }
  
  private class ConsumeElementAddValue(name: String, value: Option[String], container: ContainsElements) extends ConsumeToken {
    def consume(token: Token) = token match {
      case TokenValue(addValue) =>
        new ConsumeElementAfterValue(name, extendValue(value, addValue), container)
      case TokenNewLine =>
        new ConsumeElementAfterValue(name, extendValue(value), container)
      case _ =>
        new ConsumeError(new Exception("bad"))
    }
    def result = Failure(???)  
  }
  
  private def extendValue(value: Option[String], addValue: String = ""): Some[String] = {
      value match {
      case Some(v) =>
      Some(v + '\n' + addValue)
      case None =>
      Some(addValue)
      }
  }
  
  private class ConsumeElementContent(name: String, value: Option[String], attributes: Vector[TefAttribute], elements: Vector[TefElement], container: ContainsElements) extends ConsumeToken with ContainsElements with ContainsAttributes {

    // from ConsumeToken
    
    def consume(token: Token) = token match {
      case TokenAttribute =>
        new ConsumeAttributeName(this)
      case TokenElementDecl =>
        new ConsumeElementName(this)
      case TokenElementEnd =>
        container.addElement(new TefElement(name, value, attributes.to[List], elements.to[List]))
      case TokenNewLine | TokenComment =>
        this
      case w: TokenWhitespace =>
        this
        
      case _ =>
        new ConsumeError(new Exception("bad"))
    }
    def result = Failure(???)
    
    // from ContainsElements

    override def addElement(element: TefElement) = new ConsumeElementContent(name, value, attributes, elements :+ element, container)
    
    // from ContainsAttributes
    
    override def addAttribute(attribute: TefAttribute) = new ConsumeElementContent(name, value, attributes :+ attribute, elements, container)
  }
  
  private class ConsumeAttributeName(container: ContainsAttributes) extends ConsumeToken {
    def consume(token: Token) = token match {
      case TokenName(name) =>
        new ConsumeAttributeValue(name, container)
      case w: TokenWhitespace =>
        this
      case _ =>
        new ConsumeError(new Exception("bad"))
    }
    def result = Failure(???)
  }
  
  private class ConsumeAttributeValue(name: String, container: ContainsAttributes) extends ConsumeToken {
    def consume(token: Token) = token match {
      case TokenValue(value) =>
        new ConsumeAttributeAfterValue(name, Some(value), container)
      case TokenNewLine =>
        new ConsumeAttributeAfterValue(name, None, container)
      case w: TokenWhitespace =>
        this
      case _ =>
        new ConsumeError(new Exception("bad"))
    }
    def result = Failure(???)
  }
  
  private class ConsumeAttributeAfterValue(name: String, value: Option[String], container: ContainsAttributes) extends ConsumeToken {
    def consume(token: Token) = token match {
      case TokenAddValue =>
        new ConsumeAttributeAddValue(name, value, container)
      case TokenElementDecl | TokenAttribute | TokenElementEnd =>
        container.addAttribute(new TefAttribute(name, value)).consume(token)
      case TokenNewLine | TokenComment =>
        this
      case w: TokenWhitespace =>
        this
      case _ =>
        new ConsumeError(new Exception("bad"))
    }
    def result = Failure(???)
  }
  
  private class ConsumeAttributeAddValue(name: String, value: Option[String], container: ContainsAttributes) extends ConsumeToken {
    def consume(token: Token) = token match {
      case TokenValue(addValue) =>
        new ConsumeAttributeAfterValue(name, extendValue(value, addValue), container)
      case TokenNewLine =>
        new ConsumeAttributeAfterValue(name, extendValue(value), container)
      case _ =>
        new ConsumeError(new Exception("bad"))
    }
    def result = Failure(???)  
  }
  
  private sealed trait Token {
    def content: String
  }

  private abstract class CharToken(override val content: String) extends Token

  private case class TokenName(override val content: String) extends Token
  private case class TokenValue(override val content: String) extends Token
  private case object TokenElementDecl extends CharToken("+")
  private case object TokenElementEnd extends CharToken("-")
  private case object TokenAddValue extends CharToken("=")
  private case object TokenComment extends CharToken("#")
  private case object TokenAttribute extends CharToken(".")
  private case class TokenWhitespace(override val content: String) extends Token
  private case object TokenNewLine extends CharToken("\n")
}
