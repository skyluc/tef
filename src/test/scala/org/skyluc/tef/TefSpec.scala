package org.skyluc.tef

import org.scalatest.FunSpec
import scala.io.Source

class TefSpec extends FunSpec {

  describe("TextFormat") {
    it("should correctly parse a simple tef string") {

      val content = Tef(Source.fromString("""
    		  |+ element
    		  |    . attribute value
    		  |  +subelement subelement text
    		  |  -
    		  |-
    		  |""".stripMargin))

      val elements= content.elements
      assert(elements.size === 1)
    		  
      assert(elements(0).name === "element")
      assert(elements(0).value === None)

      val attributes = elements(0).attributes
      assert(attributes.size === 1)
      assert(attributes(0).name === "attribute")
      assert(attributes(0).value.get === "value")

      val subElements = elements(0).subs
      assert(subElements.size === 1)
      assert(subElements(0).name === "subelement")
      assert(subElements(0).value.get === "subelement text")
      assert(subElements(0).attributes === Nil)
      assert(subElements(0).subs === Nil)
    }
  }

}