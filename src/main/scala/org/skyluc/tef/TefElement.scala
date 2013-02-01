package org.skyluc.tef

class TefElement(val name: String, val value: Option[String], val attributes: List[TefAttribute], val subs: List[TefElement]) {

  override def toString = "Element[%s, %s, %s, %s]".format(name, value.getOrElse(""), attributes.mkString(", "), subs.mkString(", "))
  
}

class TefDocument(val elements: List[TefElement]) {
  
  override def toString = "Document: %s".format(elements.mkString(", "))
  
}