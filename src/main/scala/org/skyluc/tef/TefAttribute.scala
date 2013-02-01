package org.skyluc.tef

class TefAttribute(val name: String, val value: Option[String]) {
  
  override def toString = "Attribute(%s, %s)".format(name, value.getOrElse(""))

}