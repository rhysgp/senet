package com.rhyssoft.senet

import scala.scalajs.js.JSApp
import org.scalajs.dom
import dom.document

object TutorialApp extends JSApp {
  def main(): Unit = {
    println("Hello world!")
    appendPara(document.body, "Hello World!")
  }

  def appendPara(targetNode: dom.Node, text: String): Unit = {
    val paraNode = document.createElement("p")
    val textNode = document.createTextNode(text)
    paraNode.appendChild(textNode)
    targetNode.appendChild(paraNode)
  }


}
