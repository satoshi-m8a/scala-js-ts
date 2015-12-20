package scala.js.ts.ast

import java.io.File

import org.scalatest.{Matchers, FlatSpec}

import scala.io.Source

class ParserSpec extends FlatSpec with Matchers {

  val testFiles = Seq(
    //"./DefinitelyTyped/jquery/jquery.d.ts",
    //"./DefinitelyTyped/acc-wizard/acc-wizard.d.ts",
    //"./DefinitelyTyped/adm-zip/adm-zip.d.ts",
    "./DefinitelyTyped/add2home/add2home.d.ts"
  ).map(f => new File(f))


  def printCode(script: String) = {
    for {
      (l, n) <- script.split("\n").zipWithIndex
    } yield {
      println(s"${n + 1}: $l")
    }
  }

  "parser" should "parse" in {

    testFiles.foreach {
      f =>
        val content = Source.fromFile(f).mkString
        printCode(content)
        Parser.parse(content) shouldBe a[Right[_, _]]
    }
  }
}
