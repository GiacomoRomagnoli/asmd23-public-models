package scala.u06.examples

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.shouldBe
import u06.utils.MSet

import scala.u06.examples.PNReadersAndWriters.{*, given}

class PNReadersAndWritersProp extends AnyFunSuite:

  // Task 1
  test("In path long at most 100 states mutual exclusion never fails"):
    pnRW.always(!_.matches(MSet(p7, p7))) shouldBe true
    pnRW.always(!_.matches(MSet(p6, p7))) shouldBe true

  // Task 2
  test("In pnERW a reader should eventually read"):
    pnERW.eventually(_.matches(MSet(p6)))(using MSet(p1, p1, p1, p3, p5)) shouldBe true
    pnERW.eventually(_.matches(MSet(p6)))(using MSet(p2, p2, p2, p3, p5)) shouldBe true

