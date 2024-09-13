package scala.u06.examples

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.shouldBe
import u06.utils.MSet

import scala.u06.examples.PNReadersAndWriters.{*, given}

class PNReadersAndWritersProp extends AnyFunSuite:

  test("In path long at most 100 states mutual exclusion never fails"):
    pnRW.always(!_.matches(MSet(p7, p7))) shouldBe true
    pnRW.always(!_.matches(MSet(p6, p7))) shouldBe true

