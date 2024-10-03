package scala.u07.examples

import u07.modelling.{CTMC, SPN}
import u07.utils.MSet
import u07.utils.Stochastics.{*, given}

import scala.util.Random

object Brussellator extends App:
  enum Place:
    case X, Y

  export Place.*
  export u07.modelling.CTMCSimulation.*
  export u07.modelling.SPN.*

  val spn = SPN[Place](
    Trn(MSet(), _ => 2.0, MSet(X), MSet()),
    Trn(MSet(X), m => 3.0 * m(X), MSet(), MSet()),
    Trn(MSet(X, X, Y), m => m(X) * m(X) * m(Y), MSet(X, X), MSet()),
    Trn(MSet(), m => 2.0 * m(X), MSet(Y), MSet()),
  )

  // A = 2.0
  // B = 2.0
  println:
    toCTMC(spn).newSimulationTrace(MSet(), summon[Random]).take(10000).map(_.state).mkString("\n")