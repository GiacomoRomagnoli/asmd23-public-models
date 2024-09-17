package scala.u06.examples

export u06.modelling.PetriNet
import u06.utils.MSet

import scala.u06.examples.PNReadersAndWriters.{depth, initialMark}

object PNReadersAndWriters:

  enum Place:
    case p1, p2, p3, p4, p5, p6, p7

  export Place.*
  export u06.modelling.SystemAnalysis.*
  export u06.modelling.PetriNet.*

  // Readers and writers
  def pnRW = PetriNet[Place](
    MSet(p1) ~~> MSet(p2),
    MSet(p2) ~~> MSet(p3),
    MSet(p2) ~~> MSet(p4),
    MSet(p3, p5) ~~> MSet(p5, p6),
    MSet(p4, p5) ~~> MSet(p7) ^^^ MSet(p6),
    MSet(p6) ~~> MSet(p1),
    MSet(p7) ~~> MSet(p5, p1),
  ).toSystem

  // Eventually readers and writers
  def pnERW = PetriNet[Place](
    MSet(p1) ~~> MSet(p2) ^^^ MSet(p3),
    MSet(p2) ~~> MSet(p3),
    MSet(p2) ~~> MSet(p4),
    MSet(p3, p5) ~~> MSet(p5, p6),
    MSet(p4, p5) ~~> MSet(p7) ^^^ MSet(p6),
    MSet(p6) ~~> MSet(p1),
    MSet(p7) ~~> MSet(p5, p1),
  ).toSystem

  given initialMark: MSet[Place] = MSet(p1, p1, p5)
  given depth: Int = 15

@main def mainPNReadersAndWriters =
  import PNReadersAndWriters.*
  println(pnRW.syntheticPaths(initialMark, 100).toList.mkString("\n"))