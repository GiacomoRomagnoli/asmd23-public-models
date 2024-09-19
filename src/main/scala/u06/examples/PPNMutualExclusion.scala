package scala.u06.examples

import u06.modelling

import scala.u06.modelling
import u06.modelling.System

object PPNMutualExclusion:

  enum Place:
    case N, T, C

  export Place.*
  import scala.u06.modelling.PrtPetriNet
  import PrtPetriNet.*
  export u06.modelling.SystemAnalysis.*
  export u06.utils.MSet

  def ppnME: System[Marking[Place]] = PrtPetriNet[Place](
    MSet(N) ~~> MSet(T) :- 1,
    (MSet(T) ~~> MSet(C) ^^^ MSet(C)) :- 0,
    MSet(C) ~~> MSet() :- 0
  ).toSystem

@main def main =
  import PPNMutualExclusion.*
  println(ppnME.paths(MSet(N, N), 7).toList.mkString("\n"))