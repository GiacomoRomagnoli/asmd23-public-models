package scala.u06.examples

object CPNMutualExclusion:

  enum Place:
    case N, T, C
  enum Color:
    case Red, Blue

  export Place.*
  export Color.*
  export scala.u06.modelling.ClrdPetriNet.*
  export u06.modelling.SystemAnalysis.*
  export u06.utils.MSet

  def pnME = Set[Trn[Place, Color]](
    MSet(N) ~~> MSet(T),
    MSet(T) ~~> MSet(C) ^^^ MSet(C),
    MSet(C) ~~>> MSet(),
  ).toSystem

@main def mainCPNMutualExclusion =
  import CPNMutualExclusion.*
  println(pnME.paths(MSet((N, Red),(N, Blue)),7).toList.mkString("\n"))