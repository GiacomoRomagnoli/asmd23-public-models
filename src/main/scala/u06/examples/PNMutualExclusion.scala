package u06.examples

export u06.modelling.PetriNet
import u06.utils.MSet

object PNMutualExclusion:

  enum Place:
    case N, T, C
    
  export Place.*
  export u06.modelling.PetriNet.*
  export u06.modelling.SystemAnalysis.*
  export u06.utils.MSet

  // DSL-like specification of a Petri Net
  def pnME = PetriNet[Place](
    MSet(N) ~~> MSet(T),
    MSet(T) ~~> MSet(C) ^^^ MSet(C),
    MSet(C) ~~> MSet()
  ).toSystem

  given initialMarking: MSet[Place] = MSet(N,N)
  given depth: Int = 100

@main def mainPNMutualExclusion =
  import PNMutualExclusion.*
  import PNMutualExclusion.given
  // example usage
  println(pnME.paths(MSet(N,N),7).toList.mkString("\n"))
  println(pnME.always(!_.matches(MSet(C,C))))
