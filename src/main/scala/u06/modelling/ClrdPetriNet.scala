package scala.u06.modelling
import u06.modelling.PetriNet
import u06.modelling.System
import u06.utils.MSet

object ClrdPetriNet:
  case class Trn[P, C](cond: MSet[P], eff: MSet[(P, C)] => MSet[(P, C)], inh: MSet[P])
  type PetriNet[P, C] = Set[Trn[P, C]]

  extension [P, C](self: MSet[(P, C)])
    private def color(cond: MSet[P]) =
      val groupByPlaces = self.asList.groupBy((p, _) => p)
      for
        (pc, nc) <- cond.asMap
        (ps, tokens) <- groupByPlaces
        if pc == ps && nc <= tokens.size
        c <- tokens.combinations(nc)
      yield MSet.ofList(c)

  extension [P, C](pn: PetriNet[P, C])
    def toSystem: System[MSet[(P, C)]] = mc =>
      val m = MSet.ofList(mc.asList.map((p, _) => p))
      for
        trn <- pn
        if m `disjoined` trn.inh
        cond <- mc.color(trn.cond)
        out <- mc `extract` cond
      yield out `union` trn.eff(cond)

  extension [P, C](cond: MSet[P])
    def ~~>> (eff: MSet[(P, C)]): Trn[P, C] =  Trn(cond, _ => eff, MSet())
    def ~~> (eff: MSet[(P, C)] => MSet[(P, C)]): Trn[P, C] = Trn(cond, eff, MSet())
    def ~~> (eff: MSet[P]): Trn[P, C] = if cond.size != eff.size then
      throw IllegalArgumentException()
    else
      Trn(
        cond,
        condition => MSet.ofList((for i <- 0 to condition.size yield (eff.asList(i), condition.asList(i)._2)).toList),
        MSet()
      )

  extension [P, C](trn: Trn[P, C])
    def ^^^ (z: MSet[P]): Trn[P, C] = trn.copy(inh = z)