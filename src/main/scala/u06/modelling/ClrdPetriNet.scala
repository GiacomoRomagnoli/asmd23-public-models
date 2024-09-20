package scala.u06.modelling
import u06.modelling.PetriNet
import u06.modelling.System
import u06.utils.MSet

object ClrdPetriNet:

  export PetriNet.{Trn, Marking, PetriNet}
  type Token[P, C] = (P, C)

  extension [P, C](self: MSet[Token[P, C]])
    private def color(cond: MSet[P]) =
      val groupByPlaces = self.asList.groupBy((p, _) => p)
      for
        (pc, nc) <- cond.asMap
        (ps, tokens) <- groupByPlaces
        if pc == ps && nc <= tokens.size
        c <- tokens.combinations(nc)
      yield MSet.ofList(c)

  extension [P, C](pn: PetriNet[P])
    def toSystem: System[Marking[Token[P, C]]] = mc =>
      val m = MSet.ofList(mc.asList.map((p, _) => p))
      val transitions = for
        trn <- pn
        if m disjoined trn.inh
        cond <- mc.color(trn.cond)
        // gli effetti non si colorano allo stesso modo riguardaci!
        eff <- cond.color(trn.eff)
        out <- mc extract cond
      yield out union eff
      Set()
