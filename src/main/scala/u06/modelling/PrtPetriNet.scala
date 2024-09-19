package scala.u06.modelling

import u06.modelling.PetriNet
import u06.modelling.System

object PrtPetriNet:
  export PetriNet.{Trn, Marking, ~~>, ^^^}

  type PrtPetriNet[P] = Map[Trn[P], Int]

  def apply[P](transitions: (Trn[P], Int)*): PrtPetriNet[P] = transitions.toMap

  extension [P](ppn: PrtPetriNet[P])
    def toSystem: System[Marking[P]] = m =>
      val transitions = for
        trn <- ppn.keys
        if m `disjoined` trn.inh
        _ <- m `extract` trn.cond
      yield trn
      val maxPriority = transitions.map(ppn.get).max
      for 
        trn <- transitions.toSet
        if ppn.get(trn) == maxPriority
        out <- m `extract` trn.cond
      yield out `union` trn.eff

  extension [P](trn: Trn[P])
    def :- (prt: Int): (Trn[P], Int) = (trn, prt)