package scala.u07.examples

import u07.modelling.{CTMC, SPN}
import u07.utils.MSet
import u07.utils.Stochastics.{*, given}

object SPNReadersAndWriters extends App:
  enum Place:
    case p1, p2, p3, p4, p5, p6, p7

  export Place.*
  export u07.modelling.CTMCSimulation.*
  export u07.modelling.SPN.*

  val spn = SPN[Place](
    Trn(MSet(p1), _ => 1.0, MSet(p2), MSet()),
    Trn(MSet(p2), _ => 200000.0, MSet(p3), MSet()), // t2
    Trn(MSet(p2), _ => 100000.0, MSet(p4), MSet()), //t3
    Trn(MSet(p3, p5), _ => 100000.0, MSet(p5, p6), MSet()),
    Trn(MSet(p4, p5), _ => 100000.0, MSet(p7), MSet(p6)),
    Trn(MSet(p6), m => 0.1 * m(p6), MSet(p1), MSet()), // t6
    Trn(MSet(p7), _ => 0.2, MSet(p1, p5), MSet()), // t7
  )

  println:
    "Writing Time = " +
    amountOfTime(MSet(p2, p2, p5), _.matches(MSet(p7)), _.matches(MSet(p1, p1)), toCTMC(spn), 10000) + "%" +
    "\nReadingTime = " +
    amountOfTime(MSet(p2, p2, p5), _.matches(MSet(p6)), _.matches(MSet(p1, p1)), toCTMC(spn), 10000) + "%"

  /*
  *  A parità di rate sulle transizioni t6 e t7 ciò che influisce maggiormente è il rapporto fra i rate di t2 e t3;
  *  se si pareggiano anche i rate su t2 e t3 i tempi di reading e writing si avvicinano al 50-50 se non per il fatto
  *  che i lettori possono leggere insieme e quindi il rapporto è leggermente sbilanciato verso i lettori
  */