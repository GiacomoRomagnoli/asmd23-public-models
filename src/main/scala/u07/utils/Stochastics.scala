package u07.utils

import u07.modelling.CTMC
import u07.modelling.CTMCSimulation.newSimulationTrace
import scala.util.Random

object Stochastics:

  given Random = new Random()

  // (p1,a1),...,(pn,an) --> (p1,a1),(p1+p2,a2),..,(p1+..+pn,an)
  def cumulative[A](l: List[(Double, A)]): List[(Double, A)] =
    l.tail.scanLeft(l.head):
      case ((r, _), (r2, a2)) => (r + r2, a2)

  // (p1,a1),...,(pn,an) --> ai, selected randomly and fairly
  def draw[A](cumulativeList: List[(Double,A)])(using rnd: Random): A =
    val rndVal = rnd.nextDouble() * cumulativeList.last._1
    cumulativeList.collectFirst{ case (r, a) if r >= rndVal => a }.get

  // (p1,a1),...,(pn,an) + 100 --> {a1 -> P1%,...,an -> Pn%}
  def statistics[A](choices: Set[(Double,A)], size: Int)
                   (using rnd: Random): Map[A,Int] =
    (1 to size).map(i => draw(cumulative(choices.toList)))
                .groupBy(identity).view.mapValues(_.size).toMap

  def meanTime[A](from: A, until: A => Boolean, in: CTMC[A], runs: Int)(using rnd: Random): Double =
    val times = for
      _ <- 1 to runs
    yield in.newSimulationTrace(from, rnd).dropWhile(e => !until(e.state)).head.time
    times.sum / times.size

  def amountOfTime[A](from: A, of: A => Boolean, until: A => Boolean, in: CTMC[A], runs: Int)(using rnd: Random): Double =
    val simulations = for
      span <- for
        _ <- 1 to runs
      yield in.newSimulationTrace(from, rnd).span(e => !until(e.state))
    yield span._1 :+ span._2.head
    val times = for
      simulation <- simulations
    yield simulation.foldLeft((0.0, simulation.head))(
      (acc, e) => acc match
        case (amount, last) if of(last.state) => (amount + e.time - last.time, e)
        case (amount, _) => (amount, e)
    )._1
    (times.sum / simulations.map(_.last.time).sum) * 100