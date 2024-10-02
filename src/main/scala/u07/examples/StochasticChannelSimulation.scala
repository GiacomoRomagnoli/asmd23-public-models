package u07.examples

import u07.utils.Time

import java.util.Random
import u07.examples.StochasticChannel.*
import u07.utils.Stochastics.{amountOfTime, meanTime, given}

@main def mainStochasticChannelSimulation =
  Time.timed:
    println:
      stocChannel.newSimulationTrace(IDLE, new Random)
        .take(10)
        .toList
        .mkString("\n")

@main def mainStochasticChannelStatistics(): Unit =
  println:
    "mean time until DONE = " + meanTime(IDLE, DONE, stocChannel, 10000) + "\n" +
    "FAIL time until DONE = " + amountOfTime(FAIL, IDLE, DONE, stocChannel, 10000) + " %"