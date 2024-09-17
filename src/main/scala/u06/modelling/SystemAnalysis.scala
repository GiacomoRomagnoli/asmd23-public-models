package u06.modelling

import u06.modelling.SystemAnalysis.paths
import u07.utils.MSet

import scala.annotation.tailrec

// Basical analysis helpers
object SystemAnalysis:

  type Path[S] = List[S]

  extension [S](system: System[S])

    def normalForm(s: S): Boolean = system.next(s).isEmpty

    def complete(p: Path[S]): Boolean = normalForm(p.last)

    def paths(s: S, depth: Int): Seq[Path[S]] = depth match
      case 0 => LazyList()
      case 1 => LazyList(List(s))
      case _ =>
        for
          path <- paths(s, depth - 1)
          next <- system.next(path.last) match
            case s if s.isEmpty => Set(path.last)
            case s => s
        yield path :+ next

    // complete paths with length '<= depth' (could be optimised)
    def completePathsUpToDepth(s: S, depth:Int): Seq[Path[S]] =
      (1 to depth).to(LazyList) flatMap (paths(s, _)) filter (complete(_))

    /**
     * Used to make analysis of a system without generating all possible paths
     * @param s start state
     * @param depth max length for paths 
     * @return a map representing every possible transition available in paths with max length depth
     */
    def syntheticPaths(s: S, depth: Int): Map[S, Set[S]] = 

      @tailrec
      def loop(states: Path[S], depth: Int, acc: Map[S, Set[S]]): Map[S, Set[S]] = depth match
        case 0 => acc
        case _ =>
          val kv = for 
            s <- states
            entry <- system.next(s) match
              case _ if acc.contains(s) => List() 
              case n => List((s, n))
          yield entry
          val ns = for 
            entry <- kv
            s <- entry._2
          yield s
          loop(ns, depth - 1, acc ++ kv)
      
      loop(List(s), depth, Map.empty)

    def always(prop: S => Boolean)(using s: S, depth: Int): Boolean =
      system.syntheticPaths(s, depth).keys.forall(prop)