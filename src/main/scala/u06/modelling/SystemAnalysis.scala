package pc.modelling

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

    def neverHappen(prop: S)(using s: S, depth: Int): Boolean =
      val runs = for
        path <- system.completePathsUpToDepth(s, depth)
      yield path.contains(prop)
      runs.forall(b => !b)