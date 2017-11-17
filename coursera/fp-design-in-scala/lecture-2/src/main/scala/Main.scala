class Main extends App {

  // States
  type State = Vector[Int]

  class Pouring(capacity: State) {

    val initalState: State = capacity.map(_ => 0)

    // Moves

    trait Move {
      def change(state: State): State
    }

    case class Empty(glass: Int) extends Move {
      override def change(state: State): State = state updated(glass, 0)
    }

    case class Fill(glass: Int) extends Move {
      override def change(state: State): State = state updated(glass, capacity(glass))
    }

    case class Pour(from: Int, to: Int) extends Move {
      override def change(state: State): State = {
        val amount = state(from) min (capacity(to) - state(to))
        state updated(from, state(from) - amount) updated(to, state(to) + amount)
      }
    }

    val glasses: Range = capacity.indices

    val moves =
      (for (g <- glasses) yield Empty(g)) ++
        (for (g <- glasses) yield Fill(g)) ++
        (for (from <- glasses; to <- glasses if from != to) yield Pour(from, to))


    class Path(history: List[Move]) {

      def endStateFold: State = (history foldRight initalState) (_ change _)

      def endState: State = trackState(history)

      def extend(move: Move) = new Path(move :: history)

      override def toString = (history.reverse mkString " ") + "  -->" + endState

      private def trackState(xs: List[Move]): State = xs match {
        case Nil => initalState
        case move :: xs1 => move change trackState(xs1)
      }
    }

    val initialPath = new Path(Nil)

    def from(paths: Set[Path], explored: Set[State]): Stream[Set[Path]] =
      if (paths.isEmpty) Stream.empty
      else {
        val more = for {
          path <- paths
          next <- moves map path.extend
          if !(explored contains next.endState)
        } yield next
        paths #:: from(more, explored ++ ( more map(_.endState)))
      }

    val pathSets = from(Set(initialPath), Set(initalState))

    def solution(target: Int): Stream[Path] =
      for {
        pathSet <- pathSets
        path <- pathSet
        if path.endState contains target
      } yield path
  }

}
