package pouringexample

//import streams.GameDef.Move

/**
  * Created by vitali on 2016-07-12.
  */
class Pouring (capacity: Vector[Int]){

  // States
  type State = Vector[Int]

  val initialState = capacity map (x => 0) //takes Int and returns 0

  // Moves
  trait Move {
    def change( state: State): State
  }
  case class Empty(glass: Int) extends  Move {
    def change( state: State): State = state updated (glass, 0)
  }

  case class Fill(glass: Int) extends Move {
    def change( state: State): State = state updated (glass, capacity(glass))
  }

  case class Pour(from: Int, to: Int) extends Move {
    def change( state: State): State = {
      val amount = state(from) min (capacity(to) - state (to))
      state updated (from, state(from) - amount) updated (to, state(to) + amount)
    }
  }

  val glasses = 0 until capacity.length

  // All moves
  val moves =
    (for (g <- glasses) yield Empty(g)) ++
    (for (g <- glasses) yield Fill(g)) ++
    (for (from <- glasses; to <- glasses if from != to) yield Pour(from, to))

}
