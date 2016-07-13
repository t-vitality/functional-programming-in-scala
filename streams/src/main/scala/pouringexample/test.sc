
//package pouringexample

//object test {
//  val problem = new Pouring( Vector(4, 7) )
//
//  problem.moves
//}

class Pouring (capacity: Vector[Int]){

  // States
  type State = Vector[Int]

  val initialState = capacity map (x => 0) //takes Int and returns 0

  // Moves
  trait Move
  case class Empty(glass: Int) extends  Move
  case class Fill(glass: Int) extends Move
  case class Pour(from: Int, to: Int) extends Move

  val glasses = 0 until capacity.length

  // All moves
  val moves =
    (for (g <- glasses) yield Empty(g)) ++
      (for (g <- glasses) yield Fill(g)) ++
      (for (from <- glasses; to <- glasses if from != to) yield Pour(from, to))

}

val problem = new Pouring(Vector(4, 7, 9))

problem.moves