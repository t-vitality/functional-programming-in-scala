
object primes {
  def from(n: Int): Stream[Int] = n #:: from(n+1)

  def sieve(s: Stream[Int]): Stream[Int] =
    s.head #:: sieve(s.tail filter(_ % s.head !=0))

  val nats = from(0)
  val m4s = nats map(_ * 4)

  (m4s take 1000).toList

  val primes = sieve(from(2))
  primes.take(100).toList
}

object  Test{

  val f: String => String =  {case "ping" => "pong"}

  f("ping")
  //f("abc")

  val f1: PartialFunction[String, String] = {case "ping" => "pong"}
  f1.isDefinedAt("abc")
  f1.isDefinedAt("ping")
  f1("ping")
}