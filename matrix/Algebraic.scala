package math

sealed trait Algebraic {
    def evaluate: Double
}
case class RationalQ(q: Quotient) extends Algebraic {
    override def evaluate: Double = {
        q.num / q.den
    }
}

case class SqrtQ(q: Quotient) extends Algebraic {
    override def evaluate: Double = {
        2
    }
}