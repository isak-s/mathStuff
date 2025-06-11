package math

// numerator = num
// denominator = den
private class Quotient(val num: Int, val den: Int) extends Cloneable {

    infix def *(o: Int) = Quotient(num*o, den)
    infix def /(o: Int) = Quotient(num, den*o)

    infix def *(o: Quotient) = Quotient(num*o.num, den*o.den)
    infix def /(o: Quotient) = Quotient(num*o.den, den*o.num)

    infix def +(o: Int) = Quotient(num+o*den, den)
    infix def -(o: Int) = Quotient(num-o*den, den)

    infix def +(o: Quotient) = Quotient(num*o.den + o.num*den, den*o.den)
    infix def -(o: Quotient) = Quotient(num*o.den - o.num*den, den*o.den)

    def sqrt: Double = {
        // val approx =
        Math.sqrt(this.toDouble)
        // val denom = 10000 // or higher for better precision
        // val num = (approx * denom).round.toInt
        // Quotient(num, denom)
    }

    def toDouble: Double = {
        num / den.toFloat
    }

    override def toString(): String = {
        if (den == 1) {s"${num}"}
        else {s"${num}/${den}"}
    }

    override def clone: Quotient = {
        new Quotient(num, den)
    }

    override def equals(other: Any): Boolean = other match {
        case that: Quotient => this.num * that.den == that.num * this.den
        case that: Int => this.num == that * this.den
        case _ => false
    }
}

object Quotient {

    def apply(num: Int, den: Int) = {
        if (num == 0) {
            new Quotient(0, 1)
        }

        else {
            val d = gcd(den, num)
            if (den < 0) {
                // If the denominator is negative, Move the negative to the numerator,
                // If both happen to be negative, this turns it positive
                new Quotient(-num/d, -den/d)
            }
            else {
                new Quotient(num/d, den/d)
                }
        }
    }

    def fromDouble(d: Double): Quotient = {
        val scale = 1000000
        Quotient((d * scale).toInt, scale)
    }

    def gcd(a: Int, b: Int): Int = {
        if (b == 0) { a.abs }
        else { gcd(b, a % b) }
    }

    def lcm(a: Int, b: Int) = {

    }

        // Add conversion from Int or double to quotient
    given QuotientConversion: Conversion[Int, Quotient] with {
        def apply(n: Int): Quotient = new Quotient(n, 1)
    }

    extension (inlineContext: StringContext)
        def q(args: Any*): Quotient = {
            val raw = inlineContext.s(args*)
            val operands = raw.split("/").map(_.trim).map(_.toInt)
            operands.length match {
            case 2 =>
                Quotient(operands(0), operands(1))
            case 3 =>
                Quotient(operands(0) * operands(1), operands(2))
            case 4 =>
                Quotient(operands(0) * operands(3), operands(2) * operands(1))
            case x if x > 4 =>
                throw new IllegalArgumentException(s"Too many operands in '$raw'. Use at most 4 parts.")
            case _ =>
                throw new IllegalArgumentException(s"Invalid quotient format: '$raw'")
            }
        }
}