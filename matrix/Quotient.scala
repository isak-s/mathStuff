package math

// numerator = num
// denominator = den
private class Quotient(val num: Int, val den: Int) extends Cloneable {

    infix def *(o: Int) = Quotient(num*o, den)
    infix def /(o: Int) = Quotient(num, den*o)

    infix def *(o: Quotient) = Quotient(num*o.num, den*o.den)
    infix def /(o: Quotient) = Quotient(num*o.den, den*o.num)

    infix def +(o: Quotient) = Quotient(num*o.den + o.num*den, den*o.den)
    infix def -(o: Quotient) = Quotient(num*o.den - o.num*den, den*o.den)

    override def toString(): String = {
        if (den == 1) {s"${num}"}
        else {s"${num}/${den}"}
    }

    override def clone: Quotient = {
        new Quotient(num, den)
    }
}

object Quotient {

    def apply(num: Int, den: Int) = {
        if (num == 0) {
            new Quotient(0, 1)
        }

        else {
            val d = gcd(den, num)
            new Quotient(num/d, den/d)
        }
    }
    def fromRational() = {
        ???
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
            val Array(num, den) = raw.split("/").map(_.trim)
            Quotient(num.toInt, den.toInt)
        }
}