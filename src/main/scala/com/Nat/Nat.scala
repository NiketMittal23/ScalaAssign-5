package com.Nat

trait Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat
  def +(that: Nat): Nat
  def -(that: Nat): Nat
  def toInteger: Int
}

object Zero extends Nat {
  def isZero: Boolean = true
  def predecessor: Nat = throw new Exception("negative number")
  def successor: Nat = new Succ(Zero)
  def +(that: Nat): Nat = that
  def -(that: Nat): Nat = if (that.isZero) Zero else throw new Exception("negative number")
  def toInteger(): Int = 0
}

class Succ(x: Nat) extends Nat {
  def isZero: Boolean = false
  def predecessor: Nat = x
  def successor: Nat = new Succ(this)
  def +(that: Nat): Nat = x + that.successor
  def -(that: Nat): Nat = if (that.isZero) this else x - that.predecessor
  def toInteger: Int = x.toInteger + 1
}

trait Sign {
  def isPositive: Boolean
  def negate: Sign
}

object Positive extends Sign {
  def isPositive: Boolean = true
  def negate: Sign = Negative
}

object Negative extends Sign {
  def isPositive: Boolean = false
  def negate: Sign = Positive
}

case class Integer(value: Nat, sign: Sign) extends Nat with Sign {

  def isZero: Boolean = value.isZero

  def predecessor: Nat =
    if (isZero) Integer(value.successor, Negative)
    else if (sign.isPositive) Integer(value.predecessor, sign)
    else  Integer(value.successor, Negative)

  def successor: Nat =
    if (isZero) Integer(value.successor, Positive)
    else if (sign.isPositive)  Integer(value.successor, sign)
    else  Integer(value.predecessor, Negative)

  def +(that: Nat): Nat =
    if (isZero) that
    else if (sign.isPositive) this.predecessor + that.successor
    else this.successor + that.predecessor

  def -(that: Nat): Nat =
    if (that.isZero) this
    else that match {
      case Integer(v, s) => this + Integer(v, s.negate)
    }

  def isPositive: Boolean = sign.isPositive

  def negate: Integer = Integer(value, sign.negate)

  def toInteger: Int =  value.toInteger

  override def toString = {
    if (sign.isPositive) "+" + value.toInteger
    else "-" + value.toInteger
  }
}
object NatTests extends App {
  val no0 = Zero
  val no1 = new Succ(Zero)
  val no2 = new Succ(no1)
  val nat = Integer(no2, Negative)
  println(nat.predecessor)
  println(nat.successor)
  println(nat.isZero)
}