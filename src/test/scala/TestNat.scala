import com.Nat.{Succ, Zero}
import org.scalatest.funsuite.AnyFunSuite

class TestNat extends AnyFunSuite{
  val one = new Succ(Zero)
  val two = new Succ(one)


  test("Checking isZero") {
    assert(Zero.isZero)
  }

  test("one isZero is false") {
    assert(!one.isZero)
  }

  test("checking zero plus one is one") {
      assert((Zero + one) === one)
  }

  test(" checking predecessor of zero it will give exception") {
    assertThrows[Exception] {
      Zero.predecessor
    }
  }

  test("zero minus one throws an error") {
      assertThrows[Exception] {
        Zero - one
      }
  }

  test("checking predecessor of one is zero") {
      assert(one.predecessor === Zero)
  }

  test("one minus zero is one") {
      assert((one - Zero) === one)
  }

  test("two minus one is one") {
      assert((two - one) === one)
  }

}
