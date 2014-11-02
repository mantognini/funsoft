package fos.test

import org.scalatest._

class GeneralTest extends FlatSpec with Matchers {
  import fos.{ SimplyTyped, Term }
  import SimplyTyped.{ Success, Failure, Error }
  import fos.test.helpers.Helper
  import Helper.termParser

  def reduce(t: Term): Stream[Term] = {
    try {
      val u = SimplyTyped.reduce(t)
      t #:: u #:: reduce(u)
    } catch {
      case SimplyTyped.NoRuleApplies(_) => t #:: Stream.empty
    }
  }

  val input =
    """
    letrec plus: Nat -> Nat -> Nat
          = \m: Nat. \n: Nat.
            if iszero m then n else succ (plus (pred m) n)
    in
    letrec times : Nat -> Nat -> Nat
          = \m: Nat. \n: Nat.
            if iszero m then 0 else plus n (times (pred m) n)
    in
    letrec factorial : Nat -> Nat
        = \m: Nat.
          if iszero m then 1 else times m (factorial (pred m))
    in
    let or: (Bool * Bool) -> Bool
        = \xy: Bool * Bool.
          if fst xy then true else snd xy
    in
    let and: (Bool * Bool) -> Bool
        = \xy: Bool * Bool.
          if fst xy then snd xy else false
    in  
    let not: Bool -> Bool
        = \x: Bool.
          if x then false else true
    in
    let swap: (Bool * Bool) -> (Bool * Bool)
        = \xy: Bool * Bool.
          { snd xy, fst xy }
    in
    let xor: (Bool * Bool) -> Bool
        = \xy: Bool * Bool.
          or { and { fst xy, not snd xy }, and { fst swap xy, not snd swap xy } }
    in
    letrec equal: (Nat * Nat) -> Bool
        = \p: Nat * Nat.
          if and { iszero fst p, iszero snd p } then true
          else and { not (xor { iszero fst p, iszero snd p }), equal { pred fst p, pred snd p } }
    in
    letrec fib: Nat -> Nat
        = \n: Nat.
          if or { equal { 0, n }, equal { 1, n } } then 1
          else plus (fib pred n) (fib pred pred n)
    in
    let foo: (Nat -> Nat) -> (Nat -> Nat) -> Nat -> (Nat + Bool)
        = \fun: Nat -> Nat. \gun: Nat -> Nat. \n: Nat.
          if equal { fun n, 6 }
          then inl gun n as Nat + Bool
          else inr equal { 42, gun n } as Nat + Bool
    in
    case foo factorial fib 3 of
          inl x => equal { 3, x }
         |inr x => false
    """

  val answer = "true"

  behavior of "Our compiler with a little bit of everything"

  it should s"evaluate $input to $answer" in {
    val expected = Helper.parseAndCheckOrFail(answer)
    val ast = Helper.parseAndCheckOrFail(input)
    val output = reduce(ast).last
    assert(output === expected)
  }
}