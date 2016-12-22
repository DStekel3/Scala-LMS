object Automata extends IO {
  val under = "Automata"
  
  def run() = {
    val p = new AutomataDriver with NFAtoDFA {
      def snippet(x: Rep[Unit]) = {
        def findAAB(): NIO = {
          guard(C('A')) {
            guard(C('A')) {
              guard(C('B'), true) {
                stop()
          }}} ++
          guard(W) { findAAB() } // in parallel ...
        }

        // NFA to DFA conversion via staging explained below.
        convertNFAtoDFA((findAAB(), false))
      }
    }

    // Some tests.
    assert(p.matches("AAB") == true)
    assert(p.matches("AAC") == false)
    assert(p.matches("AACAAB") == true)
    assert(p.matches("AACAABAAC") == true)
    println("Generating code snippet in /out/"+under+".actual.scala")
    exec("", p.code)
  }
}

trait NFAOps extends scala.lms.util.ClosureCompare {
  type NIO = List[NTrans] // state: many possible transitions
  def guard(cond: CharSet, found: => Boolean = false)(e: => NIO): NIO = {
    List(NTrans(cond, () => found, () => e))
  }
  def stop(): NIO = Nil
  def trans(c: CharSet)(s: () => NIO): NIO = List(NTrans(c, () => false, s))
  def guards(conds: List[CharSet], found: Boolean = false)(e: => NIO): NIO = {
    conds.flatMap(guard(_, found)(e))
  }

  case class NTrans(c: CharSet, e: () => Boolean, s: () => NIO) extends Ordered[NTrans] {
    override def compare(o: NTrans) = {
      val i = this.c.compare(o.c)
      if (i != 0) i else {
        val i2 = this.e().compare(o.e())
        if (i2 != 0) i2 else {
          val tf = canonicalize(this.s())
          val of = canonicalize(o.s())
          if (tf == of) 0 else tf.compare(of)
	      }
      }
    }
  }

  sealed abstract class CharSet extends Ordered[CharSet] {
    override def compare(o: CharSet) = (this,o) match {
      case (W,W) => 0
      case (W,_) => 1
      case (_,W) => -1
      case (C(c1),C(c2)) => c1.compare(c2)
    }
  }

  case class C(c: Char) extends CharSet
  case object W extends CharSet
}

case class Automaton[@specialized(Char) I, @specialized(Boolean) O](
  out: O, next: I => Automaton[I,O])

trait DFAOps extends Dsl {
  implicit def dfaTyp: Typ[DfaState]
  type DfaState = Automaton[Char,Boolean]
  type DIO = Rep[DfaState]
  def dfa_trans(f: Rep[Char] => DIO): DIO = dfa_trans(false)(f)
  def dfa_trans(e: Boolean)(f: Rep[Char] => DIO): DIO
}

trait NFAtoDFA extends NFAOps with DFAOps {
  def convertNFAtoDFA(in: (NIO, Boolean)): DIO = {
    def iterate(flag: Boolean, state: NIO): DIO = {
      dfa_trans(flag){ c: Rep[Char] => exploreNFA(canonicalize(state), c) { iterate }
    }}
    iterate(in._2, in._1)
  }

  def canonicalize(state: NIO): NIO = {
    if (state.isEmpty) state else {
      val state_sorted = state.sorted
      state_sorted.head :: (for ((s,sn) <- (state_sorted zip state_sorted.tail)
        if s.compare(sn) != 0) yield sn)
    }
  }

  def exploreNFA[A:Typ](xs: NIO, cin: Rep[Char])(
    k: (Boolean, NIO) => Rep[A]): Rep[A] = xs match {
    case Nil => k(false, Nil)
    case NTrans(W, e, s)::rest =>
      val (xs1, xs2) = xs.partition(_.c != W)
      exploreNFA(xs1,cin)((flag,acc) =>
        k(flag || xs2.exists(_.e()), acc ++ xs2.flatMap(_.s())))
    case NTrans(cset, e, s)::rest =>
      if (cset contains cin) {
        val xs1 = for (
          NTrans(rcset, re, rs) <- rest;
          kcset <- rcset knowing cset
        ) yield NTrans(kcset,re,rs)
        exploreNFA(xs1,cin)((flag,acc) => k(flag || e(), acc ++ s()))
      } else {
        val xs1 = for (
          NTrans(rcset, re, rs) <- rest;
          kcset <- rcset knowing_not cset
        ) yield NTrans(kcset,re,rs)
        exploreNFA(xs1, cin)(k)
      }
  }

  def infix_contains(s: CharSet, c: Rep[Char]): Rep[Boolean] = s match {
    case C(c1) => c == c1
    case W => unit(true)
  }
  def infix_knowing(s1: CharSet, s2: CharSet): Option[CharSet] = (s1,s2) match {
    case (W,_) => Some(W)
    case (C(c1),C(c2)) if c1 == c2 => Some(W)
    case _ => None
  }
  def infix_knowing_not(s1: CharSet, s2: CharSet): Option[CharSet] = (s1,s2) match {
    case (C(c1), C(c2)) if c1 == c2 => None
    case _ => Some(s1)
  }
}

trait DFAOpsExp extends DslExp with DFAOps {
  implicit def dfaTyp: Typ[DfaState] = manifestTyp
  case class DFAState(e: Boolean, f: Rep[Char => DfaState]) extends Def[DfaState]
  def dfa_trans(e: Boolean)(f: Rep[Char] => DIO): DIO = DFAState(e, doLambda(f))
}

trait ScalaGenDFAOps extends DslGen {
  val IR: DFAOpsExp
  import IR._
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case dfa@DFAState(b,f) =>
      emitValDef(sym, "Automaton(" + quote(b) + ", " + quote(f) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}

abstract class AutomataDriver extends DslDriver[Unit,Automaton[Char,Boolean]] with DFAOpsExp { q =>
  override val codegen = new ScalaGenDFAOps {
    val IR: q.type = q
  }
  def matches(s: String): Boolean = {
    var a: Automaton[Char,Boolean] = f(())
    var i: Int = 0
    while (!a.out && i < s.length) {
      a = a.next(s(i))
      i += 1
    }
    a.out
  }
}