package chapter3

import scala.annotation.tailrec

/*
t ::= terms:
true constant true
false constant false
if t then t else t conditional
0 constant zero
succ t successor
pred t predecessor
iszero t zero test
 */

case class LineInfo(line: Int, column: Int)

case class NoRulesApplyException(lineInfo: LineInfo) extends Exception

enum Term:

  def fold[A](
      isTmTrue: TmTrue => A,
      isTmFalse: TmFalse => A,
      isTmIf: TmIf => A,
      isTmZero: TmZero => A,
      isTmSucc: TmSucc => A,
      isTmPred: TmPred => A,
      isTmIsZero: TmIsZero => A
  ): A = this match
    case t: TmTrue   => isTmTrue(t)
    case t: TmFalse  => isTmFalse(t)
    case t: TmIf     => isTmIf(t)
    case t: TmZero   => isTmZero(t)
    case t: TmSucc   => isTmSucc(t)
    case t: TmPred   => isTmPred(t)
    case t: TmIsZero => isTmIsZero(t)

  case TmTrue(info: LineInfo) extends Term
  case TmFalse(info: LineInfo) extends Term
  case TmIf(info: LineInfo, t1: Term, t2: Term, t3: Term) extends Term
  case TmZero(info: LineInfo) extends Term
  case TmSucc(info: LineInfo, term: Term) extends Term
  case TmPred(info: LineInfo, term: Term) extends Term
  case TmIsZero(info: LineInfo, term: Term) extends Term

object Term:

  // unsafe eval as we can be returning a boolean or a number or an unknown result
  def eval(t: Term): Any =
    t.fold(
      _ => true,
      _ => false,
      t => if (isTrue(t.t1)) eval(t.t2) else eval(t.t3),
      _ => 0,
      t =>
        if (isNumericVal(t.term)) 1 + eval(t.term).asInstanceOf[Int]
        else throw IllegalArgumentException(),
      t =>
        if (isNumericVal(t.term)) eval(t.term).asInstanceOf[Int] - 1
        else throw NoRulesApplyException(t.info),
      t =>
        if (isNumericVal(t.term)) eval(t.term) == 0
        else throw NoRulesApplyException(t.info)
    )
  def isTrue(t: Term): Boolean = t match
    case Term.TmTrue(info) => true
    case Term.TmIf(info, t1, t2, t3) =>
      if (isTrue(t1)) isTrue(t2) else isTrue(t3)
    case _ => false

  def info(t: Term): LineInfo = t match
    case Term.TmTrue(info)        => info
    case Term.TmFalse(info)       => info
    case Term.TmIf(info, _, _, _) => info
    case Term.TmZero(info)        => info
    case Term.TmSucc(info, _)     => info
    case Term.TmPred(info, _)     => info
    case Term.TmIsZero(info, _)   => info

  @tailrec
  private def isNumericVal(t: Term): Boolean = t match
    case Term.TmZero(_)     => true
    case Term.TmSucc(_, t1) => isNumericVal(t1)
    case _                  => false

  def isVal(t: Term): Boolean = t match
    case Term.TmTrue(_)       => true
    case Term.TmFalse(_)      => true
    case t if isNumericVal(t) => true
    case _                    => false
