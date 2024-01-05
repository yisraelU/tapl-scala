package chapter3

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



enum Term:
  case TmTrue extends Term
  case TmFalse extends Term
  case  TmIf(t1:Term, t2:Term, t3:Term) extends Term 
  case TmZero extends Term
  case TmSucc(term:Term) extends Term
  case TmPred(term:Term) extends Term
  case TmIsZero(term: Term) extends Term
