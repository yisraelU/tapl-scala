package chapter5

sealed trait UntypedLambdaCalculus

object UntypedLambdaCalculus {
  case class Variable(a: Any) extends UntypedLambdaCalculus
  case class Abstraction(f: Any => Any) extends UntypedLambdaCalculus

  // left associative
  case class Application(
      applier: UntypedLambdaCalculus,
      appliee: UntypedLambdaCalculus
  )

}
