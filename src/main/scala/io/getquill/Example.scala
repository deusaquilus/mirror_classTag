package io.getquill

import scala.quoted._
import scala.compiletime._
import scala.deriving._
import scala.reflect._

case class Foo(value: String)
case class Bar(value: String)
case class Composite(foo: Foo, bar: Bar)

trait ProductContains[T] {
  def apply(ct: ClassTag[_]): Boolean
}

object Derives {
  given ProductContains[Composite] = ProductContains.derived
}

object ProductContains {

  inline def walkThrough[Types <: Tuple](ct: ClassTag[_]): Boolean =
    erasedValue[Types] match
      case _: (tpe *: tpes) =>
        val tpeClassTag = summonInline[ClassTag[tpe]]
        if (ct.runtimeClass.isAssignableFrom(tpeClassTag.runtimeClass))
          true
        else
          walkThrough[tpes](ct)
      case _: EmptyTuple =>
        false
      

  inline def derived[T]: ProductContains[T] = 
    summonFrom {
      case ev: Mirror.Of[T] =>
        new ProductContains[T] {
          def apply(ct: ClassTag[_]) =
            inline ev match
              case m: Mirror.ProductOf[T] => walkThrough[m.MirroredElemTypes](ct)
        }
    }
}
