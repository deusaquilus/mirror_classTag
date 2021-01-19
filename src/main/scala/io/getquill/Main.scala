package io.getquill

import scala.reflect._

object Main {

  def deriveContains[T](ct: ClassTag[_])(using pc: ProductContains[T]) = pc.apply(ct)

  def main(args: Array[String]): Unit = {
    import Derives.{given, _}
    println( deriveContains[Composite](classTag[Foo]) )

    

  }
}