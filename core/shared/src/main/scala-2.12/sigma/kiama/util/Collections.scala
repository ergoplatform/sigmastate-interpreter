/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013-2021 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package sigma.kiama
package util

object Collections {

  // Java to Scala conversions

  import scala.collection.JavaConverters._

  def javaCollectionToVector[T](s : java.util.Collection[T]) : Vector[T] =
    s.asScala.toVector

  def mapToJavaMap[T, U](v : Map[T, U]) : java.util.Map[T, U] =
    v.asJava

  def seqToJavaList[T](v : Seq[T]) : java.util.List[T] =
    v.asJava

  // Collection building

  import scala.collection.mutable.Builder

  type Factory[-B, +C] = scala.collection.generic.CanBuildFrom[_, B, C]
  type CanBuildFrom[-A, -B, +C] = scala.collection.generic.CanBuildFrom[A, B, C]

  def newBuilder[B, C](cbf : Factory[B, C]) : Builder[B, C] =
    cbf()

  def newBuilder[A, B, C](cbf : CanBuildFrom[A, B, C], from : A) : Builder[B, C] =
    cbf(from)

}
