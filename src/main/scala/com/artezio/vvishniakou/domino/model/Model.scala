package com.artezio.vvishniakou.domino.model

import scala.collection.mutable
import scala.util.Random


sealed trait Domino {
  def isDouble: Boolean

  def contain(number: Int): Boolean

  def getEnds: Map[Int, Int]

  override def equals(that: Any): Boolean

  override def toString: String = "[" + min + "," + max + "]"

  def min: Int

  def max: Int

  override def hashCode: Int = {
    10 * min + max
  }
}

case class Pair(left: Int, right: Int) extends Domino {
  override val isDouble: Boolean = false

  override val min = math.min(left, right)

  override val max = math.max(left, right)

  override def contain(number: Int) = left == number || right == number

  override def getEnds: Map[Int, Int] = Map.empty[Int, Int] ++ (for (i <- 0 to 6) yield if (left == i) left -> 1 else if (right == i) right -> 1 else i -> 0)

  override def equals(that: Any): Boolean = {
    if (!that.isInstanceOf[Domino]) false
    else {
      val other = that.asInstanceOf[Domino]
      other match {
        case Pair(_, _) => other.contain(left) && other.contain(right)
        case Double(_) => false
      }
    }
  }
}

case class Double(n: Int) extends Domino {
  override val isDouble: Boolean = true

  override def contain(number: Int) = number == n

  override def getEnds: Map[Int, Int] = Map.empty[Int, Int] ++ (for (i <- 0 to 6) yield if (n == i) n -> 1 else i -> 0)

  override val min = n

  override val max = n

  override def equals(that: Any): Boolean = {
    if (!that.isInstanceOf[Domino]) false
    else {
      val other = that.asInstanceOf[Domino]
      other match {
        case Pair(_, _) => false
        case Double(_) => other.contain(n)
      }
    }
  }
}

object Domino {

  def apply(left: Int, right: Int): Domino = {
    if (left == right) Double(left) else Pair(left, right)
  }


  private def characteristic(row: Seq[Domino]): Map[Int, Int] = row.foldLeft(mutable.Map(0 -> 0, 1 -> 0, 2 -> 0, 3 -> 0, 4 -> 0, 5 -> 0, 6 -> 0)) { (map, domino) =>
    domino match {
      case Pair(l, r) =>
        map(l) += 1
        map(r) += 1
      case Double(n) =>
        map(n) += 4
    }
    map
  }.toMap


  private def helper(row: Seq[Domino]): (Seq[Domino], Seq[Domino], Map[Int, Int]) = {
    val doubles = row.filter(_.isDouble)
    val pairs = row.filter(!_.isDouble)
    (doubles, pairs, characteristic(pairs))
  }

  private def isFullSet(row: Domino*): Boolean = {
    def go(acc: List[Set[Int]], curRow: Seq[Domino]): Boolean = {
      if (curRow.isEmpty)
        acc.reduce(_ & _).nonEmpty
      else
        curRow.head match {
          case Pair(l, r) =>
            if (acc.exists(_.contains(l)) || acc.exists(_.contains(r))) {
              val list = acc.foldLeft(List.empty[Set[Int]])((b, s) => if (s.contains(l) || s.contains(r))
                ((s + l) + r) :: b
              else
                s :: b)
              go(list, curRow.tail)
            }
            else
              go(Set(l, r) :: acc, curRow.tail)
          case Double(n) =>
            if (acc.exists(_.contains(n)))
              go(acc, curRow.tail)
            else
              go(Set(n) :: acc, curRow.tail)
        }
    }
    go(List.empty[Set[Int]], row)
  }


  def mayBeLine(row: Seq[Domino]): Boolean = {
    if (!isFullSet(row: _*))
      false
    else {
      val (doubles, pairs, character) = helper(row)
      val characterSum = character.values.view.map(_ % 2).sum
      characterSum == 0 || characterSum == 2
    }
  }

  def mayBeRing(row: Seq[Domino]): Boolean = {
    if (!isFullSet(row: _*))
      false
    else {
      val (doubles, pairs, character) = helper(row)
      val characterSum = character.values.view.map(_ % 2).sum
      characterSum == 0
    }
  }


  def mayBeUseAll(row: Seq[Domino]): Boolean = {
    if (!isFullSet(row: _*))
      false
    else {
      val (doubles, pairs, character) = helper(row)
      val characterNew = for ((k, v) <- character) yield {
        if (doubles.exists(_.contain(k))) {
          (k, if (v <= 4) 4 - v else (v - 4) % 2)
        } else
          (k, v % 2)
      }
      val characterSum = characterNew.values.sum
      characterSum % 2 == 0 && characterSum <= 2 + 2 * doubles.size
    }
  }

  def generator(n: Int): Seq[Domino] = {
    var set: mutable.Set[Domino] = mutable.Set.empty[Domino]
    while (set.size != n) {
      val l = Random.nextInt(7)
      val r = Random.nextInt(7)
      set += Domino(l, r)
    }
    set.toSet.toSeq
  }


}
