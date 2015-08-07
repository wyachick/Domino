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

  def toPair: (Int, Int) = (min, max)
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

  implicit val dominoOrdering = new Ordering[Domino] {
    override def compare(x: Domino, y: Domino): Int = (x.min * 10 + x.max).compareTo(y.min * 10 + y.max)
  }

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

  private def isFullSet(row: Seq[Domino]): Boolean = {
    def go(acc: Set[Int], curRow: Seq[Domino]): Boolean = {
      if (curRow.isEmpty) true
      else {
        val filterInclude = curRow.filter(d => acc.contains(d.toPair._1) || acc.contains(d.toPair._2))
        val filterNotInclude = curRow.filter(d => !(acc.contains(d.toPair._1) && acc.contains(d.toPair._2)))
        if (filterInclude.isEmpty) false
        else
          go(filterInclude.foldLeft(acc)((b, a) =>
            b + a.toPair._1 + a.toPair._2
          ), filterNotInclude)
      }
    }

    go(Set(row.head.toPair._1, row.head.toPair._2), row.tail)

  }

  def mayBeLine(row: Seq[Domino]): Boolean = {
    if (!isFullSet(row))
      false
    else {
      val (doubles, pairs, character) = helper(row)
      val characterSum = character.values.view.map(_ % 2).sum
      characterSum == 0 || characterSum == 2
    }
  }

  def mayBeRing(row: Seq[Domino]): Boolean = {
    if (!isFullSet(row))
      false
    else {
      val (doubles, pairs, character) = helper(row)
      val characterSum = character.values.view.map(_ % 2).sum
      characterSum == 0
    }
  }


  /* def mayBeUseAll(row: Seq[Domino]): Boolean = {
     if (!isFullSet(row))
       false
     else {
       val (doubles, pairs, character) = helper(row)

       val isError = character.exists(pair =>
         (pair._2 > 2) && (pair._2 % 2 == 1)
       )

       val characterNew = for ((k, v) <- character) yield {
         if (doubles.exists(_.contain(k))) {
           (k, if (v <= 4) 4 - v else (v - 4) % 2)
         } else
           (k, v % 2)
       }
       val characterSum = characterNew.values.sum
       characterSum % 2 == 0 && characterSum <= 2 + 2 * doubles.size
     }
   }*/

  def mayBeUseAll(row: Seq[Domino]): Boolean = {

    if (!isFullSet(row)) false
    else {

      val sortedDomino = mutable.TreeSet(row: _*)

      def go(tail: Set[Domino], ends: Map[Int, Int]): List[Domino] = {

        var result = List.empty[Domino]

        for (domino <- tail.filter(x => ends(x.min) != 0 || ends(x.max) != 0)) {
          val filteredTail = tail.filter(_ != domino)
          if (domino.isDouble) {
            if (ends(domino.max) != 0) {
              result = tryImprove(domino, filteredTail, ends, 0, 2, result)
              if (result.size == tail.size) return result
            }
          } else {
            if (ends(domino.min) != 0) {
              result = tryImprove(domino, filteredTail, ends, -1, 1, result)
              if (result.size == tail.size) return result
            }
            if (ends(domino.max) != 0) {
              result = tryImprove(domino, filteredTail, ends, 1, -1, result)
              if (result.size == tail.size) return result
            }
          }
        }
        result
      }

      def tryImprove(current: Domino, last: Set[Domino], v: Map[Int, Int], first: Int, second: Int, result: List[Domino]) = {
        val r = current :: go(last, v.+(current.min -> (v(current.min) + first), current.max -> (v(current.max) + second)))
        if (r.size > result.size) r else result
      }
      val emptyEnds = (0 to 6).foldLeft(Map.empty[Int, Int])((m, el) => m.+(el -> 0))
      val (num_1, num_2) = sortedDomino.head.toPair
      row.size == (sortedDomino.head :: go(sortedDomino.tail.toSet, if (sortedDomino.head.isDouble) emptyEnds.+(num_1 -> 4) else emptyEnds.+(num_1 -> 1, num_2 -> 1))).size
    }
  }


  def generator(n: Int): mutable.TreeSet[Domino] = {
    var set: mutable.TreeSet[Domino] = mutable.TreeSet.empty[Domino]
    while (set.size != n) {
      val l = Random.nextInt(7)
      val r = Random.nextInt(7)
      set += Domino(l, r)
    }
    set
  }


}
