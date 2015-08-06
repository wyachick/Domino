package com.artezio.vvishniakou.domino

import com.artezio.vvishniakou.domino.model.Domino
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class DominoSpec extends FlatSpec with ShouldMatchers {
  "A Domino seq " should
    "([0,0], [0,5], [5,6],  [6,6] )" in {
    val seq = Seq(
      Domino(0,0),
      Domino(0,5),
      Domino(5,6),
      Domino(6,6)
    )
    Domino.mayBeUseAll(seq) should be
    Domino.mayBeLine(seq) should be
    Domino.mayBeRing(seq) should be

  }
  it should "([0,0], [0,5], [5,6], [0,6], [6,6] )" in {
    val seq = Seq(
      Domino(0,0),
      Domino(0,5),
      Domino(5,6),
      Domino(0,6),
      Domino(6,6)
    )
    Domino.mayBeUseAll(seq) should be 
    Domino.mayBeLine(seq) should be 
    Domino.mayBeRing(seq) shouldNot be  
  }

  it should "([1,1],[1,0], [1,2], [1,3], [1,4], [1,6], [1,5], [0,0], [5,4], [3,4], [4,2], [5,3], [3,2], [0,4] )" in {
    val seq = Seq(
      Domino(1,0),
      Domino(1,1),
      Domino(1,2),
      Domino(1,3),
      Domino(1,4),
      Domino(1,5),
      Domino(1,6),
      Domino(0,0),
      Domino(5,4),
      Domino(3,4),
      Domino(4,2),
      Domino(5,3),
      Domino(3,2),
      Domino(0,4)
    )
    Domino.mayBeUseAll(seq) should be
    Domino.mayBeLine(seq) shouldNot be
    Domino.mayBeRing(seq) shouldNot be

  }
  it should "([1,1],[1,0], [1,2], [1,3], [1,4], [1,6], [1,5], [0,0], [5,4], [3,4], [2,2], [4,2], [5,3], [3,2], [0,4] )" in {
    val seq = Seq(
      Domino(1,0),
      Domino(1,1),
      Domino(1,2),
      Domino(1,3),
      Domino(1,4),
      Domino(1,5),
      Domino(1,6),
      Domino(0,0),
      Domino(5,4),
      Domino(3,4),
      Domino(2,2),
      Domino(4,2),
      Domino(5,3),
      Domino(3,2),
      Domino(0,4)
    )
    Domino.mayBeUseAll(seq) should be
    Domino.mayBeLine(seq) shouldNot be 
    Domino.mayBeRing(seq) shouldNot be 

  }
  it should "([1,1],[1,0], [1,2], [1,3], [1,4], [0,0], [5,4], [3,4], [2,2], [4,2], [5,3], [3,2], [0,2] )" in {
    val seq = Seq(
      Domino(1,0),
      Domino(1,1),
      Domino(1,2),
      Domino(1,3),
      Domino(1,4),
      Domino(0,0),
      Domino(5,4),
      Domino(3,4),
      Domino(2,2),
      Domino(4,2),
      Domino(5,3),
      Domino(3,2),
      Domino(0,2)
    )
    Domino.mayBeUseAll(seq) should be
    Domino.mayBeLine(seq) should be
    Domino.mayBeRing(seq) should be

  }
  it should "([1,1],[2,2] )" in {
    val seq = Seq(
      Domino(2,2),
      Domino(1,1)
    )
    Domino.mayBeUseAll(seq) shouldNot be 
    Domino.mayBeLine(seq) shouldNot be 
    Domino.mayBeRing(seq) shouldNot be 

  }
  it should "([1,1],[2,2] [2,1])" in {
    val seq = Seq(
      Domino(2,2),
      Domino(1,2),
      Domino(1,1)
    )
    Domino.mayBeUseAll(seq) should be
    Domino.mayBeLine(seq) should be
    Domino.mayBeRing(seq) shouldNot be 

  }
  it should "([1,2],[2,3] [3,3], [3,4], [4,5], [5,0], [3,6], [6,6], [6,1])" in {
    val seq = Seq(
      Domino(3,2),
      Domino(1,2),
      Domino(3,3),
      Domino(3,4),
      Domino(4,5),
      Domino(5,0),
      Domino(3,6),
      Domino(6,6),
      Domino(6,1)
    )
    Domino.mayBeUseAll(seq) should be
    Domino.mayBeLine(seq) should be
    Domino.mayBeRing(seq) shouldNot be 

  }
  it should "([6,2],[2,3] [3,3], [3,4], [0,0], [5,1], [3,6], [6,6], [6,1])" in {
    val seq = Seq(
      Domino(6,2),
      Domino(2,3),
      Domino(3,3),
      Domino(3,4),
      Domino(0,0),
      Domino(5,1),
      Domino(3,6),
      Domino(6,6),
      Domino(6,1)
    )
    Domino.mayBeUseAll(seq) shouldNot be 
    Domino.mayBeLine(seq) shouldNot be 
    Domino.mayBeRing(seq) shouldNot be 

  }
  it should "([6,2],[2,3] [3,3], [3,4], [5,1], [3,6], [6,6], [6,1])" in {
    val seq = Seq(
      Domino(6,2),
      Domino(2,3),
      Domino(3,3),
      Domino(3,4),
      Domino(5,1),
      Domino(3,6),
      Domino(6,6),
      Domino(6,1)
    )
    Domino.mayBeUseAll(seq) should be
    Domino.mayBeLine(seq) shouldNot be 
    Domino.mayBeRing(seq) shouldNot be 

  }
}