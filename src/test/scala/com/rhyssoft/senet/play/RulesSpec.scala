package com.rhyssoft.senet.play

import com.rhyssoft.senet.state.{Reel, Cone, Board}
import org.specs2.mutable._

class RulesSpec extends Specification {

  "Initialising a board" should {
    "produce a board with alternating piece types on the first row, starting with cone" in {

      val boardOpt = Rules.initialBoard(NullMove, Board(Nil))

      boardOpt.isDefined must beTrue
      boardOpt.get.pieces must_== Seq(Cone(1), Reel(2), Cone(3), Reel(4), Cone(5), Reel(6), Cone(7), Reel(8), Cone(9), Reel(10))
    }
  }


}
