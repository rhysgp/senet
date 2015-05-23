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

  "Making a simple move" should {
    "produce a board with only that piece moved" in {
      val initialBoard = Rules.initialBoard(NullMove, Board(Nil)).get
      val boardAfterMoveOpt = Rules.play(PieceMove(10, 5), initialBoard)
      boardAfterMoveOpt must beSome[Board]
      boardAfterMoveOpt.get.pieces must be_==(Seq(Cone(1), Reel(2), Cone(3), Reel(4), Cone(5), Reel(6), Cone(7), Reel(8), Cone(9), Reel(15)))
    }
  }

  "Moving a piece to a space occupied by an opponent" should {
    "lead to dancing" in {
      val board = Board(Seq(Cone(1), Reel(2), Cone(3), Reel(4), Cone(5), Reel(6), Cone(7), Reel(8), Cone(9), Reel(12)))
      val boardAfterMoveOpt = Rules.play(PieceMove(9, 3), board)
      boardAfterMoveOpt must beSome[Board]
      boardAfterMoveOpt.get.pieces must be_==(Seq(Cone(1), Reel(2), Cone(3), Reel(4), Cone(5), Reel(6), Cone(7), Reel(8), Reel(9), Cone(12)))
    }
  }

  "Moving a piece to a space occupied by a piece of the same type" should {
    "be an illegal move" in  {
      val board = Board(Seq(Cone(1), Reel(2), Cone(3), Reel(4), Cone(5), Reel(6), Cone(7), Reel(8), Cone(9), Reel(12)))
      val boardAfterMoveOpt = Rules.play(PieceMove(8, 4), board)
      boardAfterMoveOpt must beNone
    }
  }

}
