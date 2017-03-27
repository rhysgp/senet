package com.rhyssoft.senet.play

import com.rhyssoft.senet.state.Board
import org.specs2.mutable._

class RulesSpec extends Specification {

  import com.rhyssoft.senet.state.Piece._
  
  sequential

  "Initialising a board" should {
    "produce a board with alternating piece types on the first row, starting with cone" in {
      val boardOpt = Rules.initialBoard(NullMove, Board(Nil))
      boardOpt.isDefined must beTrue
      boardOpt.get.pieces must_== Seq(cone(1), reel(2), cone(3), reel(4), cone(5), reel(6), cone(7), reel(8), cone(9), reel(10))
    }
  }

  "Making a simple move" should {
    "produce a board with only that piece moved" in {
      val initialBoard = Rules.initialBoard(NullMove, Board(Nil)).get
      val boardAfterMoveOpt = Rules.play(PieceMove(10, 5), initialBoard)
      boardAfterMoveOpt must beSome[Board]
      boardAfterMoveOpt.get.pieces must be_==(Seq(cone(1), reel(2), cone(3), reel(4), cone(5), reel(6), cone(7), reel(8), cone(9), reel(15)))
    }
  }

  "Moving a piece to a space occupied by an opponent" should {
    "lead to dancing" in {
      val board = Board(Seq(cone(1), reel(2), cone(3), reel(4), cone(5), reel(6), cone(7), reel(8), cone(9), reel(12)))
      val boardAfterMoveOpt = Rules.play(PieceMove(9, 3), board)
      boardAfterMoveOpt must beSome[Board]
      boardAfterMoveOpt.get.pieces must be_==(Seq(cone(1), reel(2), cone(3), reel(4), cone(5), reel(6), cone(7), reel(8), reel(9), cone(12)))
    }
    "not lead to dancing if the target is accompanied by a piece of the same type before it" in {
      val board = Board(Seq(cone(1), reel(2), cone(3), reel(4), cone(5), reel(6), cone(7), cone(9), reel(11), reel(12)))
      val boardAfterMoveOpt = Rules.play(PieceMove(9, 3), board)
      boardAfterMoveOpt must beNone
    }
    "not lead to dancing if the target is accompanied by a piece of the same type after it" in {
      val board = Board(Seq(cone(1), reel(2), cone(3), reel(4), cone(5), reel(6), cone(7), cone(9), reel(12), reel(13)))
      val boardAfterMoveOpt = Rules.play(PieceMove(9, 3), board)
      boardAfterMoveOpt must beNone
    }
    "not lead to dancing if the target is accompanied by a piece of the same type before it when there are three in a row" in {
      val board = Board(Seq(cone(1), reel(2), cone(3), reel(4), cone(5), cone(7), cone(9), reel(10), reel(11), reel(12)))
      val boardAfterMoveOpt = Rules.play(PieceMove(9, 3), board)
      boardAfterMoveOpt must beNone
    }
  }

  "Moving a piece to a space occupied by a piece of the same type" should {
    "be an illegal move" in  {
      val board = Board(Seq(cone(1), reel(2), cone(3), reel(4), cone(5), reel(6), cone(7), reel(8), cone(9), reel(12)))
      val boardAfterMoveOpt = Rules.play(PieceMove(8, 4), board)
      boardAfterMoveOpt must beNone
    }
  }

  "Moving a piece over three consecutive opponent pieces" should {
    "be an illegal move" in {
      val board = Board(Seq(cone(1), reel(2), cone(3), reel(4), cone(5), cone(7), cone(9), reel(10), reel(11), reel(12)))
      val boardAfterMoveOpt = Rules.play(PieceMove(9, 4), board)
      boardAfterMoveOpt must beNone
    }
  }

  "Moving a piece over three non-consecutive opponent pieces" should {
    "be a legal move" in {
      val board = Board(Seq(cone(1), reel(2), cone(3), reel(4), cone(5), cone(7), cone(8), reel(9), reel(11), reel(12)))
      val boardAfterMoveOpt = Rules.play(PieceMove(8, 6), board)
      boardAfterMoveOpt must beSome[Board]
      boardAfterMoveOpt.get must be_==(Board(Seq(cone(1), reel(2), cone(3), reel(4), cone(5), cone(7), reel(9), reel(11), reel(12), cone(14))))
    }
  }

  "When drowning it" should {
    "be illegal to move any piece but the drowning one" in {
      val board = Board(Seq(cone(1), reel(2), cone(3), reel(4), cone(5), cone(7), cone(8), reel(9), reel(11), reel(22), reel(27)))
      Rules.play(PieceMove(11, 1), board) must beNone
      Rules.play(PieceMove(2, 6), board) must beNone
      Rules.play(PieceMove(9, 2), board) must beNone
    }
    "be legal for any throw for the drowning piece even if it is not 2 (but the board should not change)" in {
      val board = Board(Seq(cone(1), reel(2), cone(3), reel(4), cone(5), cone(7), cone(8), reel(9), reel(11), reel(22), reel(27)))
      Rules.play(PieceMove(Rules.SeaOfHumiliation, 1), board) must beSome(board)
      Rules.play(PieceMove(Rules.SeaOfHumiliation, 6), board) must beSome(board)
      Rules.play(PieceMove(Rules.SeaOfHumiliation, 3), board) must beSome(board)
    }
    "be legal to move the drowning piece on a move of 2" in {
      val board =         Board(Seq(cone(1), reel(2), cone(3), reel(4), cone(5), cone(7), cone(8), reel(9), reel(11), reel(22), reel(27)))
      val expectedBoard = Board(Seq(cone(1), reel(2), cone(3), reel(4), cone(5), cone(7), cone(8), reel(9), reel(11), reel(15), reel(27)))
      Rules.play(PieceMove(Rules.SeaOfHumiliation, 2), board) must beSome[Board](expectedBoard)
    }
    "move the drowning piece in the next available space when the recovery space is occupied" in {
      ko
    }
    "move the drowning piece in the next available space when the recovery space is occupied x 2" in {
      ko
    }
    "move the drowning piece in the next available space when the recovery space is occupied x 3" in {
      ko
    }
    "move the drowning piece in the next available space when the recovery space is occupied x 4" in {
      ko
    }
  }
}
