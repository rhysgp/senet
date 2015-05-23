/*
 * Copyright (c) 2015 Rhys Parsons. All rights reserved.
 */

package com.rhyssoft.senet.play

import com.rhyssoft.senet.state.{Piece, Reel, Cone, Board}

/**
 * The rules of the game.
 *
 * TODO: work out the statistics of getting each throw (1, 2, 3, 4, 6)
 *
 *
 *
 * @author Rhys Parsons
 */
object Rules {

  type Play = (Move, Board) => Option[Board]

  def initialBoard(move: Move, board: Board): Option[Board] = {
    Some(Board(
      for (pos <- 1 to 10) yield {
        if (pos % 2 == 0)
          Reel(pos)
        else
          Cone(pos)
      }
    ))
  }

  def play(move: Move, board: Board): Option[Board] = {
    if (validate(move, board)) {
      Some(moveIt(move, board))
    } else {
      None
    }
  }

  private[play] def moveIt(move: Move, board: Board): Board = {
    move match {
      case PieceMove(pieceAtPos, spaces) =>
        val piecesOpts = board.pieces.map(Some(_): Option[Piece])
        val unaffectedPieces =
          piecesOpts.filter(p => p.get.position != pieceAtPos && p.get.position != pieceAtPos + spaces).toList
        val pieceOpt: Option[Piece] = board.pieceAt(pieceAtPos).map(_.move(spaces))
        val dancedPieceOpt: Option[Piece] = board.pieceAt(pieceAtPos + spaces).map(_.move(-spaces))
        val sorted = (pieceOpt :: dancedPieceOpt :: unaffectedPieces)
          .flatten
          .sortBy(_.position)
        Board(sorted)
      case NullMove =>
        board
    }
  }

  def validate(move: Move, board: Board): Boolean = {
    /*
     * 1.
     */
    true
  }
}
