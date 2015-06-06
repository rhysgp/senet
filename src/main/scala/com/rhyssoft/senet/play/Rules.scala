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

  val SEA_OF_HUMILIATION_POS = 27
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

    move match {
      case PieceMove(position, spaces) =>
        val movingPieceOpt = board.pieceAt(position)
        val targetOpt = board.pieceAt(position + spaces)
        val targetPlus1Opt = board.pieceAt(position + spaces + 1)
        val targetMinus1Opt = board.pieceAt(position + spaces - 1)

        /*
         * 1. There's a piece at the point in the move:
         */
        movingPieceOpt.isDefined &&
          /*
           * 2. A piece fallen into the Sea of Humiliation drowns the player: no other moves can be made except
           * on the piece that fell in; but any throw is valid, even if it doesn't result in a move.
           */
          pieceIsDrowning(movingPieceOpt.get) ||
            /*
             * 2b. If any other piece is drowning, the moving piece can't be moved:
             */
            (!playerIsDrowning(movingPieceOpt.get, board) &&
              /*
               * 3. The target space is occupied either by nothing or by a piece of the opposite type:
               */
              (targetOpt.isEmpty || !areSameType(targetOpt, movingPieceOpt)) &&
              /*
               * 4. A piece cannot be danced when accompanied by a fellow (either one before or one after):
               */
              (targetOpt.isEmpty || !(areSameType(targetOpt, targetPlus1Opt) || areSameType(targetOpt, targetMinus1Opt))) &&
              /*
               * 5. A piece cannot jump over three consecutive opponent pieces:
               */
              board.pieces
                .filter(p => p.position > position && p.position < position + spaces)
                .filter(p => !areSameType(p, movingPieceOpt.get))
                .foldLeft((0, 0))((acc, p) => acc match {
                case (consecutiveCount, lastPos) =>
                  if (p.position == lastPos + 1)
                    (consecutiveCount + 1, p.position)
                  else
                    (1, p.position)
              })._1 < 3)

      case _ => true
    }

  }

  private def playerIsDrowning(piece: Piece, board: Board): Boolean = {
    board.pieces.filter(areSameType(_, piece)).exists(_.position == SEA_OF_HUMILIATION_POS)
  }

  private def pieceIsDrowning(piece: Piece): Boolean = {
    piece.position == SEA_OF_HUMILIATION_POS
  }

  private def areSameType(p1: Option[Piece], p2: Option[Piece]): Boolean = {
    p1.isDefined && p2.isDefined && areSameType(p1.get, p2.get)
  }

  private def areSameType(p1: Piece, p2: Piece): Boolean = {
    p1.getClass == p2.getClass
  }
}
