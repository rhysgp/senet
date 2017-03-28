/*
 * Copyright (c) 2015 Rhys Parsons. All rights reserved.
 */

package com.rhyssoft.senet.play

import com.rhyssoft.senet.state.{Board, Piece}

import scala.annotation.tailrec

/**
  * The rules of the game.
  *
  * @todo work out the statistics of getting each throw (1, 2, 3, 4, 6)
  * @todo The possible results of a play are: IllegalMove, MoveApplied, GameOver
  * @todo On playing a move, whose turn is next should also be returned.
  *
  *
  * @author Rhys Parsons
  */
object Rules {

  type Play = (Move, Board) => Option[Board]


  private val log = TempLogger

  private val protectedSpaces = Seq(26, 28, 29)

  val seaOfHumiliation = 27
  val escapePosition = 15

  def initialBoard(move: Move, board: Board): Option[Board] = {
    Some(Board(
      for (pos <- 1 to 10) yield {
        if (pos % 2 == 0)
          Piece.reel(pos)
        else
          Piece.cone(pos)
      }
    ))
  }

  def play(move: Move, board: Board): Option[Board] = {

    move match {
      case pm: PieceMove =>
        validate(pm, board) match {
          case ValidatedMove =>
            Some(moveIt(pm, board))
          case ValidatedDance =>
            Some(dance(pm, board))
          case ValidatedEscapeSeaOfHumiliation =>
            Some(escapeSeeOfHumiliation(board))
          case ValidatedNoMove =>
            Some(board)
          case NotValidated(msg) =>
            None
        }
      case NullMove =>
        Some(board)
    }

  }

  private[play] def moveIt(move: PieceMove, board: Board): Board = {
    move match {
      case PieceMove(pieceAtPos, spaces) =>
        val piecesOpts = board.pieces.map(Option(_))
        val unaffectedPieces =
          piecesOpts.filter(p => p.get.position != pieceAtPos && p.get.position != pieceAtPos + spaces).toList
        val pieceOpt: Option[Piece] = board.pieceAt(pieceAtPos).map(_.move(spaces))
        val dancedPieceOpt: Option[Piece] = board.pieceAt(pieceAtPos + spaces).map(_.move(-spaces))
        val sorted = (pieceOpt :: dancedPieceOpt :: unaffectedPieces)
          .flatten
          .sortBy(_.position)
        Board(sorted)
    }
  }

  private def dance(move: PieceMove, board: Board): Board = {
    val sourcePosition = move.pieceAtPosition
    val targetPosition = move.pieceAtPosition + move.spaces

    val byStanders = board.pieces.filterNot(p => p.position == sourcePosition || p.position == targetPosition)
    val sourcePiece = board.pieceAt(sourcePosition).get
    val targetPiece = board.pieceAt(targetPosition).get

    val newOrder = (byStanders :+ sourcePiece.copy(position = targetPosition) :+ targetPiece.copy(position = sourcePosition)).sortBy(_.position)

    board.copy(pieces = newOrder)
  }

  private def escapeSeeOfHumiliation(board: Board): Board = {
    val byStanders = board.pieces.filterNot(_.position == seaOfHumiliation)
    val drowningPiece = board.pieceAt(seaOfHumiliation).get
    val newPosition = emptyEscapePosition(board, escapePosition)
    val newOrder = (byStanders :+ drowningPiece.copy(position = newPosition)).sortBy(_.position)
    board.copy(pieces = newOrder)
  }

  @tailrec
  private def emptyEscapePosition(board: Board, pos: Int): Int = {
    board.pieceAt(pos) match {
      case Some(_) => emptyEscapePosition(board, pos - 1)
      case None => pos
    }
  }

  def validate(move: PieceMove, board: Board): ValidationResult = {

    move match {
      case PieceMove(position, spaces) =>
        val targetPosition = position + spaces
        val movingPieceOpt = board.pieceAt(position)
        val targetOpt = board.pieceAt(targetPosition)

        movingPieceOpt match {
          case Some(movingPiece) =>
            if (pieceIsDrowning(movingPiece)) {

              if (move.spaces == 2) {
                log.debug("Piece is drowning: escaped Sea of Humiliation with a throw of 2.")
                ValidatedEscapeSeaOfHumiliation
              } else {
                log.debug(s"Piece is drowning: any throw is valid, but the board won't change on a throw of ${move.spaces}.")
                ValidatedNoMove
              }
            } else {

              // piece is not drowning, continue validation:
              if (playerIsDrowning(movingPieceOpt.get, board)) {
                log.info("Player is drowning: can't move _that_ piece!")
                NotValidated("Player is drowning: can't move _that_ piece!")

              } else {
                // is there a piece of the same type in the target?
                if (targetOpt.isDefined && areSameType(movingPiece, targetOpt.get)) {
                  log.info("A piece of the same type occupies the target space: can't move there!")
                  NotValidated("A piece of the same type occupies the target space: can't move there!")
                } else {

                  // is there a piece of the opposite type that is protected?
                  if (targetOpt.isDefined) {
                    if (isProtected(targetOpt.get, board)) {
                      log.info("Target piece is protected: can't dance with _her_!")
                      NotValidated("Target piece is protected: can't dance with _her_!")
                    } else {
                      log.debug("Validated dance!")
                      ValidatedDance
                    }
                  } else {
                    if (rangeContainsTripletOfOpposingType(movingPiece.position, targetPosition, movingPiece, board)) {
                      log.info("Can't jump of triplet of opposing player!")
                      NotValidated("Can't jump of triplet of opposing player!")
                    } else {
                      ValidatedMove
                    }
                  }
                }
              }
            }

          case None =>
            log.error(s"No piece found at position $position")
            NotValidated(s"No piece found at position $position")
        }

      case _ => ValidatedNoMove
    }

  }

  private def playerIsDrowning(piece: Piece, board: Board): Boolean = {
    board.pieceAt(seaOfHumiliation).exists(areSameType(_, piece))
  }

  private def pieceIsDrowning(piece: Piece): Boolean = {
    piece.position == seaOfHumiliation
  }

  private def areSameType(p1: Piece, p2: Piece): Boolean = p1.pieceType == p2.pieceType

  private def areSameType(p1: Piece, p2: Option[Piece]): Boolean = p2.isDefined && areSameType(p1, p2.get)

  private def isProtected(p: Piece, board: Board): Boolean = {
      protectedSpaces.contains(p.position) ||
        (p.position >  1 && areSameType(p, board.pieceAt(p.position - 1))) ||
        (p.position < 30 && areSameType(p, board.pieceAt(p.position + 1)))

  }

  private def rangeContainsTripletOfOpposingType(from: Int, to: Int, movingPiece: Piece, board: Board): Boolean = {
    board.pieces
      .filter(p => p.pieceType != movingPiece.pieceType && p.position >= from && p.position <= to)
      .map(_.position)
      .foldLeft(Seq[Int]())((r, i) =>
        if (r.length == 3)
          r
        else if (r.nonEmpty && r.last == i - 1)
          r :+ i
        else
          Seq(i)
      )
      .length == 3
  }

}

sealed trait ValidationResult
case object ValidatedMove extends ValidationResult
case object ValidatedDance extends ValidationResult
case object ValidatedNoMove extends ValidationResult
case object ValidatedEscapeSeaOfHumiliation extends ValidationResult
case class NotValidated(reason: String) extends ValidationResult


//sealed trait MoveResult
//case object Moved(nextPlayer: )


object TempLogger {

  def debug(s: String) = println("DEBUG: " + s)
  def info(s: String) = println("INFO: " + s)
  def error(s: String) = println("ERROR: " + s)
}
