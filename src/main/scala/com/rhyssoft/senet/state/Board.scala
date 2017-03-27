/*
 * Copyright (c) 2015 Rhys Parsons. All rights reserved.
 */

package com.rhyssoft.senet.state

import com.rhyssoft.senet.state.PieceType.PieceType

object  PieceType extends Enumeration {
  type PieceType = Value
  val Cone, Reel = Value
}

case class Piece(position: Int, pieceType: PieceType) {

  def move(spaces: Int) = copy(position = position + spaces)
}

object Piece {
  def cone(pos: Int) = Piece(pos, PieceType.Cone)
  def reel(pos: Int) = Piece(pos, PieceType.Reel)
}


case class Board(pieces: Seq[Piece]) {
  def pieceAt(position: Int): Option[Piece] = {
    pieces.find(_.position == position)
  }
}

