/*
 * Copyright (c) 2015 Rhys Parsons. All rights reserved.
 */

package com.rhyssoft.senet.state

/**
 * The state.
 * @author Rhys Parsons
 */

sealed trait Piece {
  def position: Int
  def move(spaces: Int): Piece
}

case class Cone(position: Int) extends Piece {
  def move(spaces: Int) = this.copy(position = position + spaces)
}

case class Reel(position: Int) extends Piece {
  def move(spaces: Int) = this.copy(position = position + spaces)
}

case class Board(pieces: Seq[Piece]) {
  def pieceAt(position: Int): Option[Piece] = {
    pieces.find(_.position == position)
  }
}

