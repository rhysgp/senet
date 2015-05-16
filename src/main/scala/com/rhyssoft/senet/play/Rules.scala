/*
 * Copyright (c) 2015 Rhys Parsons. All rights reserved.
 */

package com.rhyssoft.senet.play

import com.rhyssoft.senet.state.Board

/**
 * The rules of the game.
 * @author Rhys Parsons
 */
class Rules {

  type Play = (Move, Board) => Option[Board]

  def play(move: Move, board: Board): Option[Board] = {
    if (validate(move, board)) {
      Some(Board(Nil))
    } else {
      None
    }
  }

  def validate(move: Move, board: Board): Boolean = {
    true
  }
}
