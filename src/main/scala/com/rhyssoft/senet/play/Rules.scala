/*
 * Copyright (c) 2015 Rhys Parsons. All rights reserved.
 */

package com.rhyssoft.senet.play

import com.rhyssoft.senet.state.{Reel, Cone, Board}

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
      Some(Board(Nil))
    } else {
      None
    }
  }

  def validate(move: Move, board: Board): Boolean = {
    /*
     * 1.
     */
    true
  }
}
