/*
 * Copyright (c) 2015 Rhys Parsons. All rights reserved.
 */

package com.rhyssoft.senet.state

/**
 * The state.
 * @author Rhys Parsons
 */

sealed trait Piece {}

case class Cone(position: Int) extends Piece {}

case class Reel(position: Int) extends Piece {}

case class Board(pieces: Seq[Piece])

