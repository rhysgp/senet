/*
 * Copyright (c) 2015 Rhys Parsons. All rights reserved.
 */

package com.rhyssoft.senet.play

/**
 * Represents a move played.
 */

sealed trait Move

case object NullMove extends Move

case class PieceMove(pieceAtPosition: Int, spaces: Int) extends Move