use super::movable::PieceType;
use crate::{board:: Board, BOARD_DIMENSION};
use rayon::prelude::*;

use std::process; 

// The color enum used for the Board
// and the individual pieces.
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum Color {
    White,
    Black,
}

impl Color {
    pub fn opposite(&self) -> Self {
        match self {
            Color::White => Color::Black,
            Color::Black => Color::White,
        }
    }

    pub fn to_char(&self) -> char {
        match self {
            Color::White => 'w',
            Color::Black => 'b',
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            Color::White => String::from("White"),
            Color::Black => String::from("Black"),
        }
    }
}

// Here, we create a Piece type.
// It has a color, a boolean for
// if it has moved yet, and a piece_type.
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct Piece {
    pub color: Color,
    pub has_moved: bool,
    pub piece_type: PieceType,
}


pub struct PiecesList {
    white_pieces: Vec<(Piece, [usize; 2])>,
    black_pieces: Vec<(Piece, [usize; 2])>,
}

impl PiecesList {
    pub fn new() -> Self {
        let white_pieces = Vec::new();
        let black_pieces = Vec::new();
        
        PiecesList { white_pieces, black_pieces }
    }
    
    pub fn get_mut(&mut self, color: Color) -> &mut Vec<(Piece, [usize; 2])> {
        match color {
            Color::White => &mut self.white_pieces,
            Color::Black => &mut self.black_pieces,
        }
    }

    pub fn get(&self, color: Color) -> &Vec<(Piece, [usize; 2])> {
        match color {
            Color::White => &self.white_pieces,
            Color::Black => &self.black_pieces,
        }
    }

    pub fn from_board(board: &Board) -> Self {
        let pieces = board.par_iter().enumerate().fold(|| PiecesList::new(), |mut pieces, (index, square)| {
            let &Square::Busy(piece) = square else {
                return pieces
            };

            let row = index / BOARD_DIMENSION;
            let column = index % BOARD_DIMENSION;

            pieces.get_mut(piece.color).push((piece, [row, column]));

            pieces
        })
        .reduce(|| PiecesList::new(), |mut pieces_out, pieces| {
            pieces_out.white_pieces.extend(pieces.white_pieces);
            pieces_out.black_pieces.extend(pieces.black_pieces);

            pieces_out
        });


        pieces
    }
}

// The Square. It has three variants,
// a piece, Empty, or an EnPassant square.
#[derive(Debug, Default, PartialEq, Eq, Hash, Clone, Copy)]
pub enum Square {
    Busy(Piece),
    #[default]
    Empty,
    EnPassant(u8),
}

impl Square {
    // We can create a new Square::Busy(piece) with this function:
    pub fn new_piece(color: char, piece_type: &str) -> Self {
        let color = match color.to_ascii_lowercase() {
            'w' => Color::White,
            'b' => Color::Black,
            _ => process::exit(1),
        };
        let has_moved = false;
        let piece_type = match &piece_type.to_lowercase()[..] {
            "king" | "k" => PieceType::King,
            "queen" | "q" => PieceType::Queen,
            "rook" | "r" => PieceType::Rook,
            "bishop" | "b" => PieceType::Bishop,
            "knight" | "n" => PieceType::Knight,
            "pawn" | "p" => PieceType::Pawn,
            _ => process::exit(1),
        };
        
        Square::Busy(Piece { color, has_moved, piece_type })
    }

    // Here, we set the piece to has_moved if it is one.
    // This isn't needed anymore as get_mut() exists.
    pub fn set_moved(&mut self) {
        let Square::Busy(piece) = self else {
            process::exit(1)
        };

        if !piece.has_moved { piece.has_moved = true; };
    }

    pub fn get(&self) -> &Piece {
        let Square::Busy(piece) = self else {
            eprintln!("square was not a piece");
            process::exit(1)
        };

        piece
    }

    pub fn get_mut(&mut self) -> &mut Piece {
        let Square::Busy(piece) = self else {
            eprintln!("square was not a piece");
            process::exit(1);
        };

        piece
    }
}

