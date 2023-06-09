pub mod movable;
use movable::PieceType;

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
// if it has moved yet, a piece_type,
// and a way to test if it's a dummy
// for the check_checks method in Board.
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct Piece {
    pub color: Color,
    pub has_moved: bool,
    pub piece_type: PieceType,
    pub check_dummy: bool,
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
    pub fn new_piece(color: char, piece_type: &str, check_dummy: bool) -> Self {
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
        
        Square::Busy(Piece { color, has_moved, piece_type, check_dummy })
    }

    // Here, we set the piece to has_moved if it is one.
    // Otherwise, we exit the program entirely.
    pub fn set_moved(&mut self) {
        let Square::Busy(piece) = self else {
            process::exit(1)
        };

        if !piece.has_moved { piece.has_moved = true; };
    }
}