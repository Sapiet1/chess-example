use std::{fmt, error::Error};

// An enum to name the variants of
// the errors that may occur. Check errors
// are different, however.
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum MoveError {
    Syntax,
    Style,
    Take,
    Path,
    Square,
    Color,
}

impl fmt::Display for MoveError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let err_msg = match self {
            MoveError::Syntax => "invalid move syntax",
            MoveError::Style => "invalid move style",
            MoveError::Take => "invalid takable piece",
            MoveError::Path => "invalid and obstructed path",
            MoveError::Square => "invalid chosen square",
            MoveError::Color => "invalid piece color",
        };

        write!(f, "{}", err_msg)
    }
}

impl Error for MoveError { }

// The check error. The explanation
// for why it is different is seen with
// the ErrorID enum below.
#[derive(Debug)]
pub struct CheckError;

impl Error for CheckError {  }

impl fmt::Display for CheckError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "check invalidation")
    }
}

impl CheckError {
    pub fn error() -> Self {
        CheckError
    }
}

pub trait BoardError: Error + Send + Sync {
    fn id(&self) -> ErrorID;
}
impl BoardError for MoveError {
    fn id(&self) -> ErrorID {
        ErrorID::Move(*self)
    }
}
impl BoardError for CheckError {
    fn id(&self) -> ErrorID {
        ErrorID::Check
    }
}

// This allows for us to check if,
// at the end of the game, it was a stalemate
// or checkmate.
#[derive(PartialEq)]
pub enum ErrorID {
    Move(MoveError),
    Check,
}