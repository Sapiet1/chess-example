use std::{iter, mem};

use crate::{Board, BOARD_DIMENSION, FIFTY_MOVE_RULE, THREE_REPEATS_RULE};
use crate::square::{Square, Color};
use crate::errors::{MoveError, BoardError};

struct King;
struct Queen;
struct Rook;
struct Bishop;
struct Knight;
struct Pawn;

trait Move {
    fn do_move(
        board: &mut Board, 
        position_start: [usize; 2], 
        position_end: [usize; 2],
        has_moved: bool,
        check_checks: bool,
        // Checking for promotions is just to be utilized
        // by the pawn when it is promoting.
        check_promotion: bool,
        is_actual_move: bool,
    ) -> Result<MoveDetails, Box<dyn BoardError>>;
}

impl Move for King {
    fn do_move(    
        board: &mut Board, 
        position_start: [usize; 2], 
        position_end: [usize; 2],
        has_moved: bool,
        check_checks: bool,
        _check_promotion: bool,
        is_actual_move: bool,
    ) -> Result<MoveDetails, Box<dyn BoardError>>
    {
        // We can guarantee the square is, in fact, a King and of the correct color
        // in the board do_move method.

        // Here, we just get the differences between the two positions.
        let position_one_diff = position_start[0] as i16 - position_end[0] as i16;
        let position_two_diff = position_start[1] as i16 - position_end[1] as i16;

        // Condition to know it's a valid move for a King:
        if position_one_diff.abs() <= 1 && position_two_diff.abs() <= 1 
        && position_start.iter().zip(position_end.iter()).any(|(a, b)| a != b)
        {
            let square: &Square = &board.map[position_end[0]][position_end[1]];
            // If the square dictated by the position_end argument has a piece in it,
            // we need to check if it's of the opposite color. 
            if let Square::Busy(piece) = square {
                let opposite_color = board.color.opposite();
                if piece.color == opposite_color {
                    let ok = board.write_andor_check(position_start, position_end, check_checks, false, MoveDetails::Take)?;
                    if is_actual_move {
                        *FIFTY_MOVE_RULE.lock().unwrap() = 0;
                        THREE_REPEATS_RULE.lock().unwrap().clear();
                    }

                    Ok(ok)
                } else {
                    Err(Box::new(MoveError::Take))
                }
            } else {
                let ok = board.write_andor_check(position_start, position_end, check_checks, false, MoveDetails::Move)?;
                if is_actual_move {
                    *FIFTY_MOVE_RULE.lock().unwrap() += 1;
                    THREE_REPEATS_RULE.lock().unwrap().push(*board);
                }

                Ok(ok)
            }
        // Castling requires the King hasn't moved:
        } else if position_two_diff.abs() == 2 && position_one_diff == 0 && !has_moved {
            // We're constructing an iterator to check for
            // if we're trying to castle. 
            let rows = {
                // We know for sure that there's going to be at most 8
                // columns in the columns iterator (actually 2, but this works).
                iter::repeat(position_start[0]).take(BOARD_DIMENSION).collect::<Vec<_>>()
            };
            let columns = {
                // We have to keep check for if the difference is negative
                // for if it was, there'd be a range of 0 (e.g., 5..1 is an iterator of nothing).
                if position_two_diff.is_negative() {
                    iter::once(position_end[1]).chain((position_start[1]..position_end[1]).skip(1).rev()).collect::<Vec<_>>()
                } else {
                    (position_end[1]..position_start[1]).rev().collect::<Vec<_>>()
                }
            };

            // Unlike with other moves, we're checking for checks from
            // within the move function itself. This is because
            // there are multiple squares we need to check, not just one. 
            // It's more simple. This also checks for path obstructions.
            for (row, column) in rows.into_iter().zip(columns.into_iter()) {
                if let Square::Busy(_) = board.map[row][column] {
                    return Err(Box::new(MoveError::Path))
                } else if check_checks {
                    board.check_checks(Some([row, column]))?;
                }
            }

            // The supposed position of the Rook. We can tell
            // where to look based off the positional difference.
            let rook_position = {
                if position_two_diff.is_negative() {
                    [position_end[0], position_end[1] + 1]
                } else {
                    [position_end[0], position_end[1] - 2]
                }
            };

            // If it's not a Rook, then we return an error.
            if let Square::Busy(piece) = &board.map[rook_position[0]][rook_position[1]] {
                if piece.has_moved { return Err(Box::new(MoveError::Style)) }
            } else {
                return Err(Box::new(MoveError::Style))
            }

            // If it is, we grab where it's supposed to 
            // be written to.
            let rook_position_end = {
                if position_two_diff.is_negative() {
                    [position_end[0], position_end[1] - 1]
                } else {
                    [position_end[0], position_end[1] + 1]
                }
            };

            // Now, we write what is needed to be written: 
            let ok = board.write_andor_check(position_start, position_end, false, false, MoveDetails::Move).unwrap();
            let _ = board.write_andor_check(rook_position, rook_position_end, false, false, MoveDetails::Take);
            if is_actual_move {
                *FIFTY_MOVE_RULE.lock().unwrap() += 1;
                THREE_REPEATS_RULE.lock().unwrap().push(*board);
            }

            Ok(ok)
        } else {
            // If you fail the move conditions for the piece, 
            // it's an immediate error.
            Err(Box::new(MoveError::Style))
        }
    }
}

impl Move for Queen {
    fn do_move(
        board: &mut Board, 
        position_start: [usize; 2], 
        position_end: [usize; 2],
        _has_moved: bool,
        check_checks: bool,
        _check_promotion: bool,
        is_actual_move: bool,
    ) -> Result<MoveDetails, Box<dyn BoardError>>
    {
        let position_one_diff = position_start[0] as i16 - position_end[0] as i16;
        let position_two_diff = position_start[1] as i16 - position_end[1] as i16;

        // Moves are not what the piece itself can do, but what we suggest it
        // to do. Thus, we can just reuse the Rook and Bishop functions.
        // It should be noted that the conditionals are still noted, because
        // if it fails them when passed into the other functions, we return 
        // the value when it fails the conditionals in those functions even when it's valid
        // (a Rook cannot move like a Bishop and vice versa).
        if [position_one_diff, position_two_diff].contains(&0) && position_one_diff != position_two_diff {
            return Rook::do_move(board, position_start, position_end, _has_moved, check_checks, false, is_actual_move);
        } else if position_one_diff.abs() - position_two_diff.abs() == 0 && (position_one_diff, position_two_diff) != (0, 0) {
            return Bishop::do_move(board, position_start, position_end, _has_moved, check_checks, false, is_actual_move);
        } else {
            return Err(Box::new(MoveError::Style))
        }
    }
}

impl Move for Rook {
    fn do_move(
        board: &mut Board, 
        position_start: [usize; 2], 
        position_end: [usize; 2],
        _has_moved: bool,
        check_checks: bool,
        _check_promotion: bool,
        is_actual_move: bool,
    ) -> Result<MoveDetails, Box<dyn BoardError>>
    {
        let position_one_diff = position_start[0] as i16 - position_end[0] as i16;
        let position_two_diff = position_start[1] as i16 - position_end[1] as i16;

        if [position_one_diff, position_two_diff].contains(&0) && position_one_diff != position_two_diff {
            // We create rows and columns for us to check if the path is obstructed.
            let rows = {
                if position_one_diff.is_negative() { (position_start[0]..position_end[0]).skip(1).collect::<Vec<_>>() }
                else if position_one_diff == 0 { iter::repeat(position_start[0]).take(BOARD_DIMENSION).collect::<Vec<_>>() }
                else { (position_end[0]..position_start[0]).skip(1).collect::<Vec<_>>() }
            };
            let columns = {
                if position_two_diff.is_negative() { (position_start[1]..position_end[1]).skip(1).collect::<Vec<_>>() }
                else if position_two_diff == 0 { iter::repeat(position_start[1]).take(BOARD_DIMENSION).collect::<Vec<_>>() }
                else { (position_end[1]..position_start[1]).skip(1).collect::<Vec<_>>() }
            };

            // Checking for said obstructions:
            for (row, column) in rows.into_iter().zip(columns.into_iter()) {
                if let Square::Busy(_) = board.map[row][column] {
                    return Err(Box::new(MoveError::Path))
                }
            }

            // Re: King.
            let square = &board.map[position_end[0]][position_end[1]];
            if let Square::Busy(piece) = square {
                let opposite_color = board.color.opposite();
                if piece.color == opposite_color {                        
                    let ok = board.write_andor_check(position_start, position_end, check_checks, false, MoveDetails::Take)?;
                    if is_actual_move {
                        *FIFTY_MOVE_RULE.lock().unwrap() = 0;
                        THREE_REPEATS_RULE.lock().unwrap().clear();
                    }

                    Ok(ok)
                } else {
                    Err(Box::new(MoveError::Take))
                }
            } else {
                let ok = board.write_andor_check(position_start, position_end, check_checks, false, MoveDetails::Move)?;
                if is_actual_move {
                    *FIFTY_MOVE_RULE.lock().unwrap() += 1;
                    THREE_REPEATS_RULE.lock().unwrap().push(*board);
                }

                Ok(ok)
            }
        } else {
            Err(Box::new(MoveError::Style))
        }
    }
}

impl Move for Bishop {
    fn do_move(
        board: &mut Board, 
        position_start: [usize; 2], 
        position_end: [usize; 2],
        _has_moved: bool,
        check_checks: bool,
        _check_promotion: bool,
        is_actual_move: bool,
    ) -> Result<MoveDetails, Box<dyn BoardError>>
    {
        let position_one_diff = position_start[0] as i16 - position_end[0] as i16;
        let position_two_diff = position_start[1] as i16 - position_end[1] as i16;

        if position_one_diff.abs() - position_two_diff.abs() == 0 && (position_one_diff, position_two_diff) != (0, 0) {
            // Re: Rook.
            let rows = {
                if position_one_diff.is_negative() { (position_start[0]..position_end[0]).skip(1).collect::<Vec<_>>() }
                else { (position_end[0]..position_start[0]).skip(1).rev().collect::<Vec<_>>() }
            };
            let columns = {
                if position_two_diff.is_negative() { (position_start[1]..position_end[1]).skip(1).collect::<Vec<_>>() }
                else { (position_end[1]..position_start[1]).skip(1).rev().collect::<Vec<_>>() }
            };

            for (row, column) in rows.into_iter().zip(columns.into_iter()) {
                if let Square::Busy(_) = board.map[row][column] {
                    return Err(Box::new(MoveError::Path))
                }
            }

            let square: &Square = &board.map[position_end[0]][position_end[1]];
            if let Square::Busy(piece) = square {
                let opposite_color = board.color.opposite();
                if piece.color == opposite_color {                        
                    let ok = board.write_andor_check(position_start, position_end, check_checks, false, MoveDetails::Take)?;
                    if is_actual_move {
                        *FIFTY_MOVE_RULE.lock().unwrap() = 0;
                        THREE_REPEATS_RULE.lock().unwrap().clear();
                    }

                    Ok(ok)
                } else {
                    Err(Box::new(MoveError::Take))
                }
            } else {
                let ok = board.write_andor_check(position_start, position_end, check_checks, false, MoveDetails::Move)?;
                if is_actual_move {
                    *FIFTY_MOVE_RULE.lock().unwrap() += 1;
                    THREE_REPEATS_RULE.lock().unwrap().push(*board);
                }

                Ok(ok)
            }
        } else {
            Err(Box::new(MoveError::Style))
        }
    }
}

impl Move for Knight {
    fn do_move(
        board: &mut Board, 
        position_start: [usize; 2], 
        position_end: [usize; 2],
        _has_moved: bool,
        check_checks: bool,
        _check_promotion: bool,
        is_actual_move: bool,
    ) -> Result<MoveDetails, Box<dyn BoardError>>
    {
        let position_one_diff = position_start[0] as i16 - position_end[0] as i16;
        let position_two_diff = position_start[1] as i16 - position_end[1] as i16;

        let p_one_p_two = [position_one_diff.abs(), position_two_diff.abs()];
        if p_one_p_two.contains(&1) && p_one_p_two.contains(&2)  {
            // Re: King.
            let square: &Square = &board.map[position_end[0]][position_end[1]];
            if let Square::Busy(piece) = square {
                let opposite_color = board.color.opposite();
                if piece.color == opposite_color {
                    let ok = board.write_andor_check(position_start, position_end, check_checks, false, MoveDetails::Take)?;
                    if is_actual_move {
                        *FIFTY_MOVE_RULE.lock().unwrap() = 0;
                        THREE_REPEATS_RULE.lock().unwrap().clear();
                    }

                    Ok(ok)
                } else {
                    Err(Box::new(MoveError::Take))
                }
            } else {
                let ok = board.write_andor_check(position_start, position_end, check_checks, false, MoveDetails::Move)?;
                if is_actual_move {
                    *FIFTY_MOVE_RULE.lock().unwrap() += 1;
                    THREE_REPEATS_RULE.lock().unwrap().push(*board);
                }

                Ok(ok)
            }
        } else {
            Err(Box::new(MoveError::Style))
        }
    }
}

impl Move for Pawn {
    fn do_move(
        board: &mut Board, 
        position_start: [usize; 2], 
        position_end: [usize; 2],
        has_moved: bool,
        check_checks: bool,
        check_promotion: bool,
        is_actual_move: bool,
    ) -> Result<MoveDetails, Box<dyn BoardError>>
    {
        let position_one_diff = position_start[0] as i16 - position_end[0] as i16;
        let position_two_diff = position_start[1] as i16 - position_end[1] as i16;

        let one = match board.color {
            Color::White => 1, 
            Color::Black => -1,
        };

        let square = &board.map[position_end[0]][position_end[1]];

        if (position_one_diff, position_two_diff) == (one, 0) {
            // The pawn is a bit different: we don't need to check
            // for if the color is correct *here*. It moves differently.
            if let Square::Busy(_) = square { Err(Box::new(MoveError::Path)) }
            else {
                // Pawns can also promote, which is why said flag exists.
                let ok = board.write_andor_check(position_start, position_end, check_checks,
                    ((board.color == Color::White && position_end[0] == 0) || (board.color == Color::Black && position_end[0] == 7)) && check_promotion,
                    MoveDetails::Move,
                )?;
                if is_actual_move {
                    *FIFTY_MOVE_RULE.lock().unwrap() = 0;
                    THREE_REPEATS_RULE.lock().unwrap().clear();
                }

                Ok(ok)
            }
        // If the pawn hasn't moved yet, it can move twice. This is seen here:
        } else if (position_one_diff, position_two_diff) == (one * 2, 0) && !has_moved {
            if let Square::Busy(_) = square { return Err(Box::new(MoveError::Path)) }
            else if let Square::Busy(_) = &board.map[(position_end[0] as i16 + one) as usize][position_end[1]] { return Err(Box::new(MoveError::Path)) }
            else {
                // We need to construct an en passant square:
                let ok = board.write_andor_check(position_start, position_end, check_checks, false, MoveDetails::Move)?;
                board.map[(position_end[0] as i16 + one) as usize][position_end[1]] = Square::EnPassant(0);
                if is_actual_move {
                    *FIFTY_MOVE_RULE.lock().unwrap() = 0;
                    THREE_REPEATS_RULE.lock().unwrap().clear();
                }

                Ok(ok)
            }
        } else if (position_one_diff, position_two_diff.abs()) == (one, 1) {
            match square {
                // If it's Busy or an EnPassant, we take the square. For the
                // latter, we also need to remove the piece behind it.
                Square::Busy(piece) => {
                    let opposite_color = board.color.opposite();
                    if piece.color == opposite_color {
                        let ok = board.write_andor_check(position_start, position_end, check_checks,
                            ((board.color == Color::White && position_end[0] == 0) || (board.color == Color::Black && position_end[0] == 7)) && check_promotion,
                            MoveDetails::Take,
                        )?;
                        if is_actual_move {
                            *FIFTY_MOVE_RULE.lock().unwrap() = 0;
                            THREE_REPEATS_RULE.lock().unwrap().clear();
                        }

                        Ok(ok)
                    } else {
                        Err(Box::new(MoveError::Take))
                    }
                },
                Square::EnPassant(_) => {
                    let to_en_passant = mem::take(&mut board.map[(position_end[0] as i16 + one) as usize][position_end[1]]);
                    match board.write_andor_check(position_start, position_end, check_checks, false, MoveDetails::Take) {
                        Ok(move_details) => {
                            if is_actual_move {
                                *FIFTY_MOVE_RULE.lock().unwrap() = 0;
                                THREE_REPEATS_RULE.lock().unwrap().clear();
                            }

                            Ok(move_details)
                        },
                        // If checking for checks then fails, we have to undo it all.
                        Err(err) => {
                            board.map[(position_end[0] as i16 + one) as usize][position_end[1]] = to_en_passant;
                            Err(err)
                        },
                    }
                }
                _ => Err(Box::new(MoveError::Style)),
            }
        } else {
            Err(Box::new(MoveError::Style))
        }
    }
}

// Trait objects are difficult to work with
// and are extremely limiting (can't look inside value).
// Generics indicate that Square itself must use
// a generic, which means it can't be held
// with pieces of different types. Thus, I
// use an enum. We don't use structs within the
// the enum itself because we can already
// implement the logic here.
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum PieceType {
    King,
    Queen,
    Rook,
    Bishop,
    Knight,
    Pawn,
}

// Executing the move:
impl PieceType {
    pub fn do_move(
        &self,
        board: &mut Board,
        position_start: [usize; 2],
        position_end: [usize; 2],
        has_moved: bool,
        check_checks: bool,
        check_promotion: bool,
        is_actual_move: bool,
    ) -> Result<MoveDetails, Box<dyn BoardError>>
    {
        match self {
            PieceType::King => King::do_move(board, position_start, position_end, has_moved, check_checks, check_promotion, is_actual_move),
            PieceType::Queen => Queen::do_move(board, position_start, position_end, has_moved, check_checks, check_promotion, is_actual_move),
            PieceType::Rook => Rook::do_move(board, position_start, position_end, has_moved, check_checks, check_promotion, is_actual_move),
            PieceType::Bishop => Bishop::do_move(board, position_start, position_end, has_moved, check_checks, check_promotion, is_actual_move),
            PieceType::Knight => Knight::do_move(board, position_start, position_end, has_moved, check_checks, check_promotion, is_actual_move),
            PieceType::Pawn => Pawn::do_move(board, position_start, position_end, has_moved, check_checks, check_promotion, is_actual_move),
        }
    }
}

// The MoveDetails is particularly helpful
// when checking for checks.
#[derive(Debug)]
pub enum MoveDetails {
    Move,
    Take,
}