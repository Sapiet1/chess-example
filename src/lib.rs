use std::{iter, process, fmt, io, mem, sync::Mutex, collections::{HashMap, HashSet}};
// Rayon is a multi-threading lib:
use rayon::prelude::*;
// A Lazy cell for global static:
use once_cell::sync::Lazy;

mod errors;
use errors::{CheckError, BoardError};
pub use errors::{ErrorID, MoveError};
mod board;
use board::{Board, BOARD_DIMENSION};
use board::square::{Square, Color, Piece};
use board::movable::{MoveDetails, PieceType};

// The fifty move rule states that if there's no 
// captures or pawn moves in fifty moves, 
// the game will be a draw.  
static FIFTY_MOVE_RULE: Lazy<Mutex<u8>> = Lazy::new(|| Mutex::new(0));
const FIFTY_MOVES: u8 = 50;
// If the board repeats thrice, it's a draw. 
// We can simply clear the vector whenever
// a piece is taken as the board can never
// be replicated once more.
static THREE_REPEATS_RULE: Lazy<Mutex<Vec<Board>>> = Lazy::new(|| Mutex::new(Vec::new()));
const THREE_REPEATS: usize = 3;

#[derive(Copy, Clone)]
pub struct ChessBoard {
    board: Board,
    turn: Color,
    board_state: BoardState,
}

// Complementary to chessboard.
struct PiecesList {
    white_pieces: Vec<(Piece, [usize; 2])>,
    black_pieces: Vec<(Piece, [usize; 2])>,
}

impl PiecesList {
    fn new() -> Self {
        let white_pieces = Vec::new();
        let black_pieces = Vec::new();
        
        PiecesList { white_pieces, black_pieces }
    }
    
    fn get_mut(&mut self, color: Color) -> &mut Vec<(Piece, [usize; 2])> {
        match color {
            Color::White => &mut self.white_pieces,
            Color::Black => &mut self.black_pieces,
        }
    }

    fn get(&self, color: Color) -> &Vec<(Piece, [usize; 2])> {
        match color {
            Color::White => &self.white_pieces,
            Color::Black => &self.black_pieces,
        }
    }

    fn from_board(board: &Board) -> Self {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum BoardState {
    Actual,
    Test {
        test_state: TestState,
        previous_board: Board,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum TestState {
    WithCheck,
    WithoutCheck,
}

impl ChessBoard {
    fn to_board_state_actual(&mut self) {
        let BoardState::Test { previous_board: board, .. } = self.board_state else {
            // Already Actual:
            return
        };

        self.board = board;
        self.board_state = BoardState::Actual;

    }

    fn to_board_state_test(&mut self, test_state: TestState) {
        let previous_board = self.board;
        self.board_state = BoardState::Test { test_state, previous_board };
    }

    // Creating a new chess board:
    pub fn new() -> Self {
        let white_frontrow: Vec<[Square; BOARD_DIMENSION]> = iter::once({
            iter::repeat(Square::new_piece('w', "p"))
                .take(BOARD_DIMENSION).collect::<Vec<_>>()
                .try_into().unwrap()
        })
        .collect();
        let black_frontrow: Vec<[Square; BOARD_DIMENSION]> = iter::once({
            iter::repeat(Square::new_piece('b', "p"))
                .take(BOARD_DIMENSION).collect::<Vec<_>>()
                .try_into().unwrap()
        })
        .collect();

        let mut piece_order = ['r', 'n', 'b', 'q', 'k', 'b', 'n', 'r'].iter();

        let white_backrow: Vec<[Square; BOARD_DIMENSION]> = iter::once({
            let mut piece_order_clone = piece_order.clone();

            iter::from_fn(|| {
                if let Some(piece_type) = piece_order_clone.next() { 
                    Some(Square::new_piece('w', &piece_type.to_string())) 
                } else { 
                    None 
                }
            })
            .collect::<Vec<_>>()
            .try_into().unwrap()
        })
        .collect();
        let black_backrow: Vec<[Square; BOARD_DIMENSION]> = iter::once({
            iter::from_fn(|| {
                if let Some(piece_type) = piece_order.next() { 
                    Some(Square::new_piece('b', &piece_type.to_string())) 
                } else { 
                    None 
                }
            })
            .collect::<Vec<_>>()
            .try_into().unwrap()
        })
        .collect();

        let empty_rows: Vec<[Square; BOARD_DIMENSION]> = iter::repeat({
            iter::repeat(Square::Empty)
                .take(BOARD_DIMENSION).collect::<Vec<_>>()
                .try_into().unwrap()
        })
        .take(4)
        .collect();

        let board: Board = [black_backrow, black_frontrow, empty_rows, white_frontrow, white_backrow]
            .into_par_iter()
            .flatten()
            .collect::<Vec<_>>()
            .try_into().unwrap();
        THREE_REPEATS_RULE.lock().unwrap().push(board);

        // White goes first:
        let turn = Color::White;
        // First move, so:
        let board_state = BoardState::Actual; 

        ChessBoard { board, turn, board_state }
    }

    // Getting input from the user and
    // acting out the move:
    pub fn try_move(&mut self) -> Result<GameStatus, Box<dyn BoardError>> {
        // Taking input:
        let input = take_input(Some("Please input your move: "), true);
        
        if &input == "quit" {
            return Ok(GameStatus::Quit)
        } else if &input == "help" {
            return Ok(GameStatus::Help)
        }

        // Hash table for when translating chess notation:
        let letter_keys: HashMap<&str, usize> = HashMap::from([
            ("a", 1),
            ("b", 2),
            ("c", 3),
            ("d", 4),
            ("e", 5),
            ("f", 6),
            ("g", 7),
            ("h", 8),
        ]);

        // Translating chess notation for the computer:
        let input = input.split(" ").collect::<String>()
            .split("->")
            .filter_map(|square_name| {
                let mut charas = square_name.chars();
                if charas.clone().count() != 2 { return None }

                let index_two= match letter_keys.get({
                    &charas.next().unwrap().to_string().as_ref()
                }) 
                {
                    Some(num) => *num - 1,
                    None => return None, 
                };
                let index_one= match charas.next().unwrap().to_string().parse::<usize>() 
                {
                    Ok(num) if num >= 1 && num <= 8 => 7 - (num - 1),
                    _ => return None,
                };
                let position_arr: [usize; 2] = [index_one, index_two];

                Some(position_arr)

            })
            .collect::<Vec<[usize; 2]>>();

        // Will only take two square values.
        // E.g., unable to take e2->e3->e4, but able
        // for e2->e4.
        if input.len() != 2 {
            return Err(Box::new(MoveError::Syntax))
        }

        self.do_move(input[0], input[1])?;

        // self.end_move() itself returns an enum
        // to tell the state of the game after a turn.
        Ok(self.end_move())
    }

    // Should pieces be put here? Or should I move this over?
    fn do_move(&mut self, position_start: [usize; 2], position_end: [usize; 2]) -> Result<(), Box<dyn BoardError>> {
        let square = &self.board[position_start[0]][position_start[1]];
        // The square has to be a piece for it to be moved.
        let Square::Busy(piece) = square else {
            return Err(Box::new(MoveError::Square))
        };
        // The piece has to have the same color as whose
        // turn the board says it is.
        if piece.color != self.turn { return Err(Box::new(MoveError::Color)) }

        // Appeases the borrow checker for PieceType implements copy.
        let piece_type = piece.piece_type;
        // The result of do_move for PieceType is an enum with fields
        // Move and Take, and are not to be used unless
        // checking for checks. Thus, you can throw it away.
        let _ = piece_type.do_move(self, position_start, position_end, piece.has_moved)?;

        Ok(())
    }

    fn check_checks(&mut self, king_position: Option<[usize; 2]>) -> Result<(), Box<dyn BoardError>> {
        // Switching the states
        let board_state = self.board_state;
        self.to_board_state_test(TestState::WithoutCheck);

        let king_position = {
            if let Some(position) = king_position {
                // Creating a dummy king...
                self.board[position[0]][position[1]] = Square::new_piece(self.turn.to_char(), "k");
                position
            } else {
                // If no dummy king is constructed, we simply
                // find it.
                let &(_, position) = PiecesList::from_board(&self.board).get(self.turn).par_iter().find_any(|(piece, _)| {
                    piece.piece_type == PieceType::King
                })
                .unwrap();

                position
            }
        };

        // The return value is stored here
        let mut out: Result<(), Box<dyn BoardError>> = Ok(());
        if let Err(check_error) = PiecesList::from_board(&self.board).get(self.turn.opposite()).par_iter().try_for_each(|(piece, position)| {
            let mut board = *self;
            board.turn = self.turn.opposite();
            if let Ok(MoveDetails::Take) = piece.piece_type
                .do_move(&mut board, *position, king_position, piece.has_moved) 
            {
                return Err(Box::new(CheckError::error()))
            }

            Ok(())
        })
        {
            // Basically, checks for if you can capture
            // the dummy value or the king. If you can,
            // you throw an error (a check error).
            out = Err(check_error);
        }

        self.board_state = board_state;
        out

    }

    // For the movable.rs file. Basically, we
    // write the piece to the location and
    // if it fails the part where we check
    // for checks (optional), we undo
    // the writing.
    fn write_andor_check(
        &mut self, 
        position_start: [usize; 2], 
        position_end: [usize; 2],
        move_details: MoveDetails,
    ) -> Result<MoveDetails, Box<dyn BoardError>> 
    {
        let our_piece_original = mem::take(&mut self.board[position_start[0]][position_start[1]]);
        let mut our_piece = our_piece_original;
        // Promotion will be handled at the end of the turn
        // when board_state is BoardState::Actual.
        let their_piece = mem::take(&mut self.board[position_end[0]][position_end[1]]);
        our_piece.set_moved();

        self.board[position_end[0]][position_end[1]] = our_piece;

        match self.board_state {
            BoardState::Test { test_state: TestState::WithoutCheck, .. } => (),
            _ => {
                if let Err(check_error) = self.check_checks(None) {
                    self.board[position_start[0]][position_start[1]] = our_piece_original;
                    self.board[position_end[0]][position_end[1]] = their_piece;

                    return Err(check_error)
                }
            }
        }

        if self.board_state == BoardState::Actual {
            match move_details {
                MoveDetails::Move if our_piece.get().piece_type != PieceType::Pawn => {
                    *FIFTY_MOVE_RULE.lock().unwrap() += 1;
                    THREE_REPEATS_RULE.lock().unwrap().push(self.board);
                },
                _ => {
                    *FIFTY_MOVE_RULE.lock().unwrap() = 0;
                    THREE_REPEATS_RULE.lock().unwrap().clear();
                }
            }
        }

        Ok(move_details)
    }

    // Things to do at the end of the move:
    fn end_move(&mut self) -> GameStatus {
        // Cleaning for EnPassant squares:
        self.board.par_iter_mut().for_each(|square| {
            if let Square::EnPassant(num) = square {
                if { *num += 1; *num } == 2 {
                    *square = Square::Empty
                }
            }
        });

        // Checking for if there's stalemate or checkmate:
        let result = self.check_over();
        // Switching the board's color.
        self.turn = self.turn.opposite();

        result
    }

    // This checks for stalemates and checkmates.
    fn check_over(&mut self) -> GameStatus {
        // We want the opposite color to check if they are able to move
        // after our move.
        let opposite_color = self.turn.opposite();
        // The state of the board.
        self.to_board_state_test(TestState::WithCheck);
        
        let (all_moves, count) = PiecesList::from_board(&self.board).get(opposite_color).par_iter()
        // Then collecting all of the results from the test moves
        // of every square on the board for each piece. At most, 
        // it's 16*64, which is only 1024. This program is
        // multi-threaded, which may help slowdowns.
        .fold(|| (Vec::new(), [0; 2]), |(mut collector, mut count), (piece, position_start)| {
            // Counter for the king, bishop, and knights. If it's two,
            // then it's a draw.
            // Move this somewhere else please:
            match (piece.color, piece.piece_type) {
                (Color::White, PieceType::King | PieceType::Bishop | PieceType::Knight) => count[0] += 1,
                (Color::Black, PieceType::King | PieceType::Bishop | PieceType::Knight) => count[1] += 1,
                _ => count.iter_mut().for_each(|x| *x += 2),
            }
                
            // The board is a square, so all indexes are to be found with
            // taking it to the second power.
            for index in 0..(BOARD_DIMENSION.pow(2)) {
                let row = index / BOARD_DIMENSION;
                let column = index % BOARD_DIMENSION;

                let position_end = [row, column];

                let mut board = *self;
                board.turn = opposite_color;

                collector.push(
                    piece.piece_type.do_move(&mut board, *position_start, position_end, piece.has_moved)
                );
            }

            (collector, count)
        })
        // We have to return the result with .reduce(), which is
        // unlike the stdlib iters in Rust.
        .reduce(|| (Vec::new(), [0; 2]), |(mut collector_out, mut count_out), (collector, count)| {
            collector_out.extend(collector);
            count_out.iter_mut().zip(count.into_iter()).for_each(|(x, y)| {
                *x += y;       
            });

            (collector_out, count_out)
        });

        // Returning the BoardState:
        self.to_board_state_actual();

        // We check for if there's stalemate or checkmate:
        let is_game_over = all_moves.par_iter().all(|result| {
            if let Ok(_) = result {
                return false 
            }

            true
        });

        // Now, we check for if it there's any checks:
        let has_check_error = all_moves.into_par_iter().any(|result| {
            if let Err(error_details) = result {
                return error_details.id() == ErrorID::Check
            }

            false
        });

        let mut for_repeats_rule: HashSet<Board> = HashSet::new();
        let repeat_count = THREE_REPEATS_RULE.lock().unwrap().iter().filter(|board| {
            !for_repeats_rule.insert(**board)
        })
        .count() + 1;        

        let impossible_to_win = count.into_iter().all(|x| x <= 2);

        // If it's game over and has a check error, it has
        // to be checkmate. Otherwise if it's game over,
        // it's stalemate. 
        if is_game_over && has_check_error {
            // We return the color of the winner.
            GameStatus::End(self.turn)
        } else if is_game_over || *FIFTY_MOVE_RULE.lock().unwrap() == FIFTY_MOVES || repeat_count == THREE_REPEATS || impossible_to_win {
            // Fifty move rule and three repitition is also considered draw.
            GameStatus::Stalemate
        } else {
            GameStatus::Ongoing
        }
    }
}

// Giving Board a way to be printed:
impl fmt::Display for ChessBoard {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let board_str = format!("_________________________________________________________\n\
                                          |      |      |      |      |      |      |      |      |\
                                          \n{}\n\
                                          |______|______|______|______|______|______|______|______|", 
            // We collect each piece into a string here:
            self.board.par_iter().enumerate().fold(|| String::new(), |collector, (index, square)| {
                let piece_str = {
                    if let Square::Busy(piece) = square {
                        let color = piece.color.to_char().to_string();
                        match piece.piece_type {
                            PieceType::King => color + "K",
                            PieceType::Queen => color + "Q",
                            PieceType::Rook => color + "R",
                            PieceType::Bishop => color + "B",
                            PieceType::Knight => color + "N",
                            PieceType::Pawn => color + "P",
                        }
                    } else {
                        "  ".to_string()
                    }
                };

                String::from( if (index + 1) % BOARD_DIMENSION == 1 { "|  " } else { "" } ) 
                    + &collector 
                    + &piece_str 
                    + {
                        if (index + 1) % BOARD_DIMENSION == 0 && index < 63 {
                            // If it's between rows, we add the below string to write:
                            "  |\n|______|______|______|______|______|______|______|______|\n\
                                  |      |      |      |      |      |      |      |      |\n"
                        } else {
                            // Otherwise, we just add a seperator.
                            "  |  "
                        } 
                    }
            })
            .reduce(|| String::new(), |mut out, collector| { out.push_str(&collector); out }));

        // Writing the string into the stream:
        write!(f, "{}", board_str) 
    }
}

#[derive(Debug, PartialEq)]
pub enum GameStatus {
    End(Color),
    Stalemate,
    Ongoing,
    // Other possible, also likely inputs:
    Quit,
    Help,
}

impl fmt::Display for GameStatus {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let game_status_str = match self {
            GameStatus::End(winner) => {
                format!("{} won!", winner.to_string())
            },
            GameStatus::Stalemate => String::from("The game ended in a draw."),
            GameStatus::Ongoing => String::from("The game is ongoing."),
            GameStatus::Quit => String::from("Quitting the game..."),
            GameStatus::Help => String::from("You can express moves by writing an arrow between two squares.\n\
                                              E.g., \"e2->e4\" for the standard e4 move. Type \"quit\" to exit."),
        };

        write!(f, "{}", game_status_str)
    }
}

fn take_input(msg: Option<&str>, is_to_lowercase: bool) -> String {
    // Asking for input:
    if let Some(msg) = msg {
        print!("{}", msg);
        if let Err(err) = io::Write::flush(&mut io::stdout()) {
            eprintln!("{}", err);
            process::exit(1)
        }
    }

    // Converting input to String: 
    let mut input = String::new();
    if let Err(err) = io::stdin().read_line(&mut input) {
        eprintln!("{}", err);
        process::exit(1)
    }
    input = {
        if is_to_lowercase {
            input.trim().to_lowercase()
        } else {
            input.trim().to_owned()
        }
    };

    input
}