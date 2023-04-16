use std::{thread, time::Duration};

use chess_example::{ChessBoard, ErrorID, MoveError, GameStatus};

fn main() {
    let mut chessboard = ChessBoard::new();

    loop {
        println!("{}", chessboard);
        let result = chessboard.try_move().unwrap_or_else(|err| {
            if let ErrorID::Move(MoveError::Syntax) = err.id() {
                println!("Type \"help\" for the input syntax and \"quit\" to exit.");
            } else {
                println!("Move can't be played!");
            }

            thread::sleep(Duration::from_secs(2));
            GameStatus::Ongoing
        });

        match result {
            GameStatus::Help => {
                println!("{}", result);
                thread::sleep(Duration::from_secs(5));
            },
            GameStatus::Quit => break,
            GameStatus::End(_) | GameStatus::Stalemate => {
                println!("{}\n{}", chessboard, result);
                break;
            },
            _ => (),
        }
    }
}
