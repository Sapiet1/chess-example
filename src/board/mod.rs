// This file creates a board type and implements a 
// multi-threaded iterator for it. We defined several iterators,
// one for taking the values itself (into_par_iter),
// a reference to the values (par_iter),
// and a mutable reference to the values (par_iter_mut).

use std::ops::{Index, IndexMut};
use rayon::prelude::*;
use rayon::iter::plumbing;

pub mod square;
pub mod movable;

use square::Square;

pub const BOARD_DIMENSION: usize = 8;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Board {
    board: [[Square; BOARD_DIMENSION]; BOARD_DIMENSION],
}

impl Index<usize> for Board {
    type Output = [Square; BOARD_DIMENSION];

    fn index(&self, index: usize) -> &Self::Output {
        &self.board[index]
    }
}

impl IndexMut<usize> for Board {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.board[index]
    }
}

impl<T> From<T> for Board
where T: TryInto<[[Square; BOARD_DIMENSION]; BOARD_DIMENSION]>,
      <T as TryInto<[[Square; 8]; 8]>>::Error: std::fmt::Debug,
{
    fn from(value: T) -> Self {
        let board: [[Square; BOARD_DIMENSION]; BOARD_DIMENSION] = value.try_into().unwrap();

        Board { board }
    }
}

// The pattern is copied after the first.
// This is where the iterator that Board uses is defined:
pub struct BoardIter {
    board: Vec<Square>,
}
// The iterator struct is a ParallelIterator, as Rayon calls it:
impl ParallelIterator for BoardIter {
    type Item = Square;

    fn drive_unindexed<C>(self, consumer: C) -> C::Result
        where
            C: plumbing::UnindexedConsumer<Self::Item> 
    {
        plumbing::bridge(self, consumer)
    }

    fn opt_len(&self) -> Option<usize> {
        Some(self.len())
    }
}
// We need IndexedParallelIterator for plumbing::bridge on self:
impl IndexedParallelIterator for BoardIter {
    fn with_producer<CB: plumbing::ProducerCallback<Self::Item>>(self, callback: CB) -> CB::Output {
        callback.callback(self)
    }

    fn drive<C: plumbing::Consumer<Self::Item>>(self, consumer: C) -> C::Result {
        plumbing::bridge(self, consumer)
    }

    fn len(&self) -> usize {
        self.board.len()
    }
}
// We need a producer of the squares. BoardIter
// is the producer itself:
impl plumbing::Producer for BoardIter {
    type Item = Square;
    type IntoIter = <Vec<Square> as IntoIterator>::IntoIter; 

    fn into_iter(self) -> Self::IntoIter {
        self.board.into_iter()
    }

    fn split_at(mut self, index: usize) -> (Self, Self) {
        let right = self.board.split_off(index);
        let left = self.board;

        ( 
            BoardIter { board: left },
            BoardIter { board: right },
        )
    }
}
// Now, we can turn Board into a ParallelIterator
// with defining this trait with the structs we created:
impl IntoParallelIterator for Board {
    type Iter = BoardIter;
    type Item = Square;

    fn into_par_iter(self) -> Self::Iter {
        let board = self.board.into_iter().flatten().collect::<Vec<_>>();

        BoardIter { board } 
    }
}

pub struct BoardRefIter<'a> {
    board: Vec<&'a Square>
}
impl<'a> ParallelIterator for BoardRefIter<'a> {
    type Item = &'a Square;

    fn drive_unindexed<C>(self, consumer: C) -> C::Result
        where
            C: plumbing::UnindexedConsumer<Self::Item> 
    {
        plumbing::bridge(self, consumer)
    }

    fn opt_len(&self) -> Option<usize> {
        Some(self.len())
    }
}
impl<'a> IndexedParallelIterator for BoardRefIter<'a> {
    fn with_producer<CB: plumbing::ProducerCallback<Self::Item>>(self, callback: CB) -> CB::Output {
        callback.callback(self)
    }

    fn drive<C: plumbing::Consumer<Self::Item>>(self, consumer: C) -> C::Result {
        plumbing::bridge(self, consumer)
    }

    fn len(&self) -> usize {
        self.board.len()
    }
}
impl<'a> plumbing::Producer for BoardRefIter<'a> {
    type Item = &'a Square;
    type IntoIter = <Vec<&'a Square> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.board.into_iter()
    }

    fn split_at(mut self, index: usize) -> (Self, Self) {
        let right = self.board.split_off(index);
        let left = self.board;

        ( 
            BoardRefIter { board: left },
            BoardRefIter { board: right },
        )
    }
}
impl<'a> IntoParallelIterator for &'a Board {
    type Iter = BoardRefIter<'a>;
    type Item = &'a Square;

    fn into_par_iter(self) -> Self::Iter {
        let board = self.board.iter().flatten().collect::<Vec<_>>();

        BoardRefIter { board }
    }
}

pub struct BoardRefMutIter<'a> {
    board: Vec<&'a mut Square>,
}
impl<'a> ParallelIterator for BoardRefMutIter<'a> {
    type Item = &'a mut Square;

    fn drive_unindexed<C>(self, consumer: C) -> C::Result
        where
            C: plumbing::UnindexedConsumer<Self::Item> 
    {
        plumbing::bridge(self, consumer)
    }

    fn opt_len(&self) -> Option<usize> {
        Some(self.len())
    }
}
impl<'a> IndexedParallelIterator for BoardRefMutIter<'a> {
    fn with_producer<CB: plumbing::ProducerCallback<Self::Item>>(self, callback: CB) -> CB::Output {
        callback.callback(self)
    }

    fn drive<C: plumbing::Consumer<Self::Item>>(self, consumer: C) -> C::Result {
        plumbing::bridge(self, consumer)
    }

    fn len(&self) -> usize {
        self.board.len()
    }
}
impl<'a> plumbing::Producer for BoardRefMutIter<'a> {
    type Item = &'a mut Square;
    type IntoIter = <Vec<&'a mut Square> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.board.into_iter()
    }

    fn split_at(mut self, index: usize) -> (Self, Self) {
        let right = self.board.split_off(index);
        let left = self.board;

        ( 
            BoardRefMutIter { board: left },
            BoardRefMutIter { board: right },
        )
    }
}
impl<'a> IntoParallelIterator for &'a mut Board {
    type Iter = BoardRefMutIter<'a>;
    type Item = &'a mut Square;

    fn into_par_iter(self) -> Self::Iter {
        let board = self.board.iter_mut().flatten().collect::<Vec<_>>();

        BoardRefMutIter { board }
    } 
}
