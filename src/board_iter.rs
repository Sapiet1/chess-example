// This file creates a multi-threaded iterator
// for Board. We defined several iterators,
// one for taking the values itself (into_par_iter),
// a reference to the values (par_iter),
// and a mutable reference to the values (par_iter_mut).

use rayon::prelude::*;
use rayon::iter::plumbing;

use crate::{Square, Board};

// The pattern is copied after the first.
// This is where the iterator that Board uses is defined:
pub struct BoardIter {
    map: Vec<Square>,
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
        self.map.len()
    }
}
// We need a producer of the squares. BoardIter
// is the producer itself:
impl plumbing::Producer for BoardIter {
    type Item = Square;
    type IntoIter = <Vec<Square> as IntoIterator>::IntoIter; 

    fn into_iter(self) -> Self::IntoIter {
        self.map.into_iter()
    }

    fn split_at(mut self, index: usize) -> (Self, Self) {
        let right = self.map.split_off(index);
        let left = self.map;

        ( 
            BoardIter { map: left },
            BoardIter { map: right },
        )
    }
}
// Now, we can turn Board into a ParallelIterator
// with defining this trait with the structs we created:
impl IntoParallelIterator for Board {
    type Iter = BoardIter;
    type Item = Square;

    fn into_par_iter(self) -> Self::Iter {
        let map = self.map.into_iter().flatten().collect::<Vec<_>>();

        BoardIter { map } 
    }
}

pub struct BoardRefIter<'a> {
    map: Vec<&'a Square>
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
        self.map.len()
    }
}
impl<'a> plumbing::Producer for BoardRefIter<'a> {
    type Item = &'a Square;
    type IntoIter = <Vec<&'a Square> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.map.into_iter()
    }

    fn split_at(mut self, index: usize) -> (Self, Self) {
        let right = self.map.split_off(index);
        let left = self.map;

        ( 
            BoardRefIter { map: left },
            BoardRefIter { map: right },
        )
    }
}
impl<'a> IntoParallelIterator for &'a Board {
    type Iter = BoardRefIter<'a>;
    type Item = &'a Square;

    fn into_par_iter(self) -> Self::Iter {
        let map = self.map.iter().flatten().collect::<Vec<_>>();

        BoardRefIter { map }
    }
}

pub struct BoardRefMutIter<'a> {
    map: Vec<&'a mut Square>,
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
        self.map.len()
    }
}
impl<'a> plumbing::Producer for BoardRefMutIter<'a> {
    type Item = &'a mut Square;
    type IntoIter = <Vec<&'a mut Square> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.map.into_iter()
    }

    fn split_at(mut self, index: usize) -> (Self, Self) {
        let right = self.map.split_off(index);
        let left = self.map;

        ( 
            BoardRefMutIter { map: left },
            BoardRefMutIter { map: right },
        )
    }
}
impl<'a> IntoParallelIterator for &'a mut Board {
    type Iter = BoardRefMutIter<'a>;
    type Item = &'a mut Square;

    fn into_par_iter(self) -> Self::Iter {
        let map = self.map.iter_mut().flatten().collect::<Vec<_>>();

        BoardRefMutIter { map }
    } 
}
