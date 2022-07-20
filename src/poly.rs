use std::cmp::Ordering;
use std::fmt;
use std::ops;

use crate::utils::OrderedPair;
use crate::utils::ordered_merge;


pub trait TermOrdering {
    fn term_cmp(one: &MTerm, other: &MTerm) -> Ordering;
}

#[derive(Copy, Clone, PartialEq, Eq, Ord, PartialOrd, Hash)]
pub struct VarIdx(pub u16);

impl fmt::Debug for VarIdx {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "x_{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MPoly<TCoeff, TOrder=DegRevLexOrder> {
    monoms: Vec<(MTerm, TCoeff)>,
    _order: std::marker::PhantomData<TOrder>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MTerm {
    pub(crate) vars: Vec<(VarIdx, u16)>,
}

impl MTerm {
    pub fn new(mut vars: Vec<(VarIdx, u16)>) -> Self {
        vars.sort_by_key(|(v, _)| v.0);
        Self{ vars }
    }

    pub fn len(&self) -> usize {
        self.vars.len()
    }

    pub fn total_power(&self) -> u32 {
        self.vars.iter().map(|x| x.1 as u32).sum()
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct LexOrder;

impl TermOrdering for LexOrder {
    fn term_cmp(one: &MTerm, other: &MTerm) -> Ordering {
        let change = one.vars.iter().zip(&other.vars)
            .find(|(l, r)| l != r);
        
        if let Some((left, right)) = change {
            left.cmp(right)
        } else {
            one.vars.len().cmp(&other.vars.len())
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct RevLexOrder;

impl TermOrdering for RevLexOrder {
    fn term_cmp(one: &MTerm, other: &MTerm) -> Ordering {
        let change = one.vars.iter().zip(&other.vars)
            .rev()
            .find(|(l, r)| l != r);
        
        if let Some((left, right)) = change {
            left.cmp(right).reverse()
        } else {
            one.vars.len().cmp(&other.vars.len()).reverse()
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct DegRevLexOrder;

impl TermOrdering for DegRevLexOrder {
    fn term_cmp(one: &MTerm, other: &MTerm) -> Ordering {
        one.total_power()
            .cmp(&other.total_power())
            .then_with(|| RevLexOrder::term_cmp(one, other))
    }
}

impl<TCoeff, TOrder> Default for MPoly<TCoeff, TOrder> {
    fn default() -> Self {
        Self::zero()
    }
}

impl<TCoeff, TOrder> MPoly<TCoeff, TOrder> {
    pub fn zero() -> Self {
        Self {
            monoms: Vec::new(),
            _order: Default::default(),
        }
    }
}

impl<TCoeff, TOrder: TermOrdering> MPoly<TCoeff, TOrder> {
    pub fn new(mut monoms: Vec<(MTerm, TCoeff)>) -> Self {
        monoms.sort_by(|l, r| TOrder::term_cmp(&l.0, &r.0).reverse());
        Self{ monoms, _order: Default::default() }
    }
}

impl<'a, TCoeff, TOrder> ops::Add<&'a MPoly<TCoeff, TOrder>> for &'a MPoly<TCoeff, TOrder>
where
    TCoeff: Clone,
    &'a TCoeff: ops::Add<&'a TCoeff, Output=TCoeff>,
    TOrder: TermOrdering,
{
    type Output = MPoly<TCoeff, TOrder>;

    fn add(self, rhs: &'a MPoly<TCoeff, TOrder>) -> Self::Output {
        let mut monoms = Vec::with_capacity(self.monoms.len() + rhs.monoms.len());
        ordered_merge(
            &self.monoms, &rhs.monoms,
            |l, r| TOrder::term_cmp(&l.0, &r.0).reverse(),
            |monom| {
                match monom {
                    OrderedPair::Left(pair) => { monoms.push(pair.clone()); }
                    OrderedPair::Right(pair) => { monoms.push(pair.clone()); }
                    OrderedPair::Both((lterm, lc), (_, rc)) => {
                        monoms.push((lterm.clone(), lc + rc));
                    }
                }
            },
        );
        MPoly { monoms, _order: Default::default() }
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_addition() {
        // 2x + 3y
        let left: MPoly<i32, DegRevLexOrder> = MPoly::new(vec![
            (MTerm{vars: vec![(VarIdx(0), 1), ]}, 2),
            (MTerm{vars: vec![(VarIdx(1), 1), ]}, 3),
        ]);
        // x**2 + xy + x - y
        let right: MPoly<i32, DegRevLexOrder> = MPoly::new(vec![
            (MTerm{vars: vec![(VarIdx(0), 2), ]}, 1),
            (MTerm{vars: vec![(VarIdx(1), 1), (VarIdx(0), 1)]}, 1),
            (MTerm{vars: vec![(VarIdx(0), 1)]}, 1),
            (MTerm{vars: vec![(VarIdx(1), 1)]}, -1),
        ]);
        // x**2 + xy + 3x + 2y
        let expected: MPoly<i32, DegRevLexOrder> = MPoly::new(vec![
            (MTerm{vars: vec![(VarIdx(0), 2), ]}, 1),
            (MTerm{vars: vec![(VarIdx(1), 1), (VarIdx(0), 1)]}, 1),
            (MTerm{vars: vec![(VarIdx(0), 1)]}, 3),
            (MTerm{vars: vec![(VarIdx(1), 1)]}, 2),
        ]);

        let result = &left + &right;
        assert_eq!(result, expected);
    }

    #[test]
    fn test_lex_ordering() {
        let one = MTerm{vars: vec![
            (VarIdx(0), 2),
            (VarIdx(1), 1),
        ]};
        let other = MTerm{vars: vec![
            (VarIdx(1), 1),
            (VarIdx(2), 2),
        ]};
        assert_eq!(LexOrder::term_cmp(&one, &other), Ordering::Less);
        assert_eq!(LexOrder::term_cmp(&other, &one), Ordering::Greater);

        let one = MTerm{vars: vec![
            (VarIdx(0), 1),
        ]};
        let other = MTerm{vars: vec![
            (VarIdx(0), 2),
        ]};
        assert_eq!(LexOrder::term_cmp(&one, &other), Ordering::Less);
        assert_eq!(LexOrder::term_cmp(&other, &one), Ordering::Greater);

        let one = MTerm{vars: vec![
            (VarIdx(0), 1),
        ]};
        let other = MTerm{vars: vec![
            (VarIdx(0), 1),
        ]};
        assert_eq!(LexOrder::term_cmp(&one, &other), Ordering::Equal);
        assert_eq!(LexOrder::term_cmp(&other, &one), Ordering::Equal);
    }

    #[test]
    fn test_degrevlex_ordering() {
        let one = MTerm{vars: vec![
            (VarIdx(0), 2),
            (VarIdx(1), 1),
        ]};
        let other = MTerm{vars: vec![
            (VarIdx(1), 1),
            (VarIdx(2), 2),
        ]};
        assert_eq!(DegRevLexOrder::term_cmp(&one, &other), Ordering::Greater);
        assert_eq!(DegRevLexOrder::term_cmp(&other, &one), Ordering::Less);

        let one = MTerm{vars: vec![
            (VarIdx(0), 1),
        ]};
        let other = MTerm{vars: vec![
            (VarIdx(0), 2),
        ]};
        assert_eq!(DegRevLexOrder::term_cmp(&one, &other), Ordering::Less);
        assert_eq!(DegRevLexOrder::term_cmp(&other, &one), Ordering::Greater);

        let one = MTerm{vars: vec![
            (VarIdx(0), 1),
        ]};
        let other = MTerm{vars: vec![
            (VarIdx(0), 1),
        ]};
        assert_eq!(DegRevLexOrder::term_cmp(&one, &other), Ordering::Equal);
        assert_eq!(DegRevLexOrder::term_cmp(&other, &one), Ordering::Equal);
    }
}
