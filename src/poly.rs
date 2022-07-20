use std::cmp::Ordering;
use std::fmt;
use std::ops;


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

#[derive(Debug, Clone)]
pub struct MPoly<TCoeff, TOrder> {
    monoms: Vec<(MTerm, TCoeff)>,
    _order: std::marker::PhantomData<TOrder>,
}

#[derive(Debug, Clone)]
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


#[cfg(test)]
mod tests {
    use super::*;

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
