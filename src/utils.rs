use std::cmp::Ordering;

pub(crate) enum OrderedPair<T> {
    Left(T),
    Right(T),
    Both(T, T),
}

#[inline]
pub(crate) fn ordered_merge<T>(
    left: impl IntoIterator<Item = T>,
    right: impl IntoIterator<Item = T>,
    compare: impl Fn(&T, &T) -> Ordering,
    mut action: impl FnMut(OrderedPair<T>),
) {
    let mut left = left.into_iter();
    let mut right = right.into_iter();
    let mut left_item = left.next();
    let mut right_item = right.next();
    while let (Some(l), Some(r)) = (&left_item, &right_item) {
        match compare(&l, &r) {
            Ordering::Less => {
                let l = left_item.take().unwrap();
                left_item = left.next();
                action(OrderedPair::Left(l));
            }
            Ordering::Greater => {
                let r = right_item.take().unwrap();
                right_item = right.next();
                action(OrderedPair::Right(r));
            }
            Ordering::Equal => {
                let l = left_item.take().unwrap();
                let r = right_item.take().unwrap();
                left_item = left.next();
                right_item = right.next();
                action(OrderedPair::Both(l, r))
            }
        }
    }
    for l in left_item.into_iter().chain(left) {
        action(OrderedPair::Left(l));
    }
    for r in right_item.into_iter().chain(right) {
        action(OrderedPair::Right(r));
    }
}
