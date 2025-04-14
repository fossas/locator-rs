use crate::{Fetcher, Revision};

use super::Constraint;

/// Fallback comparison when there is no fetcher-specific comparison
pub fn compare(constraint: &Constraint, fetcher: Fetcher, rev: &Revision) -> bool {
    todo!()
}
