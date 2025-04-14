use std::cmp::Ordering;

use semver::{Comparator, Op, Version, VersionReq};
use tap::Pipe;
use unicase::UniCase;

use crate::{Fetcher, Revision};

use super::Constraint;

/// Fallback comparison when there is no fetcher-specific comparison.
///
/// The default if there are no additional rules specified for the fetcher is:
/// - If both versions are semver, compare according to semver rules.
///   In this instance [`Constraint::Compatible`] is equivalent to a caret rule.
/// - If not, coerce them both to an opaque string and compare according to unicode ordering rules.
///   In this instance [`Constraint::Compatible`] is a case-insensitive equality comparison.
pub fn compare(constraint: &Constraint, _: Fetcher, rev: &Revision) -> bool {
    if let (Revision::Semver(c), Revision::Semver(r)) = (constraint.revision(), rev) {
        return version_req_for(constraint, c).matches(r);
    }

    match constraint {
        Constraint::Compatible(c) => UniCase::new(c.as_str()) == UniCase::new(rev.as_str()),
        _ => lexically_compare(constraint, rev),
    }
}

fn version_req_for(constraint: &Constraint, v: &Version) -> VersionReq {
    match constraint {
        Constraint::Compatible(_) => vec![Op::Tilde],
        Constraint::Equal(_) => vec![Op::Exact],
        Constraint::NotEqual(_) => vec![Op::Less, Op::Greater],
        Constraint::Less(_) => vec![Op::Less],
        Constraint::LessOrEqual(_) => vec![Op::LessEq],
        Constraint::Greater(_) => vec![Op::Greater],
        Constraint::GreaterOrEqual(_) => vec![Op::GreaterEq],
    }
    .into_iter()
    .map(|op| comparator(op, v))
    .collect::<Vec<_>>()
    .pipe(|comparators| VersionReq { comparators })
}

fn comparator(op: Op, v: &Version) -> Comparator {
    Comparator {
        op,
        major: v.major,
        minor: Some(v.minor),
        patch: Some(v.patch),
        pre: v.pre.clone(),
    }
}

fn ords_for(constraint: &Constraint) -> Vec<Ordering> {
    match constraint {
        Constraint::Compatible(_) => vec![Ordering::Equal],
        Constraint::Equal(_) => vec![Ordering::Equal],
        Constraint::NotEqual(_) => vec![Ordering::Less, Ordering::Greater],
        Constraint::Less(_) => vec![Ordering::Less],
        Constraint::LessOrEqual(_) => vec![Ordering::Less, Ordering::Equal],
        Constraint::Greater(_) => vec![Ordering::Greater],
        Constraint::GreaterOrEqual(_) => vec![Ordering::Greater, Ordering::Equal],
    }
}

fn lexically_compare(constraint: &Constraint, other: impl ToString) -> bool {
    let target = constraint.revision().to_string();
    let other = other.to_string();
    for ord in ords_for(constraint) {
        if lexical_sort::lexical_cmp(&target, &other) == ord {
            return true;
        }
    }
    return false;
}
