use color_eyre::{Result, eyre::Context};
use locator::{Locator, PackageLocator, StrictLocator};
use pretty_assertions::assert_eq;

#[test]
fn error_wrappable_context() -> Result<()> {
    const INPUT: &str = "npm+lodash$1.0.0";

    let parsed = StrictLocator::parse(INPUT).context("can wrap")?;
    assert_eq!(parsed, locator::strict!(Npm, "lodash", "1.0.0"));

    let parsed = Locator::parse(INPUT).context("can wrap")?;
    assert_eq!(parsed, locator::locator!(Npm, "lodash", "1.0.0"));

    let parsed = PackageLocator::parse(INPUT).context("can wrap")?;
    assert_eq!(parsed, locator::package!(Npm, "lodash"));

    Ok(())
}

#[test]
fn error_wrappable_withcontext() -> Result<()> {
    const INPUT: &str = "npm+lodash$1.0.0";

    let parsed = StrictLocator::parse(INPUT).with_context(|| "can wrap")?;
    assert_eq!(parsed, locator::strict!(Npm, "lodash", "1.0.0"));

    let parsed = Locator::parse(INPUT).context("can wrap")?;
    assert_eq!(parsed, locator::locator!(Npm, "lodash", "1.0.0"));

    let parsed = PackageLocator::parse(INPUT).context("can wrap")?;
    assert_eq!(parsed, locator::package!(Npm, "lodash"));

    Ok(())
}
