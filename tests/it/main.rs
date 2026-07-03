mod constraint;

/// The JSON name of every fetcher must match its locator-string name.
/// Consumers treat these as interchangeable (e.g. the `mvn` fetcher must not
/// serialize as `maven`); `#[serde(alias = ...)]` keeps old spellings parseable.
#[test]
fn fetcher_serde_names_match_locator_names() {
    use strum::IntoEnumIterator;
    for fetcher in locator::Fetcher::iter() {
        let json = serde_json::to_value(fetcher).expect("serialize fetcher");
        let name = json.as_str().expect("fetcher serializes to a string");
        assert_eq!(
            name,
            fetcher.to_string(),
            "serde/strum name mismatch for {fetcher:?}"
        );
    }
}
