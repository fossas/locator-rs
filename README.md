# locator
[![FOSSA Status](https://app.fossa.com/api/projects/git%2Bgithub.com%2Ffossas%2Flocator-rs.svg?type=shield)](https://app.fossa.com/projects/git%2Bgithub.com%2Ffossas%2Flocator-rs?ref=badge_shield)


This library provides the ability to parse and format "Locator" strings.
FOSSA uses locators to indicate specific libraries at specific versions.

For more detail, FOSSA employees can reference the
[Fetchers & Locators doc](https://go/fetchers-doc).

# Format

Locators are in the following basic format:

```not_rust
{fetcher}+{package}${version}
```

There is some nuance to this. For more details, see the library documentation.

## Example

Some example locators:

```not_rust
// The FOSSA CLI on GitHub, referencing the tag 'v3.8.24'.
git+github.com/fossas/fossa-cli$v3.8.24

// The 'lodash' library on NPM, referencing the version '4.17.21'.
npm+lodash$4.17.21

// The 'lodash' library on NPM without specifying a version.
npm+lodash
```


## License
[![FOSSA Status](https://app.fossa.com/api/projects/git%2Bgithub.com%2Ffossas%2Flocator-rs.svg?type=large)](https://app.fossa.com/projects/git%2Bgithub.com%2Ffossas%2Flocator-rs?ref=badge_large)