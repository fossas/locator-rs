# locator

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
