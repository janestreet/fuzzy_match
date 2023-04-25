## Release v0.16.0

- Added the `fuzzy_search`, which not only matches on a pattern string, but
  also produces a score indicating how good of a match was found. This is in
  contrast to the existing `fuzzy_match` library, which merely provides a
  boolean response indicating whether the match was valid or not.
