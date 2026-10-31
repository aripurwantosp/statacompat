# Build non-ASCII test strings from bytes so the tests do not depend on the
# locale in which the test files are parsed.
utf8_string <- function(...) {
  s <- rawToChar(as.raw(c(...)))
  Encoding(s) <- "UTF-8"
  s
}
e_acute <- function() utf8_string(0xc3, 0xa9)          # U+00E9
a_macron <- function() utf8_string(0xc4, 0x80)         # U+0100

tagged <- function(tag) {
  skip_if_not_installed("haven")
  haven::tagged_na(tag)
}
