% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/str_normalize_spacing.R
\name{str_unify_spacing}
\alias{str_unify_spacing}
\title{Whitespace normalization}
\usage{
str_unify_spacing(.str, .tok_lock_regex = NULL)
}
\arguments{
\item{.str}{Character vector to be normalized}

\item{.tok_lock_regex}{...}
}
\value{
\code{str_unify_spacing} - Returns the normalized character vector
}
\description{
\code{str_unify_spacing} - Normalizes whitespace by replacing everything
between words and punctuation characters with single space characters. The
identification of boundaries is performed using ICU Breakiterators with added
exceptions for #hashtags, @screen_names, URLs and <KLARTAGS> (as created by
other functions of this package)
}
\examples{
## str_unify_spacing EXAMPLE:

str_unify_spacing(c(
  "This  @screen_name that\n #hash_tag, #1",
  "<not-A_KLARTAG> <A_KLARTAG>!?!? An URL",
  "www.example.com/test ..."
))
}
\references{
https://www.unicode.org/reports/tr29/#Word_Boundaries
}
