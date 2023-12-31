% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/journal.R
\name{journalEntry}
\alias{journalEntry}
\title{Journal Entry}
\usage{
journalEntry(sym, theme, text)
}
\arguments{
\item{sym}{to identify the ticker}

\item{theme}{to know what type of entry (e.g. "trade")}

\item{text}{contents - it may include \\n for new lines returns}
}
\description{
This function provides the ability to enter one new log individual entry into the journal
}
\details{
Text contents may include special characters, but then must be quoted using \\ character
Also the record date is today - this may differ from the actual trade date.
The idea here is that the comment in the journal reflects the thinking and feeling when it is written
and therefore date should be correct.
}
\examples{
journalEntry("TEST","info","This is great stock!\nI must absolutely get in :-)")
}
