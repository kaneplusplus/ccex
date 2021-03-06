% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ccex.r
\name{getmarketsummaries}
\alias{getmarketsummaries}
\title{Summary of All Active Markets}
\usage{
getmarketsummaries()
}
\value{
A named list with the following elements:
\itemize{
 \item{success: }{a boolean indicating if the request successful?}
 \item{message: }{a string describing the error if the request was not 
                  successful, otherwise and empty string."}
 \item{result:  }{A \code{data.frame} summarizing each active market's 
 trading activity for the last 24 hours.
 }
}
}
\description{
the \code{getmarketsummaries} retrieves a summary of all
active markets for the last 24 hours on the C-Cex crypto-currency exchange 
\url{https://c-cex.com}.
}
\examples{
\dontrun{
ms = getmarketsummaries()$result
head(ms)
}
}
\references{
\url{https://c-cex.com/?id=api#getmarketsummaries}
}
