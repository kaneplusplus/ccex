% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ccex.r
\name{getmarkethistory}
\alias{getmarkethistory}
\title{Recent History for a Market}
\usage{
getmarkethistory(market, count = 50)
}
\arguments{
\item{market}{the market from which history data will be retrieved.}

\item{count}{the number of recent trades to retrieve (default is 50, max
is 100).}
}
\value{
A named list with the following elements:
\itemize{
 \item{success: }{a boolean indicating if the request successful?}
 \item{message: }{a string describing the error if the request was not 
                  successful, otherwise and empty string."}
 \item{result:  }{A code{data.frame} containing recent trade information
   including the order type, time, quantity, price, and fill type.}
}
}
\description{
the \code{getmarkethistory} function retrieves recent trade
information for a specified market on the C-Cex crypto-currency exchange 
\url{https://c-cex.com}.
}
\examples{
\dontrun{
mh = getmarkethistory("usd-btc")$result
head(mh)
}
}
\references{
\url{https://c-cex.com/?id=api#getmarkethistory}
}
