% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ccex.r
\name{buylimit}
\alias{buylimit}
\title{Place a Buy Limit Order}
\usage{
buylimit(market, quantity, rate)
}
\arguments{
\item{market}{the market to place the buy limit order on.}

\item{quantity}{the quantity of the transaction currency to buy.}

\item{rate}{the price you are willing to pay per unit of the 
transaction currency.}
}
\value{
A named list with the following elements:
\itemize{
 \item{success: }{a boolean indicating if the request successful?}
 \item{message: }{a string describing the error if the request was not 
                  successful, otherwise and empty string."}
 \item{result:  }{a named list, called "uuid" whose element is an integer
   identifying the order. This value is used to query the status of
   of the order with either the \code{link{getorder}} or 
   \code{link{getopenorders}} function. When the order is fulfilled it
   appears in the order history \code{data.frame} returned by the
   \code{link{getorderhistory}} function.
 }
}
}
\description{
The \code{buylimit} function places a buy order onto the 
C-Cex crypto-currency exchange \url{https://c-cex.com}. This function
only works after you have set up authentication.
}
\examples{
\dontrun{
# Buy one bitcoin for 1169 dollars.
order = buylimit("btc-usd", 1, 1169)
}
}
\references{
\url{https://c-cex.com/?id=api#buylimit}
}
\seealso{
\code{link{ccex_authenticate}} \code{\link{selllimit}}
\code{\link{getorder}} \code{\link{getopenorders}} 
\code{\link{getorderhistory}}
}
