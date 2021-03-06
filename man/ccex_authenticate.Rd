% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ccex.r
\name{ccex_authenticate}
\alias{ccex_authenticate}
\title{Provide User Authentication Data}
\usage{
ccex_authenticate(api_key, secret_key)
}
\arguments{
\item{api_key}{the api key provided by the exchange}

\item{secret_key}{the secret key provided by the exchange}
}
\description{
The \code{ccex_authenicate} function sets the 
CCEX_API_KEY and CCEX_SECRET_KEY environment variables in your current
session to access your account information on the C-Cex crypto-currency
exchange (\url{https://c-cex.com}).
}
\references{
\url{https://c-cex.com/?id=api#authentication}
}
