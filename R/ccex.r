library(httr)
library(rjson)
library(xml2)
library(openssl)

#' @title The List of Available Trading Pairs
#' @description The \code{pairs} function returns all currency-pairs 
#' currently available on the C-Cex crypto-currency exchange
#' \url{https://c-cex.com}.
#' @references \url{https://c-cex.com/t/prices.json}
#' @return A named list with the following elements:
#' \itemize{
#'  \item{success: }{a boolean indicating if the the request successful?}
#'  \item{message: }{a string describing the error if the request was not 
#'                   successful, othewise and empty string."}
#'  \item{result:  }{a character vector of the available markets.}
#' }
#' @details The \code{pairs} function returns a named list. The \code{result} 
#' element is a vector of names for all markets on the exchange. The
#' format for a market is "currency1-currency2" 
#' where currency2 is the reference currency. 
#' 
#' As an example, if you want to buy U.S. dollars with bitcoins you would 
#' create a "buy" order for the market denoted "usd-btc".
#' @examples
#' \dontrun{
#' markets = pairs()$result
#' print(markets)
#' }
#' @importFrom httr GET content
#' @export
pairs = function() {
  list(success=TRUE, message="", 
    result=as.vector(unlist(content(GET("https://c-cex.com/t/pairs.json")))))
}

#' @title Retrieve All Trading Pairs Data
#' @description The \code{prices} function returns all trading pairs data
#' currently available on the C-Cex crypto-currency exchange
#' \url{https://c-cex.com}.
#' @references \url{https://c-cex.com/t/prices.json}
#' @return A named list with the with the following elements:
#' \itemize{
#'  \item{success: }{a boolean indicating if the the request successful?}
#'  \item{message: }{a string describing the error if the request was not 
#'                   successful, othewise and empty string."}
#'  \item{result: }{a \code{data.frame} with sell and buy data for each market.}
#' }
#' @examples
#' \dontrun{
#' all_prices = prices()$result
#' head(all_prices)
#' }
#' @importFrom httr GET content
#' @export
prices = function() {
  resp = content(GET("https://c-cex.com/t/prices.json"), 
    type="application/json")
  ret = Reduce(rbind, Map(as.data.frame, resp))
  ret$market = names(resp)
  list(success=TRUE, message="", result=ret)
}

#' @title Retrieve Full Coin Names for All Currencies
#' @description The \code{coinnames} function returns the full coin names
#' for all available currencies on the C-Cex crypto-currency exchange
#' \url{https://c-cex.com}.
#' @references \url{https://c-cex.com/t/coinnames.json}
#' @return A named list with the with the following elements:
#' \itemize{
#'  \item{success: }{a boolean indicating if the the request successful?}
#'  \item{message: }{a string describing the error if the request was not 
#'                   successful, othewise and empty string."}
#'  \item{result: }{a \code{data.frame} with the ticker and corresponding full
#'                  name of the currency.}
#' }
#' @examples
#' \dontrun{
#' cns = coinnames()$result
#' head(cns)
#' }
#' @importFrom httr GET content
#' @export
coinnames = function() {
  resp = unlist(content(GET("https://c-cex.com/t/coinnames.json")))
  list(success=TRUE, message="", 
    result=data.frame(list(ticker=names(resp), name=as.vector(resp))))
}

volume = function(ticker) {
  req = gsub("TICKER", ticker, "https://c-cex.com/t/volume_TICKER.json")
  resp = content(GET(req))
  ti = Reduce(rbind, Map(as.data.frame, resp$ticker))
  ti$ticker = names(resp$ticker)
  list(success=TRUE, message="", result=ti)
}

getmarkets = function() {
  resp = content(GET("https://c-cex.com/t/api_pub.html?a=getmarkets"), 
    type="application/json")
  list(success=TRUE, message="", 
    result=Reduce(rbind, Map(as.data.frame, resp$result)))
}

getorderbook = function(market, type=c("both", "buy", "sell"), depth=50) {
  req="https://c-cex.com/t/api_pub.html?a=getorderbook&market=MARKET&type=TYPE&depth=DEPTH"
  req = gsub("DEPTH", depth, gsub("TYPE", type[1], gsub("MARKET", market, req)))
  resp = content(GET(req), type="application/json")
  if (any(c("both", "buy") %in% names(resp$result)))
    buy = Reduce(rbind, Map(as.data.frame, resp$result$buy))
  if (any(c("both", "sell") %in% names(resp$result)))
    sell= Reduce(rbind, Map(as.data.frame, resp$result$sell))
  if (any(c("both", "buy") %in% names(resp$result)))
    resp$result$buy = buy
  if (any(c("both", "sell") %in% names(resp$result)))
    resp$result$sell = sell
  resp
}

getmarketsummaries = function() {
  resp = content(GET("https://c-cex.com/t/api_pub.html?a=getmarketsummaries"),
    type="application/json")
  result = resp$result
  for(i in 1:length(result)) {
    for (j in 1:length(result[[i]]))
      if (is.null(result[[i]][[j]]))
        result[[i]][[j]] = NA
  }
  resp$result = Reduce(rbind, Map(as.data.frame, result))
  resp
}

getmarkethistory = function(market, count=50) {
  req="https://c-cex.com/t/api_pub.html?a=getmarkethistory&market=MARKET&count=COUNT"
  req = gsub("MARKET", market, gsub("COUNT", count, req))
  resp = content(GET(req), type="application/json")
  resp$result = Reduce(rbind, Map(as.data.frame, resp$result))
  resp
}

getbalancedistribution = function(currency="grc") {
  req = "https://c-cex.com/t/api_pub.html?a=getbalancedistribution&currencyname=CURRENCY"
  req = gsub("CURRENCY", currency, req)
  resp = content(GET(req), type="application/json")
  resp$result = as.vector(unlist(Reduce(c, 
    Map(as.data.frame, resp$result$Distribution))))
  resp
}

priv_req = function(req) {
  str_time = as.character(as.integer(Sys.time()))
  req = paste0(req, "&nonce=", str_time)
  sig = sha512(req, options()$ccex_secret_key)
  content(GET(req, add_headers(apisign=sig)), type="application/json")
}

buylimit = function(market, quantity, rate) {
  req = "https://c-cex.com/t/api.html?a=buylimit&apikey=APIKEY&market=MARKET&quantity=QUANTITY&rate=RATE"
  req = gsub("APIKEY", options()$ccex_api_key,
        gsub("MARKET", market,
        gsub("QUANTITY", quantity,
        gsub("RATE", rate, req))))
  ret = priv_req(req)
}

selllimit = function(market, quantity, rate) {
  req = "https://c-cex.com/t/api.html?a=selllimit&apikey=APIKEY&market=MARKET&quantity=QUANTITY&rate=RATE"
  req = gsub("APIKEY", options()$ccex_api_key,
        gsub("MARKET", market,
        gsub("QUANTITY", quantity,
        gsub("RATE", rate, req))))
  priv_req(req)
}

cancel = function(uuid) {
  req = "https://c-cex.com/t/api.html?a=cancel&apikey=APIKEY&uuid=UUID"
  req = gsub("UUID", uuid, gsub("APIKEY", options()$ccex_api_key, req))
  ret = priv_req(req)
  ret
}

getbalance = function(currency="btc") {
  req = "https://c-cex.com/t/api.html?a=getbalance&apikey=APIKEY&currency=CURRENCY"
  req = gsub("APIKEY", options()$ccex_api_key, gsub("CURRENCY", currency, req))
  resp = priv_req(req)
  if (length(resp$result) > 0) {
    resp$result = as.data.frame(resp$result)
  }
  resp
}

getbalances = function() {
  req = "https://c-cex.com/t/api.html?a=getbalances&apikey=APIKEY"
  req = gsub("APIKEY", options()$ccex_api_key, req)
  resp = priv_req(req)
  if (resp$success) resp$result=Reduce(rbind, Map(as.data.frame, resp$result))
  resp
}

getorder = function(uuid) {
  req = "https://c-cex.com/t/api.html?a=getorder&apikey=APIKEY&uuid=UUID"
  req = gsub("APIKEY", options()$ccex_api_key, 
        gsub("UUID", uuid, req))
  resp = priv_req(req)
  ret = NULL
  if (length(resp$result) > 0) {
    for(i in 1:length(resp$result)) {
      for (j in 1:length(resp$result[[i]])) {
        if (is.null(resp$result[[i]][[j]])) resp$result[[i]][[j]] = NA
      }
    }
    ret = Reduce(rbind, Map(as.data.frame, resp$result))
  }
  resp$result = ret
  resp
}

getopenorders = function(market=NULL) {
  req = "https://c-cex.com/t/api.html?a=getopenorders"
  if (!is.null(market)) req = paste0(req, "&market=", market)
  req = paste0(req, "&apikey=", options()$ccex_api_key)
  resp = priv_req(req)
  ret = list()
  if (length(resp$result) > 0) {
    for(i in 1:length(resp$result)) {
      for (j in 1:length(resp$result[[i]])) {
        if (is.null(resp$result[[i]][[j]])) resp$result[[i]][[j]] = NA
      }
    }
    ret = Reduce(rbind, Map(as.data.frame, resp$result))
  }
  ret
}

getorderhistory = function(market=NULL, count=NULL) {
  req = "https://c-cex.com/t/api.html?a=getorderhistory"
  if (!is.null(market)) req = paste0(req, "&market=", market)
  if (!is.null(count)) req = paste0(req, "&count=", count)
  req = paste0(req, "&apikey=", options()$ccex_api_key)
  resp = priv_req(req)
  ret = list()
  if (length(resp$result) > 0) {
    for(i in 1:length(resp$result)) {
      for (j in 1:length(resp$result[[i]])) {
        if (is.null(resp$result[[i]][[j]])) resp$result[[i]][[j]] = NA
      }
    }
    ret = Reduce(rbind, Map(as.data.frame, resp$result))
  }
  resp$result = ret
  resp
}

mytrades = function(market, limit=3) {
  req = "https://c-cex.com/t/api.html?a=mytrades&apikey=APIKEY&marketid=MARKET&limit=LIMIT"
  req = gsub("MARKET", market,
        gsub("LIMIT", limit, req))
  resp = priv_req(req)
  ret = list()
  if (length(resp$result) > 0) 
    ret = Reduce(rbind, Map(as.data.frame, resp$result))
  ret
  resp$result = ret
  resp
}

