library(httr)
library(rjson)
library(xml2)
library(openssl)

#' @title The List of Available Trading Pairs
#' @description The \code{pairs} function returns all currency-pairs 
#' currently available on the C-Cex crypto-currency exchange
#' \url{https://c-cex.com}.
#' @references \url{https://c-cex.com/?id=api#prices.json}
#' @return A named list with the following elements:
#' \itemize{
#'  \item{success: }{a boolean indicating if the request successful?}
#'  \item{message: }{a string describing the error if the request was not 
#'                   successful, otherwise and empty string."}
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
#' @references \url{https://c-cex.com/?id=api#prices.json}
#' @return A named list with the following elements:
#' \itemize{
#'  \item{success: }{a boolean indicating if the request successful?}
#'  \item{message: }{a string describing the error if the request was not 
#'                   successful, otherwise and empty string."}
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
#' @references \url{https://c-cex.com/?id=api#coinnames.json}
#' @return A named list with the following elements:
#' \itemize{
#'  \item{success: }{a boolean indicating if the request successful?}
#'  \item{message: }{a string describing the error if the request was not 
#'                   successful, otherwise and empty string."}
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

#' @title Retrieve the Volume of Trades for a Currency
#' @description The \code{volume} functions returns the online volume reported
#' for last 24 hours for all markets on the C-Cex crypto-currency exchange
#' (\url{https://c-cex.com}) in which the specified currency is traded.
#' @references \url{https://c-cex.com/?id=api#volume_[COIN\%20MARKET].json}
#' @param ticker the currency ticker to retrieve volume for. 
#' @return A named list with the following elements:
#' \itemize{
#'  \item{success: }{a boolean indicating if the request successful?}
#'  \item{message: }{a string describing the error if the request was not 
#'                   successful, otherwise and empty string."}
#'  \item{result: }{a \code{data.frame} with the ticker the specified 
#'    currency trades with, the last trade price, and the volume.}
#' }
#' @examples
#' \dontrun{
#' btc_vol = volume("btc")$result
#' head(btc_vol)
#' }
#' @importFrom httr GET content
#' @export
volume = function(ticker) {
  req = gsub("TICKER", ticker, "https://c-cex.com/t/volume_TICKER.json")
  resp = content(GET(req))
  ti = Reduce(rbind, Map(as.data.frame, resp$ticker))
  ti$ticker = names(resp$ticker)
  list(success=TRUE, message="", result=ti)
}

#' @title Title All Available Trading Markets and Other Meta Data
#' @description The \code{getmarkets} function returns all of the available
#' markets currently available currently available on the 
#' C-Cex crypto-currency exchange (\url{https://c-cex.com}) along with
#' other information including, among other information, when the exchange
#' was created and the minimum order size.
#' @references \url{https://c-cex.com/?id=api#getmarkets}
#' @return A named list with the following elements:
#' \itemize{
#'  \item{success: }{a boolean indicating if the request successful?}
#'  \item{message: }{a string describing the error if the request was not 
#'                   successful, otherwise and empty string."}
#'  \item{result:  }{A \code{data.frame} with the currencies, market names,
#'    whether or not the market is active, and when the market was created}
#' }
#' @examples
#' \dontrun{
#' markets = getmarkets()$result
#' head(markets)
#' }
#' @importFrom httr GET content
#' @importFrom stringr str_replace_all str_sub
#' @export
getmarkets = function() {
  resp = content(GET("https://c-cex.com/t/api_pub.html?a=getmarkets"), 
    type="application/json")
  result = Reduce(rbind, Map(as.data.frame, resp$result))
  sc_names = str_replace_all(names(result), "[A-Z]", 
    function(x) paste0("_", tolower(x)))
  names(result) = str_sub(sc_names, 2)
  list(success=TRUE, message="", result=result)
}

#' @title Get the Order Book for a Market
#' @description The \code{getorderbook} function returns the order book 
#' for a specified market on the C-Cex crypto-currency exchange 
#' \url{https://c-cex.com}.
#' @references \url{https://c-cex.com/?id=api#getorderbook}
#' @param market the market from which the order book will be retrieved.
#' @param type type of orders to retrieve (default is "both")
#' @param depth how deep should the returned order book be (default and 
#' maximum are 50).
#' @return A named list with the following elements:
#' \itemize{
#'  \item{success: }{a boolean indicating if the request successful?}
#'  \item{message: }{a string describing the error if the request was not 
#'                   successful, otherwise and empty string."}
#'  \item{result:  }{A named list with the buy and sell orders (depending
#'    on the specified \code{type} parameter. If \code{type} is "buy" or
#'    "both" then the list will contain a element named "buy" with
#'    a \code{data.frame} of the buy orders.}
#' }
#' @examples
#' \dontrun{
#' ob = getorderbook("usd-btc")$result
#' head(ob$buy)
#' head(ob$sell)
#' }
#' @importFrom httr GET content
#' @export
getorderbook = function(market, type=c("both", "buy", "sell"), depth=50) {
  req="https://c-cex.com/t/api_pub.html?a=getorderbook&market=MARKET&type=TYPE&depth=DEPTH"
  req = gsub("DEPTH", depth, gsub("TYPE", type[1], gsub("MARKET", market, req)))
  resp = content(GET(req), type="application/json")
  if (any(c("both", "buy") %in% names(resp$result))) {
    buy = Reduce(rbind, Map(as.data.frame, resp$result$buy))
    names(buy) = tolower(names(buy))
  }
  if (any(c("both", "sell") %in% names(resp$result))) {
    sell= Reduce(rbind, Map(as.data.frame, resp$result$sell))
    names(sell) = tolower(names(sell))
  }
  if (any(c("both", "buy") %in% names(resp$result)))
    resp$result$buy = buy
  if (any(c("both", "sell") %in% names(resp$result)))
    resp$result$sell = sell
  resp
}

#' @title Get a Summary of All Active Markets
#' @description the \code{getmarketsummaries} retrieves a summary of all
#' active markets for the last 24 hours on the C-Cex crypto-currency exchange 
#' \url{https://c-cex.com}.
#' @references \url{https://c-cex.com/?id=api#getmarketsummaries}
#' @return A named list with the following elements:
#' \itemize{
#'  \item{success: }{a boolean indicating if the request successful?}
#'  \item{message: }{a string describing the error if the request was not 
#'                   successful, otherwise and empty string."}
#'  \item{result:  }{A \code{data.frame} summarizing each active market's 
#'  trading activity for the last 24 hours.
#'  }
#' }
#' @examples
#' \dontrun{
#' ms = getmarketsummaries()$result
#' head(ms)
#' }
#' @importFrom httr GET content
#' @importFrom stringr str_replace_all str_sub
#' @export
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
  sc_names = str_replace_all(names(resp$result), "[A-Z]", 
    function(x) paste0("_", tolower(x)))
  names(resp$result) = str_sub(sc_names, 2)
  resp
}

#' @title Get the Recent History for a Market
#' @description the \code{getmarkethistory} function retrieves recent trade
#' information for a specified market on the C-Cex crypto-currency exchange 
#' \url{https://c-cex.com}.
#' @references \url{https://c-cex.com/?id=api#getmarkethistory}
#' @param market the market from which history data will be retrieved.
#' @param count the number of recent trades to retrieve (default is 50, max
#' is 100).
#' @return A named list with the following elements:
#' \itemize{
#'  \item{success: }{a boolean indicating if the request successful?}
#'  \item{message: }{a string describing the error if the request was not 
#'                   successful, otherwise and empty string."}
#'  \item{result:  }{A code{data.frame} containing recent trade information
#'    including the order type, time, quantity, price, and fill type.}
#' }
#' @examples
#' \dontrun{
#' mh = getmarkethistory("usd-btc")$result
#' head(mh)
#' }
#' @importFrom httr GET content
#' @importFrom stringr str_replace_all str_sub
#' @export
getmarkethistory = function(market, count=50) {
  req="https://c-cex.com/t/api_pub.html?a=getmarkethistory&market=MARKET&count=COUNT"
  req = gsub("MARKET", market, gsub("COUNT", count, req))
  resp = content(GET(req), type="application/json")
  resp$result = Reduce(rbind, Map(as.data.frame, resp$result))
  sc_names = str_replace_all(names(resp$result), "[A-Z]", 
    function(x) paste0("_", tolower(x)))
  names(resp$result) = str_sub(sc_names, 2)
  resp
}

#' @title Get the Exchange's Wallet Balance Distribution
#' @description The \code{getbalancedistribution} function retrieves the
#' C-Cex crypto-currency exchange's wallet balance distribution for a specified
#' currency.
#' @references \url{https://c-cex.com/?id=api#getbalancedistribution}
#' @examples
#' \dontrun{
#' mh = getbalancedistribution("grc")$result
#' head(ms)
#' }
#' @importFrom httr GET content
#' @export
getbalancedistribution = function(currency="grc") {
  req = "https://c-cex.com/t/api_pub.html?a=getbalancedistribution&currencyname=CURRENCY"
  req = gsub("CURRENCY", currency, req)
  resp = content(GET(req), type="application/json")
  resp$result = as.vector(unlist(Reduce(c, 
    Map(as.data.frame, resp$result$Distribution))))
  resp
}

#' @importFrom httr GET content
#' @importFrom openssl sha512
priv_req = function(req) {
  str_time = as.character(as.integer(Sys.time()))
  req = paste0(req, "&nonce=", str_time)
  sig = sha512(req, options()$ccex_secret_key)
  content(GET(req, add_headers(apisign=sig)), type="application/json")
}

#' @title Place a Buy Limit Order
#' @description The \code{buylimit} function places a buy order onto the 
#' C-Cex crypto-currency exchange \url{https://c-cex.com}. This function
#' only works after you have set up authentication.
#' @seealso \code{link{ccex_authenticate}} \code{\link{selllimit}}
#' \code{\link{getorder}} \code{\link{getopenorders}} 
#' \code{\link{getorderhistory}}
#' @references \url{https://c-cex.com/?id=api#buylimit}
#' @param market the market to place the buy limit order on.
#' @param quantity the quantity of the transaction currency to buy.
#' @param rate the price you are willing to pay per unit of the 
#' transaction currency.
#' @return A named list with the following elements:
#' \itemize{
#'  \item{success: }{a boolean indicating if the request successful?}
#'  \item{message: }{a string describing the error if the request was not 
#'                   successful, otherwise and empty string."}
#'  \item{result:  }{a named list, called "uuid" whose element is an integer
#'    identifying the order. This value is used to query the status of
#'    of the order with either the \code{link{getorder}} or 
#'    \code{link{getopenorders}} function. When the order is fulfilled it
#'    appears in the order history \code{data.frame} returned by the
#'    \code{link{getorderhistory}} function.
#'  }
#' }
#' @examples
#' \dontrun{
#' # Buy one bitcoin for 1169 dollars.
#' order = buylimit("btc-usd", 1, 1169)
#' }
#' @export
buylimit = function(market, quantity, rate) {
  req = "https://c-cex.com/t/api.html?a=buylimit&apikey=APIKEY&market=MARKET&quantity=QUANTITY&rate=RATE"
  req = gsub("APIKEY", options()$ccex_api_key,
        gsub("MARKET", market,
        gsub("QUANTITY", quantity,
        gsub("RATE", rate, req))))
  priv_req(req)
}

#' @title Place a Sell Limit Order
#' @description The \code{selllimit} function places a buy order onto the 
#' C-Cex crypto-currency exchange \url{https://c-cex.com}. This function
#' only works if you have set up authentication. 
#' @seealso \code{link{ccex_authenticate}} \code{\link{buylimit}}
#' \code{\link{getorder}} \code{\link{getopenorders}} 
#' \code{\link{getorderhistory}}
#' @references \url{https://c-cex.com/?id=api#buylimit}
#' @param market the market to place the buy limit order on.
#' @param quantity the quantity of the reference currency to sell. 
#' @param rate the price you would like to get per unit of the 
#' transaction 
#' currency.
#' @return A named list with the following elements:
#' \itemize{
#'  \item{success: }{a boolean indicating if the request successful?}
#'  \item{message: }{a string describing the error if the request was not 
#'                   successful, otherwise and empty string."}
#'  \item{result:  }{a named list, called "uuid" whose element is an integer
#'    identifying the order. This value is used to query the status of
#'    of the order with either the \code{link{getorder}} or 
#'    \code{link{getopenorders}} function. When the order is fulfilled it
#'    appears in the order history \code{data.frame} returned by the
#'    \code{link{getorderhistory}} function.
#'  }
#' }
#' @examples
#' \dontrun{
#' # Sell one bitcoin for 1169 dollars.
#' order = selllimit("btc-usd", 1, 1169)
#' }
#' @export
selllimit = function(market, quantity, rate) {
  req = "https://c-cex.com/t/api.html?a=selllimit&apikey=APIKEY&market=MARKET&quantity=QUANTITY&rate=RATE"
  req = gsub("APIKEY", options()$ccex_api_key,
        gsub("MARKET", market,
        gsub("QUANTITY", quantity,
        gsub("RATE", rate, req))))
  priv_req(req)
}

#' @title Authenticate Your Account on the C-Cex Exchange
#' @export
ccex_authenticate = function() {
}

#' @title Cancel an Open Order
#' @description The \code{cancel} function cancels an open order on the
#' C-Cex crypto-currency exchange \url{https://c-cex.com}. This function
#' is called after providing information to authenticate your account and 
#' after an order is placed using either 
#' the \code{link{buylimit}} or \code{link{selllimit}} functions.
#' @seealso \code{\link{ccex_authenticate}} \code{\link{getorder}}
#' @references \url{https://c-cex.com/?id=api#cancel}
#' @param uuid the uuid of the order you would like to cancel.
#' @return A named list with the following elements:
#' \itemize{
#'  \item{success: }{a boolean indicating if the request successful?}
#'  \item{message: }{a string describing the error if the request was not 
#'                   successful, otherwise and empty string."}
#'  \item{result:  }{always NULL}
#' }
#' \code{\link{getopenorders}} \code{\link{getorderhistory}} 
#' \code{\link{buylimit}} \code{\link{selllimit}}
#' @examples
#' \dontrun{
#' cancel(uuid) 
#' }
#' @export
cancel = function(uuid) {
  req = "https://c-cex.com/t/api.html?a=cancel&apikey=APIKEY&uuid=UUID"
  req = gsub("UUID", uuid, gsub("APIKEY", options()$ccex_api_key, req))
  ret = priv_req(req)
  ret
}

#' Get the Balance a Specified Currency
#' @description The \code{getbalance} function retrieves the account balance
#' for a specified currency on the C-Cex crypto-currency 
#' exchange \url{https://c-cex.com}. This function
#' can be used after you provide information for authentication.
#' @seealso \code{\link{ccex_authenticate}}
#' @references \url{https://c-cex.com/?id=api#getbalance}
#' @param currency the currency to get your account balance for. 
#' @return A named list with the following elements:
#' \itemize{
#'  \item{success: }{a boolean indicating if the request successful?}
#'  \item{message: }{a string describing the error if the request was not 
#'                   successful, otherwise and empty string."}
#'  \item{result:  }{a \code{data.frame} with the currency, balance, 
#'    available funds, the amount of any pending transactions, and a
#'    crypographic address that can be used to receive more funding.
#'  }
#' }
#' @examples
#' \dontrun{
#' getbalance("btc") 
#' }
#' @export
getbalance = function(currency="btc") {
  req = "https://c-cex.com/t/api.html?a=getbalance&apikey=APIKEY&currency=CURRENCY"
  req = gsub("APIKEY", options()$ccex_api_key, gsub("CURRENCY", currency, req))
  resp = priv_req(req)
  if (length(resp$result) > 0) {
    resp$result = as.data.frame(resp$result)
  }
  names(resp$result) = tolower(names(resp$result))
  resp
}

#' Get the Balances for All Currencies
#' @description The \code{getbalances} function retrieves the account balance
#' for all currencies on the C-Cex crypto-currency 
#' exchange \url{https://c-cex.com}. This function
#' can be used after you provide information for authentication.
#' @seealso \code{\link{ccex_authenticate}}
#' @references \url{https://c-cex.com/?id=api#getbalances}
#' @return A named list with the following elements:
#' \itemize{
#'  \item{success: }{a boolean indicating if the request successful?}
#'  \item{message: }{a string describing the error if the request was not 
#'                   successful, otherwise and empty string."}
#'  \item{result:  }{a \code{data.frame} with the currencies, balances, 
#'    available funds, the amount of any pending transactions, and 
#'    crypographic addresses that can be used to receive funding.
#'  }
#' }
#' @examples
#' \dontrun{
#' balances = getbalances()$result
#' }
#' @export
getbalances = function() {
  req = "https://c-cex.com/t/api.html?a=getbalances&apikey=APIKEY"
  req = gsub("APIKEY", options()$ccex_api_key, req)
  resp = priv_req(req)
  if (resp$success) {
    resp$result=Reduce(rbind, Map(as.data.frame, resp$result))
    names(resp$result) = tolower(names(resp$result))
  }
  resp
}

#' @title Get Order Data
#' @description The \code{getorder} function retrieves open order data 
#' on the C-Cex crypto-currency 
#' exchange \url{https://c-cex.com}. This function
#' can be used after you provide information for authentication.
#' @seealso \code{\link{ccex_authenticate}} \code{\link{getopenorders}}.
#' @references \url{https://c-cex.com/?id=api#getorder}
#' @param uuid the uuid of the order.
#' @return A named list with the following elements:
#' \itemize{
#'  \item{success: }{a boolean indicating if the request successful?}
#'  \item{message: }{a string describing the error if the request was not 
#'                   successful, otherwise and empty string."}
#'  \item{result:  }{a \code{data.frame} providing information about the 
#'    open order including (but not limited to) the market, quantity remaining
#'    in the order, the type of order, and when the order was opened.
#'  }
#' }
#' @examples
#' \dontrun{
#' getorder(uuid)
#' }
#' @importFrom stringr str_replace_all str_sub
#' @export
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
    sc_names = str_replace_all(names(ret), "[A-Z]", 
      function(x) paste0("_", tolower(x)))
    names(ret) = str_sub(sc_names, 2)
  }
  resp$result = ret
  resp
}

#' @title Get Data for All Open Orders
#' @description The \code{getopenorders} function retrieves all open orders
#' on the C-Cex crypto-currency 
#' exchange \url{https://c-cex.com}. This function
#' can be used after you provide information for authentication.
#' @seealso \code{\link{ccex_authenticate}} \code{\link{getorder}}
#' \code{\link{mytrades}}
#' @references \url{https://c-cex.com/?id=api#getorder}
#' @param market (optional) the market on which you would like to see all 
#' open orders. If not specified, then all open orders
#' for all markets are returned.
#' @return A named list with the following elements:
#' \itemize{
#'  \item{success: }{a boolean indicating if the request successful?}
#'  \item{message: }{a string describing the error if the request was not 
#'                   successful, otherwise and empty string."}
#'  \item{result:  }{a \code{data.frame} providing information about the 
#'    open orders including (but not limited to) the market, quantity remaining
#'    in the order, the type of order, and when the order was opened.
#'  }
#' }
#' @examples
#' \dontrun{
#' getopenorders("btc-usd")
#' }
#' @importFrom stringr str_replace_all str_sub
#' @export
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
    sc_names = str_replace_all(names(ret), "[A-Z]", 
      function(x) paste0("_", tolower(x)))
    names(ret) = str_sub(sc_names, 2)
  }
  resp
}

#' @title Get Order History
#' @description The \code{getorderhistory} function retrieves order history
#' data on the C-Cex crypto-currency exchange \url{https://c-cex.com}. This 
#' function can be used after you provide information for authentication.
#' @seealso \code{\link{ccex_authenticate}}
#' @references \url{https://c-cex.com/?id=api#getorderhistory}
#' @param market (optional) the market on which you would like to see all 
#' open orders. If not specified, then completed orders for all markets are 
#' returned.
#' @param count (optional) the number of records to return.
#' @return A named list with the following elements:
#' \itemize{
#'  \item{success: }{a boolean indicating if the request successful?}
#'  \item{message: }{a string describing the error if the request was not 
#'                   successful, otherwise and empty string."}
#'  \item{result:  }{a \code{data.frame} providing data about 
#'    previously completed orders including the order uuid, the exchange
#'    the time of the order, the order type, the limit, the quantity, the
#'    quantity remaining, the commission, the price, the price per unit,
#'    and whether or not it was a conditional trade.
#'  }
#' }
#' @examples
#' \dontrun{
#' getorderhistory()
#' }
#' @importFrom stringr str_replace_all str_sub
#' @export
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
    sc_names = str_replace_all(names(ret), "[A-Z]", 
      function(x) paste0("_", tolower(x)))
    names(ret) = str_sub(sc_names, 2)
  }
  resp$result = ret
  resp
}

#' @title Get a Detailed Trading History for a Market
#' @description The \code{mytrades} function retrieves a detailed
#' trading history for a specified market on the C-Cex crypto-currency 
#' exchange \url{https://c-cex.com}. This function
#' can be used after you provide information for authentication.
#' @seealso \code{\link{ccex_authenticate}} \code{\link{getorderhistory}}
#' @param market the market on which you would like to see the history data.
#' @param limit (optional) the number of records to return.
#' @return A named list with the following elements:
#' \itemize{
#'  \item{success: }{a boolean indicating if the request successful?}
#'  \item{message: }{a string describing the error if the request was not 
#'                   successful, otherwise and empty string."}
#'  \item{result:  }{a \code{data.frame} providing detailed information about
#'    trades on the market including (but not limited to) the order type,
#'    the time of the trade, the trade price, the quantity, and the uuid.
#'  }
#' }
#' @examples
#' \dontrun{
#' mytrades("btc-usd")
#' }
#' @importFrom stringr str_replace_all str_sub
#' @export
mytrades = function(market, limit=3) {
  req = "https://c-cex.com/t/api.html?a=mytrades&apikey=APIKEY&marketid=MARKET&limit=LIMIT"
  req = gsub("MARKET", market,
        gsub("LIMIT", limit, req))
  resp = priv_req(req)
  ret = list()
  if (length(resp$result) > 0) {
    ret = Reduce(rbind, Map(as.data.frame, resp$result))
    sc_names = str_replace_all(names(ret), "[A-Z]", 
      function(x) paste0("_", tolower(x)))
    names(ret) = str_sub(sc_names, 2)
  }
  resp$result = ret
  resp
}

