#' @title Client for the C-Cex Crypto-currency Exchange
#' @name ccex-package
#' @aliases ccex-package ccex
#' @docType package
#' @references \url{https://github.com/kaneplusplus/ccex}
#' \url{https://c-cex.com/}
#' @description
#' This software is in no way affiliated, endorsed, or approved by
#' the C-Cex crypto-currency exchange or any of its affiliates. It comes with
#' absolutely no warranty and should not be used in actual trading
#' unless the user can read and understand the source and know what you are
#' doing.
#' 
#' Package 'ccex' is an R implementation of the REST interface used by the C-Cex
#' crypto-currency exchange \url{https://c-cex.com/}. It provides functions 
#' for all endpoints currently (as of April 1, 2017) supported by the 
#' exchange. This includes the ability 
#' to retrieve price, volume, and orderbook information as well as the ability
#' to trade crypto-currencies.
#' 
#' Calls to the exchange are categorized as either public, which includes 
#' requests for price, volume, and order book information, and private, which 
#' includes all requests requiring an account including placing buy or sell 
#' orders. Public calls can be used directly by installing the package. 
#' Private calls require creating an account at 
#' \url{https://c-cex.com/?id=reg} and creating an API and secret key with 
#' appropriate permissions.
#' 
#' Private calls retrieve the API and secret key using the CCEX_API_KEY and 
#' CCEX_SECRET_KEY environment variables. These may be set by the user before 
#' opening the R session or, they can be set using the 'ccex_authenticate' 
#' function.
#' 
#' Public Function Calls
#' \itemize{
#' \item{coinnames: }{coin names for all currencies}
#' \item{getballancedistribution: }{exchange's wallet balance distribution}
#' \item{getmarkethistory: }{recent history for a market}
#' \item{getmarkets: }{available markets and other meta-data}
#' \item{getmarketsummaries: }{summary of active markets}
#' \item{getorderbook: }{order book for a market}
#' \item{pairs: }{available trading pairs}
#' \item{prices: }{prices for all markets}
#' \item{volume: }{volume of trades for a currency}
#' }
#' Private Function Calls
#' \itemize{
#' \item{buylimit: }{place a buy limit order}
#' \item{cancel: }{cancel an open order}
#' \item{ccex_authenticate: }{provide user authentication data}
#' \item{getbalance: }{account balance for a specified currency}
#' \item{getblanances: }{account balances for currencies}
#' \item{getopenorders: }{order data for all open orders}
#' \item{getorderdata: }{order data for a specified order}
#' \item{getorderhistory: }{recent order history for an account }
#' \item{mytrades: }{detailed recent trading history for a market}
#' \item{selllimit: }{place a sell limit order}
#' }
NULL

as_data_frame = function(x) {
  as.data.frame(x, stringsAsFactors=FALSE)
}

camel_to_snake = function(x) {
  gsub("^_+", "", tolower(gsub("([A-Z])", "_\\1", x)))
}

#' @title Available Trading Pairs
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

#' @title Prices for Markets 
#' @description The \code{prices} function returns the prices for all markets
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
  ret = Reduce(rbind, Map(as_data_frame, resp))
  ret$market = names(resp)
  list(success=TRUE, message="", result=ret)
}

#' @title Coin Names for Currencies
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

#' @title Volume of Trades for a Currency
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
  ti = Reduce(rbind, Map(as_data_frame, resp$ticker))
  ti$ticker = names(resp$ticker)
  list(success=TRUE, message="", result=ti)
}

#' @title Available Markets and Other Meta Data
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
#' @export
getmarkets = function() {
  resp = content(GET("https://c-cex.com/t/api_pub.html?a=getmarkets"), 
    type="application/json")
  result = Reduce(rbind, Map(as_data_frame, resp$result))
  names(result) = camel_to_snake(names(result))
  list(success=TRUE, message="", result=result)
}

#' @title Order Book for a Market
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
    buy = Reduce(rbind, Map(as_data_frame, resp$result$buy))
    names(buy) = tolower(names(buy))
  }
  if (any(c("both", "sell") %in% names(resp$result))) {
    sell= Reduce(rbind, Map(as_data_frame, resp$result$sell))
    names(sell) = tolower(names(sell))
  }
  if (any(c("both", "buy") %in% names(resp$result)))
    resp$result$buy = buy
  if (any(c("both", "sell") %in% names(resp$result)))
    resp$result$sell = sell
  resp
}

#' @title Summary of All Active Markets
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
  resp$result = Reduce(rbind, Map(as_data_frame, result))
  names(resp$result) = camel_to_snake(names(resp$result))
  resp$time_stamp = strptime(resp$time_stamp, "%Y-%m-%d %H:%M:%S", tz="GMT")
  resp
}

#' @title Recent History for a Market
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
#' @export
getmarkethistory = function(market, count=50) {
  req="https://c-cex.com/t/api_pub.html?a=getmarkethistory&market=MARKET&count=COUNT"
  req = gsub("MARKET", market, gsub("COUNT", count, req))
  resp = content(GET(req), type="application/json")
  resp$result = Reduce(rbind, Map(as_data_frame, resp$result))
  names(resp$result) = camel_to_snake(names(resp$result))
  resp$result$time_stamp = 
    strptime(resp$result$time_stamp, "%Y-%m-%d %H:%M:%S", tz="GMT")
  resp
}

#' @title Exchange's Wallet Balance Distribution
#' @description The \code{getbalancedistribution} function retrieves the
#' C-Cex crypto-currency exchange's wallet balance distribution for a specified
#' currency.
#' @param currency the currency to get the balance distrubtion for.
#' @references \url{https://c-cex.com/?id=api#getbalancedistribution}
#' @examples
#' \dontrun{
#' mh = getbalancedistribution("grc")$result
#' head(ms)
#' }
#' @importFrom httr GET content
#' @export
getbalancedistribution = function(currency) {
  req = "https://c-cex.com/t/api_pub.html?a=getbalancedistribution&currencyname=CURRENCY"
  req = gsub("CURRENCY", currency, req)
  resp = content(GET(req), type="application/json")
  resp$result = as.vector(unlist(Reduce(c, 
    Map(as_data_frame, resp$result$Distribution))))
  resp
}

#' @importFrom httr GET content add_headers
#' @importFrom openssl sha512
priv_req = function(req) {
  str_time = as.character(as.integer(Sys.time()))
  req = paste0(req, "&nonce=", str_time)
  sig = sha512(req, Sys.getenv("CCEX_SECRET_KEY"))
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
  req = gsub("APIKEY", Sys.getenv("CCEX_API_KEY"),
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
  req = gsub("APIKEY", Sys.getenv("CCEX_API_KEY"),
        gsub("MARKET", market,
        gsub("QUANTITY", quantity,
        gsub("RATE", rate, req))))
  priv_req(req)
}

#' @title Provide User Authentication Data
#' @description The \code{ccex_authenicate} function sets the 
#' CCEX_API_KEY and CCEX_SECRET_KEY environment variables in your current
#' session to access your account information on the C-Cex crypto-currency
#' exchange (\url{https://c-cex.com}).
#' @references \url{https://c-cex.com/?id=api#authentication}
#' @param api_key the api key provided by the exchange
#' @param secret_key the secret key provided by the exchange
#' @export
ccex_authenticate = function(api_key, secret_key) {
  Sys.setenv("CCEX_API_KEY"=api_key)
  Sys.setenv("CCEX_SECRET_KEY"=secret_key)
  invisible(TRUE)
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
  req = gsub("UUID", uuid, gsub("APIKEY", Sys.getenv("CCEX_API_KEY"), req))
  ret = priv_req(req)
  ret
}

#' Account Balance for a Specified Currency
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
  req = gsub("APIKEY", Sys.getenv("CCEX_API_KEY"),
    gsub("CURRENCY", currency, req))
  resp = priv_req(req)
  if (length(resp$result) > 0) {
    resp$result = as_data_frame(resp$result)
  }
  names(resp$result) = tolower(names(resp$result))
  resp
}

#' Account Balances for All Currencies
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
  req = gsub("APIKEY", Sys.getenv("CCEX_API_KEY"), req)
  resp = priv_req(req)
  if (resp$success) {
    resp$result=Reduce(rbind, Map(as_data_frame, resp$result))
    names(resp$result) = tolower(names(resp$result))
  }
  resp
}

#' @title Order Data for a Specified Order
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
#' @export
getorder = function(uuid) {
  req = "https://c-cex.com/t/api.html?a=getorder&apikey=APIKEY&uuid=UUID"
  req = gsub("APIKEY", Sys.getenv("CCEX_API_KEY"),
        gsub("UUID", uuid, req))
  resp = priv_req(req)
  ret = NULL
  if (length(resp$result) > 0) {
    for(i in 1:length(resp$result)) {
      for (j in 1:length(resp$result[[i]])) {
        if (is.null(resp$result[[i]][[j]])) resp$result[[i]][[j]] = NA
      }
    }
    ret = Reduce(rbind, Map(as_data_frame, resp$result))
    names(ret) = camel_to_snake(names(ret))
  }
  resp$result = ret
  resp
}

#' @title Order Data for all Open Orders
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
#' @export
getopenorders = function(market=NULL) {
  req = "https://c-cex.com/t/api.html?a=getopenorders"
  if (!is.null(market)) req = paste0(req, "&market=", market)
  req = paste0(req, "&apikey=", Sys.getenv("CCEX_API_KEY"))
  resp = priv_req(req)
  ret = list()
  if (length(resp$result) > 0) {
    for(i in 1:length(resp$result)) {
      for (j in 1:length(resp$result[[i]])) {
        if (is.null(resp$result[[i]][[j]])) resp$result[[i]][[j]] = NA
      }
    }
    ret = Reduce(rbind, Map(as_data_frame, resp$result))
    names(ret) = camel_to_snake(names(ret))
  }
  resp
}

#' @title Recent Order History for an Account
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
#' @export
getorderhistory = function(market=NULL, count=NULL) {
  req = "https://c-cex.com/t/api.html?a=getorderhistory"
  if (!is.null(market)) req = paste0(req, "&market=", market)
  if (!is.null(count)) req = paste0(req, "&count=", count)
  req = paste0(req, "&apikey=", Sys.getenv("CCEX_API_KEY"))
  resp = priv_req(req)
  ret = list()
  if (length(resp$result) > 0) {
    for(i in 1:length(resp$result)) {
      for (j in 1:length(resp$result[[i]])) {
        if (is.null(resp$result[[i]][[j]])) resp$result[[i]][[j]] = NA
      }
    }
    ret = Reduce(rbind, Map(as_data_frame, resp$result))
    names(ret) = camel_to_snake(names(ret))
    ret$time_stamp = strptime(ret$time_stamp, "%Y-%m-%d %H:%M:%S", tz="GMT")
  }
  resp$result = ret
  resp
}

#' @title Detailed Trading History for a Market
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
#' @export
mytrades = function(market, limit=3) {
  req = "https://c-cex.com/t/api.html?a=mytrades&apikey=APIKEY&marketid=MARKET&limit=LIMIT"
  req = gsub("MARKET", market,
        gsub("LIMIT", limit, 
        gsub("APIKEY", Sys.getenv("CCEX_API_KEY"), req)))
  resp = priv_req(req)
  ret = list()
  if (length(resp$return) > 0) {
    ret = Reduce(rbind, Map(as_data_frame, resp$return))
    ret$datetime = strptime(ret$datetime, "%Y-%m-%d %H:%M:%S", tz="GMT")
  }
  resp$return = NULL
  resp$result = ret
  resp
}

