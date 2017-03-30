
<!-- README.md is generated from README.Rmd. Please edit that file -->
ccex: An R client for the [C-Cex Crypto Currency Exchange](https://c-cex.com)
=============================================================================

Disclaimer
==========

This software is in no way affiliated, endorsed, or approved by the
C-Cex crypto-currency exchange or any of its affiliates. It comes with
absolutely no warranty and should not be used in actual trading unless
the user can read and understand the source and know what you are doing.

This software is Licenced under the LGPL version 2.

Overview
========

Package 'ccex' is an R implementation of the REST interface used by the
[C-Cex crypto-currency exchange](https://c-cex.com/). It provides
functions for endpoints supported by the exchange. This includes the
ability to retrieve price, volume, and orderbook information as well as
the ability to trade crypto-currencies.

Calls to the exchange are categorized as either public, which includes
requests for price, volume, and order book information, and private,
which includes all requests requiring an account including placing buy
or sell orders. Public calls can be used directly by installing the
package. Private calls require that you [create an
account](https://c-cex.com/?id=reg) and create an API and secret key
with appropriate permissions.

Private calls retrieve the API and secret key using the CCEX\_API\_KEY
and CCEX\_SECRET\_KEY environment variables. These may be set by the
user before opening the R session or, they can be set using the
'ccex\_authenticate' function.

Quickstart
==========

Install
-------

The package is available from GitHub and will be uploaded to CRAN
shortly. If you wish to install the development version then install the
[devtools package](https://CRAN.R-project.org/package=devtools),
available from CRAN. A

``` r
#install.packages("devtools")
devtools::install_github("kaneplusplus/ccex")
```

    ## Downloading GitHub repo kaneplusplus/ccex@master
    ## from URL https://api.github.com/repos/kaneplusplus/ccex/zipball/master

    ## Installing ccex

    ## '/usr/local/Cellar/r/3.3.3_1/R.framework/Resources/bin/R' --no-site-file  \
    ##   --no-environ --no-save --no-restore --quiet CMD INSTALL  \
    ##   '/private/var/folders/w4/883ym75560l31lswx77lpgy40000gn/T/RtmpGet5n2/devtools29dd48c29344/kaneplusplus-ccex-c1ffbae'  \
    ##   --library='/usr/local/lib/R/3.3/site-library' --install-tests

    ## 

Using the Package
-----------------

After installation, you may query the exchange with any of the public
calls. For example, if we want to see the spread of the cost of doge
coins in bitcoins, we can use the following code.

``` r
library(ccex)
library(scales)
library(ggplot2)

# The price of doge coins in bitcoins.
btc_usd = ccex::getmarkethistory(market='doge-btc', count=100)$result

# ggplot2 handle POSIXct types only.
btc_usd$time_stamp = as.POSIXct(btc_usd$time_stamp)

ggplot(btc_usd, aes(x=time_stamp, y=price, group=order_type, 
  color=order_type)) + geom_line() + 
  scale_x_datetime(breaks=date_breaks("hours"), 
    labels=date_format("%m-%d %H:%M")) + xlab("Date and Time") +
  ylab("Price") + scale_colour_discrete(name="Order Type")
```

[](inst/doc/README_files/figure-markdown_github-hard_line_breaks/unnamed-chunk-2-1.png)
