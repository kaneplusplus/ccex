---
title: 'ccex: An R client for the C-Cex Crypto Currency Exchange'
tags:
  - finance
  - crypto-currency
  - open exchanges
authors:
 - name: Michael Kane
   orcid: 0000-0003-1899-6662
   affiliation: 1
affiliations:
 - name: Yale University
   index: 1
date: 1 April 2017
bibliography: paper.bib
---

# Summary

Package ```ccex``` [@ccex] is an R [@R] client for the REST server used by the
C-Cex crypto-currency exchange [@ccex_site]. The package provides 
functions for all endpoints supported by the exchange and includes the
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

# References
  
