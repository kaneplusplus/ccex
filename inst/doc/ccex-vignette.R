## ---- echo=TRUE, warning=FALSE, message=FALSE, eval=FALSE----------------
#  #install.packages("devtools")
#  devtools::install_github("kaneplusplus/ccex")

## ---- fig.height=6, fig.width=7------------------------------------------
# Create a function to load libraries quietly.
shut_up = function(load_lib) {
  suppressWarnings(suppressPackageStartupMessages(load_lib))
}

# Load the libraries quietly.
shut_up(library(ccex))
shut_up(library(threejs))
shut_up(library(tidyverse))
shut_up(library(igraph))

# Create a graph of the crypo-currency markets.
currency_pairs = pairs()$result
currency_graph = currency_pairs %>% strsplit("-") %>% unlist %>% 
  matrix(ncol=2, byrow=TRUE) %>% as.data.frame(stringsAsFactors=FALSE) %>% 
  graph_from_data_frame
graphjs(currency_graph, vertex.size=0.5, vertex.label=V(currency_graph)$name, edge.width=0.25)

## ---- fig.height=6, fig.width=7------------------------------------------
shut_up(library(quantmod))

# Get the recent history of the usd-btc market.
btc_usd_df = getmarkethistory("btc-usd", 100)$result
btc_usd = btc_usd_df %>% select(price, quantity) %>% 
  xts(order.by=btc_usd_df$time_stamp)
names(btc_usd)[2] = "volume"

# Chart the recent price history.
chartSeries(btc_usd)

## ---- fig.height=4, fig.width=7------------------------------------------
# Create a data.frame frome the "buy" and "sell" orders.
btc_usd_req = getorderbook("btc-usd")$result
btc_usd_req$buy$type = "buy"
btc_usd_req$sell$type = "sell"
btc_usd = bind_rows(btc_usd_req$buy, btc_usd_req$sell)

# Plot the order book.
ggplot(btc_usd, aes(x=rate, y=quantity, group=type, color=type, fill=type)) + 
  geom_bar(stat="identity") + xlab("Price") +
  ylab("Order Size") + scale_fill_discrete(name = "Order Type") + 
  scale_color_discrete(guide=FALSE)

