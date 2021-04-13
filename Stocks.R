library(tidyverse)
library(data.table)
library(lubridate)

setwd("./stock_market_data") # Default dir is /R

GetFileName <- function(file){
  # Reads all .csv files in the folder
  read_csv(file) %>% 
    mutate(stock = basename(file)) %>%     #adds filename, removes path name
    mutate(stock = str_sub(stock, 1, -5))  #removes .csv extension
}

  get_2021 <- function(filename) {
    # Get only 2021 Data
    # But doesnt keep the data type as Date
      filter(filename, dmy(Date) >= "2021-01-01")
    }

# Stock data folders
  forbes2000 <- list.files(path = "./forbes2000", full.names = T) %>%
   map_df(~GetFileName(.))

  nasdaq <- list.files(path = "./nasdaq", pattern = ".csv", full.names = T) %>%
   map_df(~GetFileName(.))

  nyse <- list.files(path = "./nyse", pattern = ".csv", full.names = T) %>%
   map_df(~GetFileName(.))

  sp500 <- list.files(path = "./sp500", pattern = ".csv", full.names = T) %>%
    map_df(~GetFileName(.))

# Attempting to import multiple csvs from multiple folders into 1 dataframe
  
  printname <- function(filename){
    pathname <- str_c("./", filename)
    return(pathname)
  }

  import_multi <- function(filelist){
    stocklist <- c("forbes2000", "nasdaq", "nyse", "sp500")
    list.files(path = filelist, full.names = T) %>% 
      map(~GetFileName(stocklist))
  }
  
  
# Calculations
  x <- sp500  # Test variables
  today <- tail(x$Date, 1) # Get last date in the table
  x <- x %>% get_2021() # Filters 2021
  x$Date <- dmy(x$Date) # Converts data type to date
  
  x <- x %>% group_by(stock)
  x <- x %>% 
        mutate(open_adjclose = `Adjusted Close` - Open)
  
# Daily gains/losses
    
  winners <- x %>% # Top 10 biggest gains of today
    filter(open_adjclose > 0 & Date == today |  Date == (today - 7)) %>% 
    arrange(open_adjclose %>% desc())
  
  losers <- x %>% # Top 10 biggest loss of today
    filter(open_adjclose < 0 & Date == today) %>% 
    arrange(open_adjclose)
  
  # Tracking Weekly gains/loss
  y <- x %>%  filter(open_adjclose > 0 & Date == today |  Date == (today - 7)) 
  weekly <- y %>% mutate(week_dif = `Adjusted Close` - lag(Open, order_by = stock))
  weekly %>% 
    filter(!is.na(week_dif)) %>% 
    arrange(week_dif)
  
  weekly %>% 
    filter(!is.na(week_dif)) %>% 
    arrange(desc(week_dif))
