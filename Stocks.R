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

#Attempting to import multiple csvs from multiple folders into 1 dataframe
  
  printname <- function(filename){
    pathname <- str_c("./", filename)
    return(pathname)
  }

  import_multi <- function(filelist){
    stocklist <- c("forbes2000", "nasdaq", "nyse", "sp500")
    list.files(path = filelist, full.names = T) %>% 
      map(~GetFileName(stocklist))
  }