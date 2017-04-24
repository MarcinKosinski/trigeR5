library(dplyr)
library(magrittr)
library(pbapply)
library(readr)
library(RSQLite)
library(rvest)
library(stringi)

update_csv <- function(table) {
  read_csv(paste0("data/", table, ".csv")) %>%
    bind_rows(get(table, envir = .GlobalEnv)) %>%
    unique() %>%
    write_csv(paste0("data/", table, ".csv"))
}


# setwd('R/') if you run this not using `run_.sh` from the main directory
# db <- dbConnect(drv = SQLite(), dbname = "../data/wp.db")
list.files('R/wp/', full.names = TRUE, pattern = 'wp') %>%
  sapply(source, echo = TRUE, encoding = 'UTF-8')
# dbDisconnect(db)

