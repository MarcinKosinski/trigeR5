#### dicts ####
# https://github.com/morfologik/
library(data.table)
polish_lematization <- fread('../dicts/polimorfologik-2.1.txt', data.table = FALSE, encoding = 'UTF-8')
polish_stopwords <- strsplit(readLines('../dicts/polish_stopwords.txt', encoding = 'UTF-8'), ", ")[[1]]

lower_lema <- polish_lematization %>%
  set_colnames(c('to', 'from', 'no_need')) %>%
  select(-no_need) %>%
  mutate(to   = tolower(to),
         from = tolower(from))

splits_num <- 1:20
lower_lema_split <- 
  split(lower_lema, 
        f = 
          sample(
            splits_num,
            size = nrow(lower_lema),
            replace = TRUE))

#### remove previous data ####
db <- dbConnect(drv = SQLite(), dbname = "../data/wp.db")

dbListTables(db) %>%
  grep('_lematized|stem', . , value = TRUE) %>%
  sapply(function(stem_db){
    dbRemoveTable(db, stem_db)
  })

#### stem ####
library(tm)
dbListTables(db) %>%
  pbsapply(function(table){
    text = 
      dbGetQuery(db, paste0('SELECT bodies FROM ',  table)) %>%
      use_series(bodies) %>%
      lapply(tolower) %>%
      lapply(tm::removeWords, c("\"", "ul.", "godz.", polish_stopwords, 1:2000)) %>% 
      stri_extract_all_words()
    
    dict <- data_frame(
      words   = unlist(text),
      article = mapply(rep,
                       1:length(text),
                       lapply(text, length)) %>%
        unlist()
    )
    for(i in splits_num) {
      dict$words <- dict$words %>%
        plyr::mapvalues(warn_missing = FALSE,
                        from = lower_lema_split[[i]]$from,
                        to   = lower_lema_split[[i]]$to)
    }
    
    assign(
      x = table,
      envir = .GlobalEnv,
      value = dict
    )
    invisible(NULL)
  })

ls(pattern = 'wp') %>%
  pbsapply(function(table){
    arts_words <- get(table, envir = .GlobalEnv) %>% 
      group_by(article) %>%
      mutate(words_in_art = paste0(words, collapse = " ")) %>% 
      select(article, words_in_art) %>%
      unique() %>% 
      ungroup() %>%
      as_data_frame()
    
    # get(table, envir = .GlobalEnv) %>% 
    # group_by(article) %>%
    # do(words_in_art = paste0(.$words, collapse = " ")) %>%
    
    dbWriteTable(db, paste0(table, "_lematized"), arts_words)  
  }
)
  
dbDisconnect(db)
  
