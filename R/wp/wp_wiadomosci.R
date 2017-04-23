#### WIADOMOŚCI ####
adress <- c("http://wiadomosci.wp.pl/")
adresses <- c("polska", "swiat", "spoleczenstwo", "przestepczosc", "polityka",
              "ciekawostki", "nauka", "tylko-w-wp", "dzieje-sie-na-zywo") %>%
  paste0(adress, .)

links <- pblapply(adresses, function(one_ad) {
  ad_html <- read_html(one_ad)
  ad_nodes <- html_nodes(ad_html, css = "div")
  
  ad_text <- ad_nodes %>%
    grep("menuText-Raporty desktop", .) %>%
    ad_nodes[.] %>%
    as.character()
  
  biggest_list <- ad_text %>%
    stri_count_regex("href") %>%
    which.max()
  
  big_links <- biggest_list %>%
    ad_text[.] %>%
    stri_extract_all_regex("href.*[[:digit:]].*data-reactid.*<div") %>%
    unlist() %>%
    gsub("href=\"/", adress, .) %>%
    gsub("\".*", "", .)
  
  articles <- big_links %>%
    grepl("[[:digit:]]", .) %>%
    big_links[.]
  
  articles
}) %>%
  unlist() %>%
  unique()

# db <- dbConnect(drv = SQLite(), dbname = "../data/wp.db")
db_links <- dbGetQuery(db, "SELECT links FROM wp_wiadomosci")
links <- setdiff(links, db_links$links)

bodies <- pblapply(links, function(link) {
  link %>%
    read_html() %>%
    html_nodes(css = "p , ._1HGmjUl , ._1xAmRvR") %>%
    html_text() %>%
    paste0(collapse = " ")
}) %>%
  unlist() %>%
  gsub("'", "''", .)

wp_wiadomosci <- data_frame(links = links, bodies = bodies)

db_next <- "', '"

for (i in 1:nrow(wp_wiadomosci)) {
  dbGetQuery(db,
             paste0("INSERT INTO wp_wiadomosci (links, bodies) VALUES ('",
                    wp_wiadomosci$links[i], db_next,
                    wp_wiadomosci$bodies[i], "')"))
}

# dbDisconnect(db)

update_csv('wp_wiadomosci')
