db <- dbConnect(drv = SQLite(), dbname = "data/wp.db")
#### DOM ####
adress <- "http://dom.wp.pl/"
adresses <- adress %>%
  read_html() %>%
  html_nodes(css = "._1lXcfrU") %>%
  html_text %>%
  tolower() %>%
  gsub("[[:punct:]]", "", .) %>%
  gsub("[[:space:][:punct:]]", "-", .) %>%
  chartr("ąćęłńóśźż", "acelnoszz", .) %>%
  paste0(adress, .)
adresses[8] <- "http://dom.wp.pl/dekoracje-wnetrz"

links <- pblapply(adresses, function(one_ad) {
  ad_html <- read_html(one_ad)
  ad_nodes <- html_nodes(ad_html, css = "script")
  
  ad_text <- ad_nodes %>%
    grep("dom.wp.pl", .) %>%
    ad_nodes[.] %>%
    as.character()
  
  biggest_list <- ad_text %>%
    stri_count_regex("dom.wp.pl") %>%
    which.max()
  
  big_links <- biggest_list %>%
    ad_text[.] %>%
    stri_extract_all_regex("[[:alnum:]]+-[[:alnum:]-]+[[:digit:]]+[ag]")
  
  big_links
}) %>%
  unlist() %>%
  unique %>%
  paste0(adress, .)

# art_links <- links %>%
#   grep("a$", ., value = TRUE)
# gal_links <- links %>%
#   grep("g$", ., value = TRUE)

# Filter out galleries
links <- links %>%
  grep("a$", ., value = TRUE)

#db <- dbConnect(drv = SQLite(), dbname = "../data/wp.db")
db_links <- dbGetQuery(db, "SELECT links FROM wp_dom")
links <- setdiff(links, db_links$links)

bodies <- pblapply(links, function(link) {
  tryCatch(link %>%
             read_html() %>%
             html_nodes(css = "p , ._1HGmjUl , ._1xAmRvR") %>%
             html_text() %>%
             paste0(collapse = " "),
           error = function(e) {
             "Hmm... Nie ma takiej strony."
           })
}) %>%
  unlist() %>%
  gsub("'", "''", .)

wp_dom <- data_frame(links = links, bodies = bodies) %>%
  filter(bodies != "Hmm... Nie ma takiej strony.")

db_next <- "', '"

for (i in 1:nrow(wp_dom)) {
  dbGetQuery(db,
             paste0("INSERT INTO wp_dom (links, bodies) VALUES ('",
                    wp_dom$links[i], db_next,
                    wp_dom$bodies[i], "')"))
}
dbDisconnect(db)
# dbDisconnect(db)
update_csv('wp_dom')
