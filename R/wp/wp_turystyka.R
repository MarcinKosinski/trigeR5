db <- dbConnect(drv = SQLite(), dbname = "data/wp.db")
#### TURYSTYKA ####
adress <- "http://turystyka.wp.pl/"
adresses <- adress %>%
  read_html() %>%
  html_nodes(css = "._1lXcfrU") %>%
  html_text %>%
  tolower() %>%
  gsub("[[:punct:]]", "", .) %>%
  gsub("[[:space:][:punct:]]", "-", .) %>%
  chartr("ąćęłńóśźż", "acelnoszz", .) %>%
  c("redakcja-poleca", "spa", "hotele", "gory", "nad-woda", "moda",
    "romatycznie", "kulinarne-szlaki", "w-podrozy", "magiczne-miejsca",
    "okazje", "fotografia", "wydarzenia", "na-sniegu") %>%
  paste0(adress, .)
adresses <- adresses[-12]

links <- pblapply(adresses, function(one_ad) {
  ad_html <- read_html(one_ad)
  ad_nodes <- html_nodes(ad_html, css = "script")

  ad_text <- ad_nodes %>%
    grep("turystyka.wp.pl", .) %>%
    ad_nodes[.] %>%
    as.character()

  biggest_list <- ad_text %>%
    stri_count_regex("turystyka.wp.pl") %>%
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

# db <- dbConnect(drv = SQLite(), dbname = "../data/wp.db")
db_links <- dbGetQuery(db, "SELECT links FROM wp_turystyka")
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

wp_turystyka <- data_frame(links = links, bodies = bodies) %>%
  filter(bodies != "Hmm... Nie ma takiej strony.")

db_next <- "', '"

for (i in 1:nrow(wp_turystyka)) {
  dbGetQuery(db,
             paste0("INSERT INTO wp_turystyka (links, bodies) VALUES ('",
                    wp_turystyka$links[i], db_next,
                    wp_turystyka$bodies[i], "')"))
}

dbDisconnect(db)

update_csv('wp_turystyka')
