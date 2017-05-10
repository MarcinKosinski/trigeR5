library(dplyr)

starwars
glimpse(starwars)
head(starwars)
tail(starwars)
summary(starwars)

# 1 zadanie z krzyzem, 2 zadania z gwiazdka, 3 zadania trudne i 5 zadań podstawowywch?

sw <- starwars

### EASY ###

# 1. Take a glimpse of first 5 of df's last 30 records
# in classic way and using pipes.
glimpse(head(tail(sw, 15), 5))
sw %>% tail(15) %>% head(5) %>% glimpse()
sw %>%
  tail(15) %>%
  head(5) %>%
  glimpse()

# 2. Select out nested columns
sw %>%
  select(-films, -vehicles, -starships)
sw %>%
  select(-c(films, vehicles, starships))

# 3. Filter out non humans then filter those with blue or green eyes.
sw %>%
  filter(species == "Human") %>%
  filter(eye_color %in% c("hazel", "brown"))

# 4. Calculate members for all species then the weight for all species.
sw %>%
  group_by(species) %>%
  summarise(n())
sw$species %>% table()
sw %>%
  group_by(species) %>%
  summarise(mean_mass = mean(mass))
sw %>%
  group_by(species) %>%
  summarise(mean_mass = mean(mass, na.rm = TRUE))

# 5. Arrange from oldest to youngest, assume that birth_year (BBY – it's out BC)
# is actually age.
sw %>%
  rename(age = birth_year) %>%
  arrange(desc(age))

### HARD ###

# 1. What if we had 10k columns?
sw %>%
  select_if(is.list)
# sw %>%
#   select_if(is.list, `!`)
sw %>%
  select_if(function(column) {!is.list(column)})

# 2. Change hair, skin and eye colors semicolon separated format
sw$eye_color %>% table()
"Leia Organa" %>%
  gsub("Organa", "Solo")
"Leia Organa" %>%
  gsub("Organa", "Solo", .)
sw %>%
  mutate(hair_color = gsub(", ", ";", hair_color),
         skin_color = gsub(", ", ";"), skin_color,
         eye_color = gsub(", ", ";"), eye_color)
sw %>%
  mutate_at(vars(hair_color, skin_color, eye_color),
            funs(gsub(., ", ", ";")))
sw %>%
  mutate_at(vars(hair_color, skin_color, eye_color),
            gsub, pattern = ", ", replacement = ";")
sw %>%
  mutate_at(vars(contains("color")),
            gsub, pattern = ", ", replacement = ";")

# 3. Check how many droids and how many humans – at least 50j heavy – come
# from Naboo or Tatooine
sw %>%
  select(mass, species, homeworld) %>%
  filter(mass >= 50 & species %in% c("Droid", "Human") & homeworld %in% c("Naboo", "Tatooine"))
sw %>%
  select(mass, species, homeworld) %>%
  filter(mass >= 50,
         species %in% c("Droid", "Human"),
         homeworld %in% c("Naboo", "Tatooine")) %>%
  group_by(species, homeworld) %>%
  summarise(citizens = n()) %>%
  arrange(desc(citizens))

### * ###

# 1. 
# 2.

### CROSS ###

# 1. 