library(dplyr)

starwars # /show/starwars.rda
glimpse(starwars)
head(starwars)
tail(starwars)
summary(starwars)

sw <- starwars

# 1 -----------------------------------------------------------------------
# Take a glimpse of first 5 of df's last 30 records
# in classic way and using pipes.
sw %>% tail(30) %>% head(5)

# 2 -----------------------------------------------------------------------
# Select out nested columns (their class is list).
sw_withoutNested <- select(starwars, -c(vehicles, starships, films))
sw %>% select(-vehicles,-starships,-films)
# 3 -----------------------------------------------------------------------
# Filter out non humans then filter those with blue or green eyes.
sw%>% filter(species != 'Human', eye_color %in% c('blue','green'))
# 4 -----------------------------------------------------------------------
# Calculate members for all species then the weight for all species.
sw %>% group_by(species) %>% summarise(n=n(), weight=sum(mass))
# 5 -----------------------------------------------------------------------
# Arrange from oldest to youngest, assume that birth_year (BBY – it's out BC)
# is actually age. Change the name of a birth_year to age.

sw %>%
  rename(wiek=birth_year) %>%
  arrange(desc(wiek))

# 6 -----------------------------------------------------------------------
# What if we had 10k columns? Select variables of class list. Don't use specific names of columns.

sw %>%
  select_if(is.list)

# 7 -----------------------------------------------------------------------
# Change hair, skin and eye colors semicolon separated format
sw %>% 
  mutate(eye_color=gsub(",",";",eye_color),
         hair_color=gsub(",",";",hair_color),
         skin_color=gsub(",",";",skin_color))

sw %>%
  mutate_at(vars(contains("color")),
            gsub, pattern=",", replacement=";")
# 8 -----------------------------------------------------------------------
# Check how many droids and how many humans – at least 50j heavy – come
# from Naboo or Tatooine
sw %>% filter(species %in% c('Human','Droid'), mass>=50,homeworld %in% c('Tatooine','Naboo')) %>% count(species)
# 9 -----------------------------------------------------------------------
# Create a linear model (lm function) within each eye_color, for which the dependent variable
# is a birth_year, and the explanatory variables is mass and height. For each model extract the R^2
# statistics and prepare final output where there are 2 column: eye_color and R^2.

library(broom)
lm1 <-sw %>%
  select(birth_year, eye_color, mass, height) %>%
  group_by(eye_color)%>%
  na.omit() %>%
  do(glance(lm(birth_year~mass, .)))# NA chyba sie krzacza w lm()



modele_lm <-sw %>%
  select(birth_year, eye_color, mass, height) %>%
  group_by(eye_color)%>%
  na.omit() %>%
  do(modele = lm(birth_year~mass, .)) %>%
  do(r_2 = .$modele %>% summary() %>% .$r.squared) %>%
  select(r_2) %>%
  unlist
  




 
 View(lm1)
  
# group_by() %>% do()

# 10 -----------------------------------------------------------------------
# Create a function that takes the name of the continuous variables as a parameter
# and uses that name in the `filter` statement to filter rows that has values less than X
# which is a second argument of this function. 

# 11 -----------------------------------------------------------------------
# For each homeworld extract with 3 rows with the highest mass and for that dataset arrange by the sqrt(|1-mass^2|) variable.
sw %>% group_by(homeworld) %>% top_n(3,mass) %>% mutate(nm = sqrt(abs(1-mass^2))) %>% 
  arrange(nm) %>% select(nm,mass,height)

# 12 ----------------------------------------------------------------------
# Select columns from gender to eye_color in alphabetical order, then in the order they occure in the dataset.


# 13 ----------------------------------------------------------------------
# Within species and homeworld calculate the percentage ratio of observations that are below and under mean height of 'female's from 'Naboo'.
