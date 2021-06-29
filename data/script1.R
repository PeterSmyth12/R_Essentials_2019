dir.create("data")
dir.create("data_output")
dir.create("fig_output")


download.file("https://ndownloader.figshare.com/files/11492171",
              "data/SAFI_clean.csv", mode = "wb")


#----------------------

3 + 5

12 / 7

area_hectares <- 1.0

area_hectares <- 1.0    # doesn't print anything
(area_hectares <- 1.0)  # putting parenthesis around the call prints the value of `area_hectares`

area_hectares         # and so does typing the name of the object

2.47 * area_hectares

area_hectaes <- 2.5
2.47 * area_hectares

area_acres <- 2.47 * area_hectares

area_hectares <- 50

area_hectares <- 1.0			# land area in hectares
area_acres <- area_hectares * 2.47	# convert to acres
area_acres				# print land area in acres.

# ---- Functions

a <- -9
b <- sqrt(a)
b

round(3.14159)

args(round)

?round

round(3.14159, digits = 2)

round(3.14159, 2)

round(digits = 2, x = 3.14159)

# ----  Vectors and data types

hh_members <- c(3, 7, 10, 6, 7, 10, 6, 7, 10, 6, 7, 10, 6, 7, 10, 6, 7, 10, 6, 7, 10, 6, 7, 10, 6, 7, 10, 6, 7, 10, 6, 7, 10, 6, 7, 10, 6, 7, 10, 6, 7, 10, 6)
hh_members

respondent_wall_type <- c("muddaub", "burntbricks", "sunbricks")
respondent_wall_type

length(hh_members)

length(respondent_wall_type)

class(hh_members)

class(respondent_wall_type)

str(hh_members)

str(respondent_wall_type)

possessions <- c("bicycle", "radio", "television")
possessions <- c(possessions, "mobile_phone") # add to the end of the vector
possessions <- c("car", possessions) # add to the beginning of the vector
possessions


# ---- Exercise

num
num_logical <- c(1, 2, 3, TRUE)
char_logical <- c("a", "b", "c", TRUE)
tricky <- c(1, 2, 3, "4")


# ---- Subsetting vectors

respondent_wall_type <- c("muddaub", "burntbricks", "sunbricks")
respondent_wall_type[2]

respondent_wall_type[c(3, 2)]

more_respondent_wall_type <- respondent_wall_type[c(1, 2, 3, 2, 1, 3)]
more_respondent_wall_type

# ------ Conditional subsetting

hh_members <- c(3, 7, 10, 6)
hh_members[c(TRUE, FALSE, TRUE, TRUE)]

hh_members > 5    # will return logicals with TRUE for the indices that meet the condition

## so we can use this to select only the values above 5
hh_members[hh_members > 5]

hh_members[hh_members < 4 | hh_members > 7]

hh_members[hh_members >= 7 & hh_members == 3]

possessions <- c("car", "bicycle", "radio", "television", "mobile_phone")
possessions[possessions == "car" | possessions == "bicycle"] # returns both car and bicycle

possessions %in% c("car", "bicycle")

possessions %in% c("car", "bicycle", "motorcycle", "truck", "boat", "bus")

possessions[possessions %in% c("car", "bicycle", "motorcycle", "truck", "boat", "bus")]

# ---- Missing data

rooms <- c(2, 1, 1, NA, 4)
mean(rooms)

max(rooms)

mean(rooms, na.rm = TRUE)

max(rooms, na.rm = TRUE)

## Extract those elements which are not missing values.
rooms[!is.na(rooms)]

## Count the number of missing values.
sum(is.na(rooms))

## Returns the object with incomplete cases removed. The returned object is an atomic vector of type `"numeric"` (or `"double"`).
na.omit(rooms)

## Extract those elements which are complete cases. The returned object is an atomic vector of type `"numeric"` (or `"double"`).
rooms[complete.cases(rooms)]

# -------------------------------   Starting with Data -----------------------------

library(tidyverse)
interviews <- read.csv("data/SAFI_clean.csv", na = "NULL")

interviews
## Try also
## View(interviews)
head(interviews)

class(interviews)

dim(interviews)
nrow(interviews)
ncol(interviews)
head(interviews)
tail(interviews)
names(interviews)

str(interviews)
summary(interviews)
glimpse(interviews)


# ----  Indexing and subsetting data frames  -----

## first element in the first column of the tibble
interviews[1, 1]
interviews[1, 6]

## first column of the tibble (as a vector)
interviews[[1]]

## first column of the tibble
interviews[1]

## first three elements in the 7th column of the tibble
interviews[1:3, 7]

## the 3rd row of the tibble
interviews[3, ]

## equivalent to head_interviews <- head(interviews)
head_interviews <- interviews[1:6, ]

interviews[, -1]          # The whole tibble, except the first column

interviews[-c(7:131), ]   # Equivalent to head(interviews)

interviews["village"]       # Result is a tibble
interviews[, "village"]     # Result is a tibble
interviews[["village"]]     # Result is a vector
interviews$village          # Result is a vector

# -------  Factors   -------

respondent_floor_type <- factor(c("earth", "cement", "cement", "earth"))

levels(respondent_floor_type)

nlevels(respondent_floor_type)

respondent_floor_type # current order

respondent_floor_type <- factor(respondent_floor_type, levels = c("earth", "cement"))
respondent_floor_type # after re-ordering

levels(respondent_floor_type)

levels(respondent_floor_type)[2] <- "brick"
levels(respondent_floor_type)

respondent_floor_type

respondent_floor_type_ordered <- factor(respondent_floor_type, ordered=TRUE)
respondent_floor_type_ordered # after setting as ordered factor

# -----  Converting factors  -----

as.character(respondent_floor_type)

year_fct <- factor(c(1990, 1983, 1977, 1998, 1990))
as.numeric(year_fct)                     # Wrong! And there is no warning...

as.numeric(as.character(year_fct))       # Works...

as.numeric(levels(year_fct))[year_fct]   # The recommended way.

## create a vector from the data frame column "memb_assoc"
memb_assoc <- interviews$memb_assoc
## convert it into a factor
memb_assoc <- as.factor(memb_assoc)
## let's see what it looks like
memb_assoc

## bar plot of the number of interview respondents who were
## members of irrigation association:
plot(memb_assoc)

## Let's recreate the vector from the data frame column "memb_assoc"
memb_assoc <- interviews$memb_assoc
## replace the missing data with "undetermined"
memb_assoc[is.na(memb_assoc)] <- "undetermined"
## convert it into a factor
memb_assoc <- as.factor(memb_assoc)
## let's see what it looks like
memb_assoc

## bar plot of the number of interview respondents who were
## members of irrigation association:
plot(memb_assoc)

# ----- Formatting Dates  -----

str(interviews)

library(lubridate)

dates <- interviews$interview_date
str(dates)


interviews$day <- day(dates)
interviews$month <- month(dates)
interviews$year <- year(dates)
interviews

char_dates <- c("7/31/2012","8/9/2014",'4/30/2106')
str(char_dates)

as_date(char_dates, format = "%m/%d/%y")

as_date(char_dates, format = "%d/%m/%y")

mdy(char_dates)

# -----  Data Wrangling with dplyr and tidyr ------------------

## load the tidyverse
library(tidyverse)

interviews <- read_csv("data/SAFI_clean.csv", na = "NULL")

## inspect the data
interviews

## preview the data
# View(interviews)

# to select columns throughout the dataframe
select(interviews, village, no_membrs, months_lack_food)
# to select a series of connected columns
select(interviews, village:respondent_wall_type)

# one condition
filter(interviews, village == "Chirodzo")

# multiple conditions
filter(interviews, village == "Chirodzo", rooms > 1, no_meals > 2)

interviews2 <- filter(interviews, village == "Chirodzo")
interviews_ch <- select(interviews2, village:respondent_wall_type)

interviews_ch <- select(filter(interviews, village == "Chirodzo"),
                        village:respondent_wall_type)

interviews %>%
  filter(village == "Chirodzo") %>%
  select(village:respondent_wall_type)

interviews_ch <- interviews %>%
  filter(village == "Chirodzo") %>%
  select(village:respondent_wall_type)

interviews_ch

interviews %>%
  mutate(people_per_room = no_membrs / rooms)

interviews %>%
  filter(!is.na(memb_assoc)) %>%
  mutate(people_per_room = no_membrs / rooms)

# ----  Split-apply-combine data analysis and the summarize() function -----

interviews %>%
  group_by(village) %>%
  summarize(mean_no_membrs = mean(no_membrs))

interviews %>%
  group_by(village, memb_assoc) %>%
  summarize(mean_no_membrs = mean(no_membrs))

interviews %>%
  group_by(village, memb_assoc) %>%
  summarize(mean_no_membrs = mean(no_membrs)) %>%
  ungroup()

interviews %>%
  filter(!is.na(memb_assoc)) %>%
  group_by(village, memb_assoc) %>%
  summarize(mean_no_membrs = mean(no_membrs))

interviews %>%
  filter(!is.na(memb_assoc)) %>%
  group_by(village, memb_assoc) %>%
  summarize(mean_no_membrs = mean(no_membrs),
            min_membrs = min(no_membrs))

interviews %>%
  filter(!is.na(memb_assoc)) %>%
  group_by(village, memb_assoc) %>%
  summarize(mean_no_membrs = mean(no_membrs),
            min_membrs = min(no_membrs)) %>%
  arrange(min_membrs)

interviews %>%
  filter(!is.na(memb_assoc)) %>%
  group_by(village, memb_assoc) %>%
  summarize(mean_no_membrs = mean(no_membrs),
            min_membrs = min(no_membrs)) %>%
  arrange(desc(min_membrs))

interviews %>%
  count(village)

interviews %>%
  count(village, sort = TRUE)

# ---- Reshaping with pivot_wider() and pivot_longer() ---

interviews %>%
  select(key_ID, village, interview_date, instanceID)

interviews %>%
  filter(village == "Chirodzo") %>%
  select(key_ID, village, interview_date, instanceID) %>%
  sample_n(size = 10)

interviews_wide <- interviews %>%
  mutate(wall_type_logical = TRUE) %>%
  pivot_wider(names_from = respondent_wall_type,
              values_from = wall_type_logical,
              values_fill = list(wall_type_logical = FALSE))

interviews_long <- interviews_wide %>%
  pivot_longer(cols = muddaub:cement,
               names_to = "respondent_wall_type",
               values_to = "wall_type_logical")

interviews_long <- interviews_wide %>%
  pivot_longer(cols = c(burntbricks, cement, muddaub, sunbricks),
               names_to = "respondent_wall_type",
               values_to = "wall_type_logical") %>%
  filter(wall_type_logical) %>%
  select(-wall_type_logical)

interviews_items_owned <- interviews %>%
  separate_rows(items_owned, sep = ";") %>%
  replace_na(list(items_owned = "no_listed_items")) %>%
  mutate(items_owned_logical = TRUE) %>%
  pivot_wider(names_from = items_owned,
              values_from = items_owned_logical,
              values_fill = list(items_owned_logical = FALSE))

nrow(interviews_items_owned)

interviews_items_owned %>%
  filter(bicycle) %>%
  group_by(village) %>%
  count(bicycle)

interviews_items_owned %>%
  mutate(number_items = rowSums(select(., bicycle:car))) %>%
  group_by(village) %>%
  summarize(mean_items = mean(number_items))

# ------------------------------------------------------------------

interviews_plotting <- interviews %>%
  ## pivot wider by items_owned
  separate_rows(items_owned, sep = ";") %>%
  ## if there were no items listed, changing NA to no_listed_items
  replace_na(list(items_owned = "no_listed_items")) %>%
  mutate(items_owned_logical = TRUE) %>%
  pivot_wider(names_from = items_owned,
              values_from = items_owned_logical,
              values_fill = list(items_owned_logical = FALSE)) %>%
  ## pivot wider by months_lack_food
  separate_rows(months_lack_food, sep = ";") %>%
  mutate(months_lack_food_logical = TRUE) %>%
  pivot_wider(names_from = months_lack_food,
              values_from = months_lack_food_logical,
              values_fill = list(months_lack_food_logical = FALSE)) %>%
  ## add some summary columns
  mutate(number_months_lack_food = rowSums(select(., Jan:May))) %>%
  mutate(number_items = rowSums(select(., bicycle:car)))

write_csv (interviews_plotting, "data_output/interviews_plotting.csv")

# ------ Visualization

interviews_wide <- interviews %>%
  mutate(wall_type_logical = TRUE) %>%
  pivot_wider(names_from = respondent_wall_type,
              values_from = wall_type_logical,
              values_fill = list(wall_type_logical = FALSE))




interviews %>%
  separate_rows(months_lack_food, sep = ";") %>%
  mutate(months_lack_food_logical  = TRUE) 


interviews_months_lack_food <- interviews %>%
  separate_rows(months_lack_food, sep = ";") %>%
  mutate(months_lack_food_logical  = TRUE) %>%
  pivot_wider(names_from = months_lack_food,
              values_from = months_lack_food_logical,
              values_fill = list(months_lack_food_logical = FALSE))


interviews_months_lack_food <- interviews %>%
  separate_rows(months_lack_food, sep = ";") %>%
  mutate(months_lack_food_logical  = TRUE) %>%
  pivot_wider(names_from = months_lack_food,
              values_from = months_lack_food_logical,
              values_fill = list(months_lack_food_logical = FALSE))

interviews_months_lack_food %>%
  mutate(number_months = rowSums(select(., Jan:May))) %>%
  group_by(memb_assoc) %>%
  summarize(mean_months = mean(number_months))



interviews_new <- interviews %>%
  ## pivot wider by items_owned
  separate_rows(items_owned, sep = ";") %>%
  ## if there were no items listed, changing NA to no_listed_items
  replace_na(list(items_owned = "no_listed_items")) %>%
  mutate(items_owned_logical = TRUE) %>%
  pivot_wider(names_from = items_owned,
              values_from = items_owned_logical,
              values_fill = list(items_owned_logical = FALSE)) %>%
  ## pivot wider by months_lack_food
  separate_rows(months_lack_food, sep = ";") %>%
  mutate(months_lack_food_logical = TRUE) %>%
  pivot_wider(names_from = months_lack_food,
              values_from = months_lack_food_logical,
              values_fill = list(months_lack_food_logical = FALSE)) %>%
  ## add some summary columns
  mutate(number_months_lack_food = rowSums(select(., Jan:May))) %>%
  mutate(number_items = rowSums(select(., bicycle:car)))