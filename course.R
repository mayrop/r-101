# Intro to R
# Use command + Enter to run each line in the console
#------------------------------------------------
# Thinking Vectorized

##  1  2  3  4  5  6  7  8  9 10
x <- 1:10
x
seq(1, 10)

## 1   4   9  16  25  36  49  64  81 100
(1:10) ** 2

## 1    2    4    8   16   32   64  128  256  512 1024
2 ** (0:10)

## 0    1    3    7   15   31   63  127  255  511 1023
(2 ** (0:10)) - 1

## 1 1 1 2 2 2 3 3 3
rep(1:3, each=3)

## 1 2 3 1 2 3 1 2 3
rep(1:3, times=3)

## 1 3 5 7 9
seq(from=1, to=10, by=2)

## a list of items from 1 to 10 but only obtain 5 items
seq(from=1, to=10, length.out=5)

#------------------------------------------------

# Exploring Data Using Base R
df <- iris

# See structure
str(df)

# See dimensions
dim(df)
nrow(df)
ncol(df)

# Getting Species column
df[ , "Species"]

# Getting 2 columns
df[, c("Sepal.Length", "Species")]
df[, 1:2]

# Check first lines
head(df[, 1:2])

# Check last lines
tail(df[, 1:2])

# Get the columns with "Width"
head(df[, grepl("Width", colnames(df))])

# Get the total for all columns except last one
colSums(df[, -5])
# Or you can do
colSums(df[, 1:4])

# Get rows where Sepal.Length > 5
df$Sepal.Length > 5 # this will return a logical vector
df[df$Sepal.Length > 5, ]

# Get rows where Species == versicolor
df[df$Species == "versicolor", ]

# How to order stuff
x <- c(5, 15, 20, 1, -12, 100)
# U can do
sort(x)

# What if you don't care about the sign?
order(x)
x[order(x)]
abs(x)
x[order(abs(x), decreasing = TRUE)]

# Sort our dataset by Sepal.Length
df[order(df$Sepal.Length, decreasing = TRUE), ]

# Add new column with the ID
# First, we need to assign a vector that goes from 1 to the # of rows
1:nrow(df)
# Then assign it!
df$id <- 1:nrow(df)

# Let's assign other with the area!
df$Sepal.Area <- df$Sepal.Length * df$Sepal.Width

# Let's do some counts
table(df$Species)

# Let's get proportions
prop.table(table(df$Species))

# Now let's get the mean for each column per group
lapply(split(df[, 1:4], df$Species), colMeans)

#------------------------------------------------
# Exploring Data Using Tidyverse
library(tidyverse)

df %>%
  filter(
    Sepal.Length > 5
  ) %>%
  group_by(Species) %>%
  summarise(
    sepal_mean = mean(Sepal.Length)
  )
# Could also be written preppending the package before function name
df %>%
  dplyr::filter(
    Sepal.Length > 5
  ) %>%
  dplyr::group_by(Species) %>%
  dplyr::summarise(
    sepal_mean = mean(Sepal.Length)
  )

# Add new column before some column
df %>%
  mutate(
    Sepal.Area = Sepal.Width * Sepal.Length, .before=Species, .keep="used"
  ) %>%
  arrange(-Sepal.Area) %>%
  head()

df %>%
  mutate(
    id = 1:n(),
    Sepal.Area = Sepal.Width * Sepal.Length + 1
  ) %>%
  arrange(-Sepal.Area) %>%
  head()

# Add a row number per group
df %>%
  group_by(Species) %>%
  mutate(
    species_id = 1:n()
  ) %>%
  View()

# Group by one factor
starwars %>%
  count(sex, sort = TRUE)

# Group by two factors
starwars %>% 
  count(gender, species, sort = TRUE)


#------------------------------------------------
# Read/Write CSV
# See: https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-05-26/readme.md
all_drinks <- read.csv("https://raw.githubusercontent.com/shortpoet/Final-Project/master/all_drinks.csv")
# Save it locally
write.csv(all_drinks, "all_drinks.csv")
# Then you can read it from local
# all_drinks <- read.csv("all_drinks.csv")

#------------------------------------------------

library(janitor)

# Change column names
colnames(all_drinks) # not pretty!
colnames(all_drinks) <- janitor::make_clean_names(colnames(all_drinks))
colnames(all_drinks) <- gsub("str_(.*)", "\\1", colnames(all_drinks))
colnames(all_drinks) 

# Select only certain columns
drinks <- all_drinks %>%
  select(
    drink,
    alcoholic,
    category,
    glass
  )

head(drinks)

# What are the most common category for cocktails
drinks %>%
  group_by(category, alcoholic) %>%
  summarise(total = n()) %>%
  ungroup() %>%
  filter(alcoholic != "") %>%
  pivot_wider(
    names_from = alcoholic,
    values_from = total
  )

# Let's plot it!
ggplot(drinks) + 
  geom_bar(aes(y = category)) # this is not pretty!

# this is not pretty either
drinks %>%
  count(category, sort=TRUE) %>%
  ggplot(aes(category, n)) +
  geom_col() 

# Getting better
drinks %>%
  count(category, sort=TRUE) %>%
  mutate(
    category = fct_reorder(category, n, sum)
  ) %>%  
  ggplot(aes(category, n)) +
  geom_col() 

# Flip axis
drinks %>%
  count(category, sort=TRUE) %>%
  mutate(
    category = fct_reorder(category, n, sum)
  ) %>%  
  ggplot(aes(category, n)) +
  geom_col() +
  coord_flip()

# Let's group some categories and change color!
drinks %>%
  count(category, sort=TRUE) %>%
  mutate(
    category = fct_lump(category, 4, w = n, other_level="Any other"),
    category = fct_reorder(category, n, sum)
  ) %>%
  ggplot(aes(category, n)) +
  geom_col(fill="#e2231a") +
  coord_flip()

# How many alcoholic drinks do we have in the dataset
drinks %>%
  group_by(alcoholic) %>%
  summarise(total = n())

# Let's see how many drinks are Alcoholic, and how many of them start with A!
drinks %>%
  filter(alcoholic == "Alcoholic",
         grepl("^A", drink)) %>%
  select(drink, glass)

# Group drinks by category and glass
# Then, get only the first glasses for each category
drinks %>%
  count(category, glass, sort=TRUE) %>% 
  group_by(category) %>%
  slice_head(n=3) %>%
  arrange(-n)
  
# Group drinks by category and glass
# Then, get only the first glasses for each category
# Approach # 2
drinks %>%
  count(category, glass, sort=TRUE) %>% 
  group_by(category) %>%
  arrange(-n) %>%
  mutate(
    row_n = 1:n()
  ) %>%
  filter(
    row_n < 4
  ) %>%
  arrange(category, -n)

# Which are the most popular ingredients
drinks <- all_drinks %>%
  select(
    drink,
    alcoholic,
    category,
    glass,
    contains("ingred")
  )

# We need to convert dataset into a tidy format
# See: https://vita.had.co.nz/papers/tidy-data.pdf
drinks %>%
  pivot_longer(cols = starts_with("ingred"), names_to="ingredient") %>%
  filter(
    value != "",
    !is.na(value)
  ) %>%
  extract(ingredient, "ingredient_number", "(\\d+)", remove=FALSE) %>%
  count(value, sort=TRUE)

#------------------------------------------------

# How todo HTML parsing
states <- read_html("https://en.wikipedia.org/wiki/List_of_states_of_Mexico") %>%
  html_nodes("table") %>%
  .[1] %>%
  html_table(fill = TRUE)

states <- states[[1]]

#------------------------------------------------
# How to do debugging
test_here <- function(df) {
  print(df)
  browser()
  mean(df)
  x <- 1:20
}
test_here(iris)
