# Install Libraries (ONLY FIRST TIME)

install.packages("tidyverse") 

# Import Libraries

library(tidyverse)

# Create a string

string1 <- "Hi, I am Vito!" # double quote

string2 <- 'Hi, I am Vito!' # single quote

# Create a sting with double quote

string3 <- 'Hi, I am "Vito"!'
string4 <- "Hi, I am \"Vito\"!" # with escape

# I don't want to see escape

writeLines(string4)

# Create a vector of strings

string_vector <- c("one", "two", "three")


# Functions on string -----------------------------------------------------

# Count the number of char in a string

str_count("Hi!")

str_count(c("one", "two", "four"))

# Combine String

str_c("Hi", "Vito!")

str_c("Hi", "Vito!", sep = " ") # separate with whitespace

# Combine string variable

name <- "Vito"

time_of_day <- "morning"

birthday <- FALSE

str_c(
  "Good ", time_of_day, " ", name,
  if (birthday) " and HAPPY BIRTHDAY",
  "."
)

# Extract part of a string

string <- c("Banana", "Apple", "Cherry")

str_sub(string, start = 3, end = 4)

str_sub(string, start = -3, end = -1) # negative numbers count backwards from end

# Change the text in lowercase

string <- c("VITO", "Filippo", "pietro")

str_to_lower(string)

# Change the text in uppercase

str_to_upper(string)

# Remove unuseful whitespace

dummy_string <- c("       Hi      ", " Vito ", " Filippo", "Pietro ")

str_trim(dummy_string)


# Basic Regular Expression ------------------------------------------------------

fruit <- c("Apple", "banana", "cherry", "pear", "watermelon", "ornage",
           "ananas")

# Detect the words that contain a specific string

str_view(fruit, "a") 

str_view_all(fruit, "a") # Process all string

str_view(fruit, "nana")

# Match Anaychar

str_view(fruit, "e.r")

# Match the START of the string

str_view(fruit, "^a")

str_view(str_to_lower(fruit), "^a")

str_view(fruit, "^.a")

# Match the END of the string

str_view(fruit, "a$")

str_view(fruit, "a.$")


# Regular Expression ------------------------------------------------------

apple <- c("apple", "apple pie", "apple cake", "apple_1", "apple_2")

fruit <- c("Apple", "banana", "cherry", "pear", "watermelon", "ornage",
           "ananas")

email <- c("giordano.vito94@gmail.com", "Vito.Giordano@phd.unipi.it", "VITO.GIORDANO@phd.unipi.it")

# Match any digit

str_view(apple, "\\d")

# Match white space

str_view(apple,"\\s")

# Match anythings in brackets [...]

str_view_all(fruit,"[abc]")

str_view_all(fruit,"[pes]")

# Match anythings except string in brackets [...]

str_view_all(fruit,"[^abc]")

str_view_all(fruit,"[^pes]")

# Match all alphabetic characters

str_view_all(email, "[a-z]")

str_view_all(email, "[A-Z]")

str_view_all(email, "[aA-zZ]")

str_view_all(email, "[^a-z]")


# Operator in Regex -------------------------------------------------------

# Or operator

str_view(c("grey", "gray"), "gr(e|a)y")

# 0 or 1 

x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVIII"

str_view(x, "CC?")

# 1 or more

str_view(x, "CC+")


# 0 or more

str_view_all(fruit, "na*")

str_view_all(fruit, "na*$")


# Function with Regex -----------------------------------------------------

# Determine if a character vector matches a pattern

str_detect(fruit, "nana$")

!str_detect(fruit, "nana$")

# Select the elements that match a pattern

str_subset(fruit, "^.a")

# Extract the exact matched pattern

str_extract(fruit, "^.a")

string <- "Hi, I am Vito Giordano. I know another Vito, but his name is Vito Manfredi"

str_extract(string, "Vito\\s\\w+")

str_extract_all(string, "Vito\\s\\w+")

# Replace a pattern with another string

str_replace(string, "Vito", "Pietro")

str_replace_all(string, "Vito", "Pietro")


# Application -------------------------------------------------------------

# Import Scopus Paper on Sustainable Manufacturing

sm_scopus <- read_csv("Data/sustainable_manufacturing.csv")

View(sm_scopus)

# Extract any words plus learning

sm_scopus %>% 
  mutate(learning = str_extract_all(abstract, "\\w+\\slearning")) %>%
  filter(!learning == "character(0)") %>% 
  View()

# Extract any words plus machine

sm_scopus %>% 
  mutate(machine = str_extract_all(abstract, "\\w+\\smachine")) %>%
  filter(!machine == "character(0)") %>% 
  View()

# Extract 3d plus any words

sm_scopus %>% 
  mutate(three_dimensional = str_extract_all(abstract, "3\\s?(d|D)\\s\\w+")) %>%
  filter(!three_dimensional == "character(0)") %>%
  View()

sm_scopus_3d <- sm_scopus %>% 
  mutate(abstract = str_to_lower(abstract)) %>% 
  mutate(three_dimensional = str_extract_all(abstract, "3\\s?d\\s\\w+")) %>%
  unnest(three_dimensional) %>% 
  filter(!is.na(three_dimensional)) 

# Most common words with 3d

sm_scopus_3d %>% 
  count(three_dimensional, sort = TRUE) %>%
  top_n(20) %>% 
  ggplot(aes(x = reorder(three_dimensional, n), y = n)) +
  geom_col() +
  coord_flip() +
  xlab("Three dimensional Word")


# Application with udpipe -------------------------------------------------

# Import table with Part-of-Speech tagging

sm_scopus_udpipe <- read_rds("Data/sm_scopus_udpipe.rds")

View(sm_scopus_udpipe)

# Select all verbs that start with "re"

sm_scopus_udpipe %>% 
  filter(upos == "VERB") %>% 
  filter(str_detect(lemma, "^re")) %>% 
  View()

# Show variaton of verb that start with "re" from 2018 to 2019

p <- sm_scopus_udpipe %>% 
  filter(upos == "VERB") %>% 
  filter(str_detect(lemma, "^re")) %>% 
  select(doc_id, lemma) %>% 
  left_join(sm_scopus %>% select(scopusID, year), by = c("doc_id" = "scopusID")) %>%
  filter(year %in% c("2018", "2019")) %>% 
  count(lemma, year) %>% 
  group_by(year) %>% 
  mutate(n = n/sum(n)) %>%
  ungroup() %>% 
  spread(key = year, value = n, fill = 0) %>% 
  ggplot(aes(x = `2018`, y = `2019`, text = lemma)) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  theme(legend.position="none")

# Use interactive plot

ggplotly(p)


