# Install Libraries (ONLY FIRST TIME)

install.packages("tidyverse") 
install.packages("tidytext") 
install.packages("udpipe")
install.packages("plotly")
install.packages("scales")
install.packages("igraph")
install.packages("ggraph")

# Import Libraries

library(tidyverse) 
library(tidytext) # Process Text
library(udpipe) # PoS Tagging
library(plotly) # Interactive Plot
library(scales) # Rescale the axis of a plot
library(igraph) # Work with graph
library(ggraph) # Plot a graph

# Tibble vs Data Frame ----------------------------------------------------

# Import Scopus Article of Sustainable Manfacturing

sm_scopus <- read_csv("Data/sustainable_manufacturing.csv")


# Tidy Format -------------------------------------------------------------

View(sm_scopus)

sm_scopus$abstract[1] %>% 
  write("example_abstract.txt")

# How can we define tidy text format?

# Trasform text in tidy format

sm_scopus %>% 
  select(scopusID, abstract) %>% 
  unnest_tokens(output = word, input = abstract, token = "words") %>% 
  View()

sm_scopus_tidy <- sm_scopus %>% 
  select(scopusID, abstract) %>% 
  unnest_tokens(output = word, input = abstract, token = "words")


# What are the most common words?

sm_scopus_tidy %>% 
  count(word, sort = TRUE) %>% 
  View()

# How remove noise words (stop words)?

View(stop_words)

# What is stop_words? 

?stop_words

# Remove words in stop_words

sm_scopus_tidy_clean <- sm_scopus_tidy %>% 
  filter(!word %in% stop_words$word)

View(sm_scopus_tidy_clean)

# What are the most common words with out noise?

sm_scopus_count <- sm_scopus_tidy_clean %>% 
  count(word, sort = TRUE) 

View(sm_scopus_count)

# What are the problems? 

sm_scopus_count %>%
  top_n(20) %>% 
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col() +
  coord_flip() +
  xlab("Words")

my_stopwords <- c("paper", "article", "study")

sm_scopus_tidy_clean <- sm_scopus_tidy_clean %>% 
  filter(!word %in% my_stopwords)

sm_scopus_tidy_clean %>% 
  count(word) %>% 
  top_n(20) %>% 
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col() +
  coord_flip()

# N-grams Analsyis --------------------------------------------------------

# Observing variable is bi-grams

# Example of bi-grams

"My name is Vito." %>% 
  as_tibble() %>% 
  unnest_tokens(output = bi_grams, input = value, token = "ngrams", n = 2) %>% 
  View()

sm_scopus_bi_grams <- sm_scopus %>% 
  unnest_tokens(output = bi_grams, input = abstract, token = "ngrams", n = 2)

View(sm_scopus_bi_grams %>% select(scopusID, bi_grams))

# Most common bi_grams

sm_scopus_bi_grams %>% 
  count(bi_grams, sort = TRUE) %>%
  View()

# Clean bi_grams

sm_bi_grams_clean <- sm_scopus_bi_grams %>%
  mutate(bi_grams_1 = bi_grams) %>% 
  separate(bi_grams_1, into = c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word) %>% 
  select(scopusID, word1, word2, bi_grams)
  

# Most common bi-grams

sm_bi_grams_clean %>% 
  count(bi_grams) %>% 
  top_n(20) %>% 
  ggplot(aes(x = reorder(bi_grams, n), y = n)) +
  geom_col() +
  coord_flip() +
  xlab("Bi-grams")

# Create a Graph of bigram

bigram_count <- sm_bi_grams_clean %>% 
  
  # Filter Na value
  filter(!is.na(word1)) %>% 
  filter(!is.na(word2)) %>% 
  count(word1, word2)

bigram_graph <- bigram_count %>% 
  graph_from_data_frame()
  
# For reproduce the results we set a random seed (example 2016)

set.seed(00011)

# Create an arrow for the graph

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

# Plot the graph of bigrams

bigram_graph %>% 
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

# Why doesn't it work?

# Frequency distribution of the occurences in bigrams

bigram_count %>% 
  sample_n(10000) %>% 
  arrange(desc(n)) %>%
  mutate(rank = row_number()) %>% 
  ggplot(aes(x = rank, y = n)) +
  geom_point() 
  scale_x_log10() +
  scale_y_log10()

View(bigram_count)

bigram_graph <- bigram_count %>% 
  
  # filter row for n
  filter(between(n, 150, 500)) %>% 
  graph_from_data_frame()

# For reproduce the results we set a random seed (example 2016)

set.seed(1503)

# Create an arrow for the graph

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

# Plot the graph of bigrams filtered

bigram_graph %>% 
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


# Udpipe Annotate ---------------------------------------------------------

# Downolad the model from Web (ONLY FIRST TIME)

udpipe_download_model(language = "english")

# Load Udpipe Model

model <- udpipe_load_model("Data/english-ud-2.0-170801.udpipe")

# Select 2018 and 2019 year

sm_scopus <- sm_scopus %>% 
  filter(year %in% c("2018", "2019"))

# FOR THIS PART THE CODE IS VERY SLOW (ABOUT 1 HOUR OF WAITING)

# Part-of-Speech tagging and Dependency Parsing

sm_scopus_udpipe <- udpipe_annotate(object = model, 
                                            x = sm_scopus$abstract,
                                            doc_id = sm_scopus$scopusID) %>% 
  as_tibble()

# Save Udpipe Table

write_rds(sm_scopus_udpipe, "Data/sm_scopus_udpipe.rds")

# What are the most common Adj?

sm_scopus_udpipe %>% 
  filter(upos == "ADJ") %>% 
  count(lemma, sort = TRUE) %>% 
  View()

# Show variaton of adjectives from 2018 to 2019

p <- sm_scopus_udpipe %>% 
  filter(upos == "ADJ") %>%
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
