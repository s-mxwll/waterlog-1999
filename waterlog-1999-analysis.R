# Text and sentiment analysis of the wild swimming classic Waterlog (1999) by Roger Deakin
# s-mxwll
# 2021

# Load packages:
library(glue)
library(rvest)
library(scales)
library(tidytext)
library(tidyverse)

# Set theme:
theme_set(theme_minimal(base_family = "Poppins",
                        base_size = 11) +
            theme(plot.title = element_text(face = "bold")))

# Create set caption and word count (n) label:
caption <- expression(paste("Source: ", italic("Waterlog"), " (Roger Deakin, 1999)"))
n_label <- expression(italic("n"))

# DATA ENTRY --------------------------------------------------------------

# Read in book:
text_df <- read_lines("book/waterlog-1999.txt")

# Make tibble and delete page and paragraph breaks:
waterlog_raw <- tibble(text = text_df) %>% 
  na_if("") %>% 
  filter(!is.na(text))

# Create line and chapter number variable:
waterlog_df <- waterlog_raw %>% 
  mutate(line_number = row_number(),
         chapter_number = cumsum(str_detect(text,
                                            regex("^CHAPTER [\\d]", # search for capitalised chapter followed by number
                                                  ignore_case = TRUE))))

# Tokenise text into individual words:
waterlog_tidy <- waterlog_df %>% 
  unnest_tokens(word, text) %>% 
  filter(!str_detect(word, "[\\d]")) # delete numbers (note the word chapter remains)

# Create tibble containing chapter title info:
chapter_info <- tribble(~chapter_number, ~chapter_title,
                        1, "The Moat",
                        2, "I-Spy at the Seaside",
                        3, "Lords of the Fly",
                        4, "A People's River",
                        5, "Swimming with Eels",
                        6, "At Swim-Two-Birds",
                        7, "Tiderips and Moonbeams",
                        8, "Borrow and Thoreau",
                        9, "The Lost Pools of the Malverns",
                        10, "Tribal Swimming",
                        11, "Salmon-Runs",
                        12, "The Red River",
                        13, "Crossing the Fowey",
                        14, "The Blandford Bomber",
                        15, "A Small World",
                        16, "Extinctions",
                        17, "The Wash",
                        18, "Natando Virtus",
                        19, "An Encounter with Naiads",
                        20, "Swimming with Angels",
                        21, "A Descent into Hell Gill",
                        22, "Hot and Cold",
                        23, "Orwell's Whirlpool",
                        24, "A Castle in the Air",
                        25, "The Oxbow",
                        26, "Swallow Dives",
                        27, "The Jaywick Papers",
                        28, "Great Expectations",
                        29, "Channel Swimming",
                        30, "Water Levels",
                        31, "A Mill-Race",
                        32, "Pengiun Pools",
                        33, "Steaming",
                        34, "The Walberswick Shiverers",
                        35, "On Ice",
                        36, "To the Sea")

# Using chapter number, join chapter title info to Waterlog words:
waterlog_tidy <- waterlog_tidy %>% 
  left_join(chapter_info, by = "chapter_number")

# TEXT ANALYSIS -----------------------------------------------------------

# Book word count:
waterlog_tidy %>% 
  summarise(n = n())

# Chapter word count:
waterlog_tidy %>% 
  group_by(chapter_number) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(chapter_number = as.character(chapter_number),
         chapter_number = fct_reorder(chapter_number, n)) %>% 
  ggplot(aes(n, chapter_number)) + 
  geom_col(fill = "#006994") +
  labs(title = "The longest chapters include swims in the River Test and River Fowey",
       subtitle = "Number of words in each chapter",
       x = n_label,
       y = "Chapter",
       caption = caption)

# With chapter title:
waterlog_tidy %>% 
  group_by(chapter_number, chapter_title) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(chapter_combined = glue("{chapter_title} ({chapter_number})"),
         chapter_combined = fct_reorder(chapter_combined, n)) %>% 
  ggplot(aes(n, chapter_combined)) + 
  geom_col(fill = "#006994") +
  labs(title = "The longest chapters include swims in the River Test and River Fowey",
       subtitle = "Number of words in each chapter",
       x = n_label,
       y = NULL,
       caption = caption)

# Most common words:
waterlog_tidy %>% 
  count(word, sort = TRUE) %>% 
  top_n(20) %>% 
  mutate(is_water = case_when(word == "water" ~ TRUE,
                              word != "water" ~ FALSE)) %>% 
  mutate(word = fct_reorder(word, n)) %>% 
  ggplot(aes(n, word, fill = is_water)) +
  geom_col() +
  scale_fill_manual(values = c("#006994", "#800000")) +
  labs(title = "With one exception, the most common words are stop words",
       subtitle = "Top 20 most common words",
       x = n_label,
       y = "",
       caption = caption) +
  theme(legend.position = "none")

# What are the most common words?
waterlog_tidy %>% 
  anti_join(stop_words, by = "word") %>% 
  count(word, sort = TRUE) %>% 
  top_n(20) %>% 
  mutate(word = fct_reorder(word, n)) %>% 
  ggplot(aes(n, word)) +
  geom_col(fill = "#006994") +
  labs(title = "Unsurprisingly in a book about wild swimming, water is the most common word",
       subtitle = "Top 20 most common words after deleting stop words",
       x = n_label,
       y = NULL,
       caption = caption)

# What are the least common words?
waterlog_tidy %>% 
  anti_join(stop_words, by = "word") %>% 
  count(word, sort = TRUE) %>% 
  top_n(-20) %>% 
  sample_n(20) # here are a random sample of the least common words (all mentioned only once)

# How many times is water mentioned?
waterlog_tidy %>% 
  filter(word == "water") %>% 
  summarise(n = n())

# Which chapters are the wettest?
waterlog_tidy %>% 
  filter(word == "water") %>% 
  group_by(chapter_number) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  ggplot(aes(chapter_number, n)) +
  geom_col(fill = "#006994") +
  labs(title = "Which chapters are the wettest?",
       subtitle = "Number of times water appears in each chapter",
       x = "Chapter",
       y = "n(water)")

# Percentage of words that are water by chapter:
waterlog_tidy %>% 
  group_by(chapter_number, chapter_title) %>% 
  count(yay_nay = word == "water") %>% 
  summarise(n = n,
            total = sum(n)) %>% 
  slice(2) %>% 
  ungroup() %>% 
  mutate(water_pct = n/total,
         chapter = glue("{chapter_title} ({chapter_number})"),
         chapter = fct_reorder(chapter, water_pct)) %>% 
  ggplot(aes(water_pct, chapter)) +
  geom_col(fill = "#006994") +
  geom_vline(aes(xintercept = 0.01),
             linetype = "dotted",
             colour = "#942F00") +
  scale_x_continuous(labels = percent_format()) +
  labs(title = "Water makes up more than 1% of words in only 2 chapters",
       subtitle = "What percentage of words are water?",
       x = "% water",
       y = NULL,
       caption = caption)

# Scrape text from A Guide to the 58 Crazy Different Terms for Water:
bow_scraped <- read_html("https://www.atlasobscura.com/articles/a-guide-to-the-58-crazy-different-terms-for-water") %>% 
  html_nodes(".item-body-text-graf") %>% 
  html_text()

# Extract water bodies:
bow <- tibble(text = bow_scraped) %>% 
  unnest_tokens(bow, text, token = "ngrams", n = 2) %>% 
  filter(str_detect(bow, "^[^\\s]+\\s\\d+$")) %>% 
  mutate(bow = str_remove_all(bow, pattern = " \\d+")) %>% 
  filter(bow != "nearly") %>% 
  add_row(bow = c("moat",
                  "gill",
                  "lido",
                  "moat",
                  "llyn",
                  "whirlpool"))

# Count water bodies mentioned in text:
waterlog_tidy %>% 
  filter(word %in% bow$bow) %>% 
  count(word, sort = TRUE) %>% 
  top_n(20) %>% 
  mutate(word = fct_reorder(word, n)) %>% 
  ggplot(aes(n, word)) +
  geom_col(fill = "#006994") +
  labs(title = "The vast majority of Roger's swims were in rivers and pools",
       subtitle = "Top 20 most common water bodies",
       x = n_label,
       y = "Water body",
       caption = caption)

# Which words most commonly precede water?
water_before <- bigrams_filtered %>%
  filter(word2 == "water") %>%
  count(word1, sort = TRUE) %>% 
  top_n(10) %>% 
  mutate(ba = "[] water") %>% 
  rename(word = word1)

# Which words most commonly follow water?
water_after <- bigrams_filtered %>%
  filter(word1 == "water") %>%
  count(word2, sort = TRUE) %>% 
  top_n(10) %>% 
  mutate(ba = "water []") %>% 
  rename(word = word2)

# What are the most common words which occur before and after water?
water_before %>% 
  bind_rows(water_after) %>% 
  mutate(word = fct_reorder(word, n)) %>% 
  ggplot(aes(n, word)) +
  geom_col(fill = "#006994") +
  facet_wrap(~ba, scales = "free_y") +
  labs(title = "Green, white, black, blue",
       subtitle = "Most common words to precede and follow water",
       x = n_label,
       y = NULL)
