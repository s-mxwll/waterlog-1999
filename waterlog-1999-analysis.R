# Text and sentiment analysis of the wild swimming classic Waterlog (1999) by Roger Deakin
# s-mxwll
# 2021

# Load packages:
library(glue)
library(tidylo)
library(rvest)
library(scales)
library(textdata)
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
  scale_x_continuous(labels = comma_format()) +
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
  scale_x_continuous(labels = comma_format()) +
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
  scale_x_continuous(labels = comma_format()) +
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
  labs(title = "Unsurprisingly for a book about wild swimming, water is the most common word",
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
       x = "Chapter number",
       y = "n(water)")

# Shown as percentage:
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
  ggplot(aes(chapter_number, water_pct)) +
  geom_col(fill = "#006994") +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Which chapters are the wettest?",
       subtitle = "n(water)/n(total)",
       x = "Chapter number",
       y = "% water")

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

# Weighted log odds ratio for water through the book:
waterlog_tidy %>% 
  anti_join(stop_words, by = "word") %>% 
  count(chapter_number, word, sort = TRUE) %>% 
  bind_log_odds(chapter_number, word, n) %>% 
  arrange(desc(log_odds_weighted)) %>% 
  filter(word == "water") %>% 
  ggplot(aes(chapter_number, log_odds_weighted)) +
  geom_col(fill = "#006994") +
  geom_smooth(method = "lm", colour = "#942F00") +
  labs(title = "In which chapters is the word water more or less likely to appear?",
       subtitle = "The word water becomes a more common feature through the book",
       x = "Chapter number",
       y = "Weighted log odds ratio")

# Weighted log odds ratio in descending order:
waterlog_tidy %>% 
  anti_join(stop_words, by = "word") %>% 
  count(chapter_number, word, sort = TRUE) %>% 
  bind_log_odds(chapter_number, word, n) %>% 
  filter(word == "water") %>% 
  mutate(chapter_number = fct_reorder(as.factor(chapter_number), log_odds_weighted)) %>% 
  ggplot(aes(log_odds_weighted, chapter_number)) +
  geom_col(fill = "#006994") +
  labs(title = "In which chapters is the word water more or less likely to appear?",
       subtitle = "Weighted log odds ratio of the word water",
       x = "Weighted log odds ratio",
       y = "Chapter number")

# Scrape text from A Guide to the 58 Crazy Different Terms for Water:
bow_scraped <- read_html("https://www.atlasobscura.com/articles/a-guide-to-the-58-crazy-different-terms-for-water") %>% 
  html_nodes(".item-body-text-graf") %>% 
  html_text()

# Extract water bodies:
bow <- tibble(text = bow_scraped) %>% 
  unnest_tokens(bow, text, token = "ngrams", n = 2) %>% 
  filter(str_detect(bow, "^[^\\s]+\\s\\d+$")) %>% 
  mutate(bow = str_remove_all(bow, pattern = " \\d+")) %>% 
  filter(bow != "nearly",
         !str_detect(bow, "sea")) %>% 
  add_row(bow = c("moat",
                  "gill",
                  "lido",
                  "moat",
                  "llyn",
                  "sea",
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

temp <- c("hot", "cold")

#
waterlog_bigrams <- waterlog_raw %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

# 
bigram_counts <- waterlog_bigrams %>% 
  count(bigram, sort = TRUE) %>% 
  separate(bigram, into = c("word1", "word2"), sep = " ") %>% 
  filter(word1 %in% temp,
         !word2 %in% stop_words$word) %>% 
  count(word1, word2, wt = n, sort = TRUE) %>% 
  rename(total = n)

# 
word_ratios <- bigram_counts %>% 
  group_by(word2) %>% 
  filter(sum(total) > 1) %>% 
  ungroup() %>% 
  pivot_wider(names_from = word1,
              values_from = total,
              values_fill = 0) %>% 
  mutate_if(is.integer, funs((. + 1) / sum(. + 1))) %>% 
  mutate(log_ratio = log2(hot / cold)) %>% 
  arrange(desc(log_ratio))

#
word_ratios %>% 
  mutate(abs_lr = abs(log_ratio)) %>%
  group_by(log_ratio < 0) %>%
  top_n(15, abs_lr) %>%
  ungroup() %>%
  filter(!word2 %in% c("tap",
                       "sensors")) %>% 
  mutate(word = reorder(word2, log_ratio)) %>%
  ggplot(aes(log_ratio, word, colour = log_ratio < 0)) +
  geom_segment(aes(y = word,
                   yend = word,
                   x = 0,
                   xend = log_ratio),
               size = 1.1) +
  geom_point(size = 3.5) +
  labs(title = "Words paired with 'hot' and 'cold' in Waterlog",
       subtitle = "Cold bathing demands a hot chocolate",
       x = "Relative appearance after 'hot' compared to 'cold'",
       y = NULL) +
  scale_colour_manual(values = c("#942F00", "#006994"),
                      name = "",
                      labels = c("More 'hot'", "More 'cold'")) +
  scale_x_continuous(limits = c(-2,3),
                     breaks = seq(-2, 3),
                     labels = c("0.25x", "0.5x", 
                                "Same", "2x", "4x", "8x"))

# SENTIMENT ANALYSIS ------------------------------------------------------

# Sentiment score for each 80-line chunk:
waterlog_tidy %>% 
  inner_join(get_sentiments("bing")) %>%
  count(chapter_number, index = line_number %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>% # needs to be pivot_wider
  mutate(sentiment = positive - negative) %>% 
  ggplot(aes(index, sentiment, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  scale_fill_gradient(low = "#006994",
                      high = "#942F00") +
  labs(title = "73% of 80-line chunks are emotionally positive",
       subtitle = "Hell Gill and Corryvreckan Whirlpool are the msot emotionally negative",
       x = "80-line chunk",
       y = "Sentiment score")

# Sentiment score for each chapter:
waterlog_tidy %>% 
  inner_join(get_sentiments("bing")) %>%
  count(chapter_number, sentiment) %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = 0) %>% 
  mutate(sentiment = positive - negative) %>% 
  arrange(sentiment) %>% 
  ggplot(aes(chapter_number, sentiment, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  scale_fill_gradient(low = "#006994",
                      high = "#942F00") +
  labs(title = "72% of chapters (including the first 16 chapters) are emotionally positive",
       subtitle = "Sentiment score by chapter number",
       x = "Chapter",
       y = "Sentiment score")

# Get AFINN score for each 80-line chunk:
afinn <- waterlog_tidy %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = line_number %/% 80) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

# Get Bing et al. and NRC sentiment score:
bing_and_nrc <- bind_rows(
  waterlog_tidy %>% 
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing"),
  waterlog_tidy %>% 
    inner_join(get_sentiments("nrc") %>% 
                 filter(sentiment %in% c("positive", 
                                         "negative"))
    ) %>%
    mutate(method = "NRC")) %>%
  count(method, index = line_number %/% 80, sentiment) %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

# Bind sentiment score for each method together and plot:
bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y") +
  labs(title = NULL,
       subtitle = "Sentiment score according to three different methods",
       x = "80-line chunk",
       y = "Sentiment score")

# What are the most positive and negative words?
bing_word_count <- waterlog_tidy %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

# Which words contribute more to sentiment score?
bing_word_count %>% 
  group_by(sentiment) %>% 
  top_n(15) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("#006994", "#800000")) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Which words contribute more to sentiment score?",
       subtitle = "Simile is a common figure of speech in Roger Deakin's writing",
       x = "Contribution to sentiment",
       y = NULL)
