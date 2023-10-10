library(tidyverse)
library(here)

# For text mining:
library(pdftools)
library(tidytext)
library(textdata)
library(ggwordcloud)

#First step was to get my data in R:
got_path <- here("data","got.pdf")
got_text <- pdf_text(got_path)

#Just to make sure that it works, I tested if I could see a page, and I could!
got_p9 <- got_text[9]
got_p9

#Trim the text and get rid of \n
got_df <- data.frame(got_text) %>%
  mutate(text_full = str_split(got_text, pattern = '\n')) %>%
  unnest(text_full) %>%
  mutate(text_full = str_trim(text_full))

#Split the columns into tokens:
got_tokens <- got_df %>%
  unnest_tokens(word, text_full)
got_tokens

#Count the words
got_wc <- got_tokens %>%
  count(word) %>%
  arrange(-n)
got_wc

#remove stopwords
got_stop <- got_tokens %>%
  anti_join(stop_words) %>%
  select(-got_text)

got_swc <- got_stop %>%
  count(word) %>%
  arrange(-n)
got_wc

# This code will filter out numbers by asking:
# If you convert to as.numeric, is it NA (meaning those words)?
# If it IS NA (is.na), then keep it (so all words are kept)
# Anything that is converted to a number is removed
got_no_numeric <- got_stop %>%
  filter(is.na(as.numeric(word)))

#making a word cloud
length(unique(got_no_numeric$word))

# Let's filter to only include the top 100 most frequent:
got_top100 <- got_no_numeric %>%
  count(word) %>%
  arrange(-n) %>%
  head(100)

got_cloud <- ggplot(data = got_top100, aes(label = word)) +
  geom_text_wordcloud() +
  theme_minimal()

got_cloud

#making the word cloud prettier
ggplot(data = got_top100, aes(label = word, size = n)) +
  geom_text_wordcloud_area(aes(color = n), shape = "diamond") +
  scale_size_area(max_size = 12) +
  scale_color_gradientn(colors = c("darkgreen","blue","red")) +
  theme_minimal()

#"afinn": Words ranked from -5 (very negative) to +5 (very positive).

get_sentiments(lexicon = "afinn")
# Note: may be prompted to download (yes)

# Let's look at the pretty positive words:
afinn_pos <- get_sentiments("afinn") %>%
  filter(value %in% c(3,4,5))
afinn_pos

#bing: binary, "positive" or "negative"
get_sentiments(lexicon = "bing")


#I had some trouble getting nrc to work. I googled and found others with the same problem, they gave me this solution:
textdata::lexicon_nrc(delete=TRUE)
textdata::lexicon_nrc() #now everything seems to work

#Now nrc
get_sentiments(lexicon = "nrc")

#First, bind words in `ipcc_stop` to `afinn` lexicon:
got_afinn <- got_stop %>%
  inner_join(get_sentiments("afinn"))

#Let's find some counts (by sentiment ranking):
got_afinn_hist <- got_afinn %>%
  count(value)

# Plot them:
ggplot(data = got_afinn_hist, aes(x = value, y = n)) +
  geom_col()

# What are these '2' words?
got_afinn2 <- got_afinn %>%
  filter(value == 2)

# Check the unique 2-score words:
unique(got_afinn2$word)

# Count & plot them
got_afinn2_n <- got_afinn2 %>%
  count(word, sort = TRUE) %>%
  mutate(word = fct_reorder(factor(word), n))

ggplot(data = got_afinn2_n, aes(x = word, y = n)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90))

#Summarize
got_summary <- got_afinn %>%
  summarize(
    mean_score = mean(value),
    median_score = median(value)
  )

#use inner_join() to combine the IPCC non-stopword text with the nrc lexicon:
got_nrc <- got_stop %>%
  inner_join(get_sentiments("nrc"))

#remembering the excluded words:
got_exclude <- got_stop %>%
  anti_join(get_sentiments("nrc"))

# Count to find the most excluded:
got_exclude_n <- got_exclude %>%
  count(word, sort = TRUE)

head(got_exclude_n)

#Now find some counts:
got_nrc_n <- got_nrc %>%
  count(sentiment, sort = TRUE)

# And plot them:
ggplot(data = got_nrc_n, aes(x = sentiment, y = n)) +
  geom_col()


#Or count by sentiment *and* word, then facet:
got_nrc_n5 <- got_nrc %>%
  count(word,sentiment, sort = TRUE) %>%
  group_by(sentiment) %>%
  top_n(5) %>%
  ungroup()

got_nrc_gg <- ggplot(data = got_nrc_n5, aes(x = reorder(word,n), y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, ncol = 2, scales = "free") +
  coord_flip() +
  theme_minimal() +
  labs(x = "Word", y = "count")

# Show it
got_nrc_gg

# Save it
ggsave(plot = got_nrc_gg,
       here("figures","got_nrc_sentiment.png"),
       height = 8,
       width = 5)

#The task:
#I would have expected that the negative words would be dominating.
# But I found that there are most positive words, or to be fair it is close to be half and half.


