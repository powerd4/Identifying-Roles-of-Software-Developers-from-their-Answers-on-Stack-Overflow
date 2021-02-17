library(tidyverse)
library(scales)


quarter <- 'Q2Q3'
year <- 2018

df <-
  readRDS(paste0('data/preprocessed/df_', quarter, year, '.rds'))

# ~16k users, ~2k tags, ~150k questions
df %>% 
  summarise(n_distinct(OwnerUserId),
            n_distinct(Tag),
            n_distinct(ParentId))

# Inspect the tag popularity distribution.

df %>%
  select(Tag, tag_count) %>% 
  unique() %>% 
  arrange(desc(tag_count)) %>%
  mutate(tag_count_rank = row_number(),
         tag_count_prop = cumsum(tag_count / sum(tag_count))) %>% 
  ggplot(aes(x = tag_count_rank, y = tag_count_prop)) +
  geom_step() +
  scale_x_continuous(limits = c(0, 200), labels = label_number_si()) +
  scale_y_continuous(labels = label_percent(), breaks = seq(0, 1, 0.1)) +
  coord_flip() +
  labs(x = 'The top y tags...', y = '...cover off x% of questions') +
  theme_light()

# Inspect user question total.
df %>% 
  select(OwnerUserId, user_question_total) %>% 
  unique() %>% 
  ggplot(aes(x = user_question_total)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(labels = label_number_si()) +
  scale_y_continuous(labels = label_number_si()) +
  coord_cartesian(x = c(0, 25)) +
  labs(x = 'Questions Answered', y = 'Users') +
  theme_light()

# Inspect user unique tags.
df %>% 
  select(OwnerUserId, user_tag_breadth) %>% 
  unique() %>% 
  ggplot(aes(x = user_tag_breadth)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(labels = label_number_si()) +
  scale_y_continuous(labels = label_number_si()) +
  coord_cartesian(x = c(0, 25)) +
  labs(x = 'Different Tags Answered', y = 'Users') +
  theme_light()
