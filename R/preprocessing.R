library(tidyverse)
library(readr)
library(scales)

year <- 2018

preprocess <- function(year) {
  # read and concatenate all files in folder
  df <- list.files(path = paste0("data/raw/", year, "_data"),
                   full.names = TRUE) %>%
    lapply(read_csv) %>%
    bind_rows() %>%
    unique() %>%
    mutate(
      ParentId = as.character(ParentId),
      OwnerUserId = as.character(OwnerUserId),
      
      # keep only the top tag per question.
      Tag = word(Tags, 1, sep = ">"),
      Tag = str_remove(Tag, "<")) %>% 
    
    # remove rows with missing data.
    drop_na()
  
  # there are a lot of tag synonyms such as python-3.x, azure-cdn etc.
  # make a rule that if the substring before the first hyphen is also a tag,
  # then rename the tag as that substring.
  df %>% 
    mutate(prehyphen = str_split(Tag, '-', simplify = TRUE)[,1]) %>% 
    left_join(df %>% 
                select(Tag) %>% 
                unique() %>% 
                mutate(prehyphen_is_tag = 1),
              by = c('prehyphen' = 'Tag')) %>%
    mutate(TagNew = case_when(prehyphen_is_tag == 1 ~ prehyphen,
                              TRUE ~ Tag)) %>% 
    select(Id, ParentId, OwnerUserId, TagNew) %>% 
    rename(Tag = TagNew) %>%
    
    # some ad-hoc synonyms from inspection.
    mutate(Tag = case_when(str_detect(Tag, 'sql') ~ 'sql',
                           str_detect(Tag, 'google') ~ 'google',
                           str_detect(Tag, 'asp.net') ~ 'asp.net',
                           str_detect(Tag, '.js') ~ 'javascript',
                           str_detect(Tag, 'asp.net') ~ 'asp.net',
                           str_detect(Tag, 'react') ~ 'react',
                           TRUE ~ Tag)) %>% 
    
    # remove some irrelevant but popular tags as per analysis below.
    filter(!Tag %in% c('excel', 'twitter', 'regex', 'unit-testing', 
                       'json', 'list', 'arrays', 'xml', 'wordpress', 'multithreading', 'batch-file', 
                       'string', 'shell')) %>% 
    
    # different tags per author, and total questions answered per author.
    group_by(OwnerUserId) %>%
    mutate(user_tag_breadth = n_distinct(Tag),
           user_question_total = n_distinct(ParentId)) %>%
    ungroup() %>%
    
    # remove authors with only one tag as it is useless to us.
    filter(user_tag_breadth > 1)
}

df <- preprocess(year)

# ~50k users, ~4k tags, ~590k questions
df %>% 
  summarise(n_distinct(OwnerUserId),
            n_distinct(Tag),
            n_distinct(ParentId))

# inspect the tag popularity distribution.
df %>%
  group_by(Tag) %>%
  summarise(TagCount = n_distinct(ParentId)) %>%
  arrange(desc(TagCount)) %>%
  mutate(TagRank = row_number(),
         Prop = cumsum(TagCount / sum(TagCount))) %>% 
  ggplot(aes(x = TagRank, y = Prop)) +
  geom_step() +
  scale_x_continuous(limits = c(0, 200), labels = label_number_si()) +
  scale_y_continuous(labels = label_percent(), breaks = seq(0, 1, 0.1)) +
  coord_flip() +
  labs(x = 'The top y tags...', y = '...are on x% of questions') +
  theme_light()

# inspect user question total.
df %>% 
  select(OwnerUserId, user_question_total) %>% 
  unique() %>% 
  ggplot(aes(x = user_question_total)) +
  geom_histogram(binwidth = 1) +
  coord_cartesian(x = c(0, 25))

# inspect user unique tags.
df %>% 
  select(OwnerUserId, user_tag_breadth) %>% 
  unique() %>% 
  ggplot(aes(x = user_tag_breadth)) +
  geom_histogram(binwidth = 1) +
  coord_cartesian(x = c(0, 25))

# create source-target-weight format df to create network.
networkise <-
  function(df,
           top_n_users = 10000,
           similarity = 'cosine') {
    
    G <- df %>%
      inner_join(df %>% 
                   select(OwnerUserId, user_question_total) %>% 
                   unique() %>% 
                   arrange(desc(user_question_total)) %>% 
                   head(top_n_users)
                 )
    
    if (similarity == 'simple') {
      G %>%
        widyr::pairwise_count(OwnerUserId, Tag) %>%
        rename(User = item1,
               User_2 = item2,
               weight = n) %>%
        # keep A-B and discard B-A
        filter(User < User_2) %>%
        arrange(desc(weight)) %>%
        # max normalisation to keep between 0 and 1
        mutate(weight = weight / max(weight)) %>% 
        write_csv(paste0(
          'data/preprocessed/',
          year,
          '_top_',
          top_n_users,
          '_',
          similarity,
          '.csv'
        ))
    } else if (similarity == 'cosine') {
      G %>%
        
        # tag cosine similarity.
        # this can be used to weight the network.
        # https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0109507
        group_by(Tag, OwnerUserId) %>%
        summarise(num_answers = n()) %>%
        ungroup() %>%
        widyr::pairwise_similarity(OwnerUserId, Tag, num_answers) %>%
        rename(User = item1,
               User_2 = item2,
               weight = similarity) %>%
        # keep A-B and discard B-A
        filter(User < User_2) %>%
        arrange(desc(weight)) %>%
        write_csv(paste0(
          'data/preprocessed/',
          year,
          '_top_',
          top_n_users,
          '_',
          similarity,
          '.csv'
        ))
    } else {
      print('Error: similarity must be either \"simple\" or \"cosine\".')
    }
  }

for (n in c(1000, 5000, 10000, 20000)) {
  for (s in c('simple', 'cosine')) {
    networkise(df, top_n_users = n, similarity = s)
  }
}
