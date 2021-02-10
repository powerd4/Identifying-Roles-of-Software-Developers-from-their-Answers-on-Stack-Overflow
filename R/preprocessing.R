tictoc::tic('Timing the script')

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
    filter(!Tag %in% c('excel', 'vba', 'twitter', 'regex', 'unit-teting', 
                       'json', 'list', 'database', 'arrays', 'powershell', 
                       'xml', 'wordpress', 'multithreading', 'batch-file', 
                       'string', 'shell')) %>% 
    
    # different tags per author, and total questions answered per author.
    group_by(OwnerUserId) %>%
    mutate(user_tag_breadth = n_distinct(Tag),
           user_question_total = n_distinct(ParentId)) %>%
    ungroup() %>%
    
    # remove authors with only one tag.
    filter(user_tag_breadth > 1)
}

df <- preprocess(year)

saveRDS(df, file = "data/preprocessed/df_2018.rds")

# inspect the tag popularity distribution.
# there are 6,888 unique questions, BUT:
# the top 50 tags covers off around 85% of questions at a nice elbow point.
# I will limit to the top 50 tags for computational efficiency.
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

# create source-target-weight format df to create network.
networkise <-
  function(df,
           top_n_tags = 50,
           similarity = 'simple', 
           unique = FALSE) {
    G <- df %>%
      inner_join(
        df %>%
          group_by(Tag) %>%
          summarise(TagCount = n_distinct(ParentId)) %>%
          arrange(desc(TagCount)) %>%
          head(top_n_tags)
      )
    
    if (unique) {
      G <- G %>% 
        select(OwnerUserId, Tag) %>% 
        unique()
      unique_word <- '_unique'
    } else {
      unique_word <- ''
    }
    
    if (similarity == 'simple') {
      G %>%
        widyr::pairwise_count(Tag, OwnerUserId) %>%
        rename(Tag = item1,
               Tag_2 = item2,
               Weight = n) %>%
        # keep A-B and discard B-A
        filter(Tag < Tag_2) %>%
        arrange(desc(Weight)) %>%
        # max normalisation to keep between 0 and 1
        mutate(Weight = Weight / max(Weight)) %>% 
        write_csv(paste0(
          'data/preprocessed/',
          year,
          '_top_',
          top_n_tags,
          '_',
          similarity,
          unique_word,
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
        widyr::pairwise_similarity(Tag, OwnerUserId, num_answers) %>%
        rename(Tag = item1,
               Tag_2 = item2,
               Weight = similarity) %>%
        # keep A-B and discard B-A
        filter(Tag < Tag_2) %>%
        arrange(desc(Weight)) %>%
        write_csv(paste0(
          'data/preprocessed/',
          year,
          '_top_',
          top_n_tags,
          '_',
          similarity,
          unique_word,
          '.csv'
        ))
    } else {
      print('Error: similarity must be either \"simple\" or \"cosine\".')
    }
  }

for (n in c(25, 50, 100)) {
  for (s in c('simple', 'cosine')) {
    for (u in c(TRUE, FALSE)) {
      networkise(df, top_n_tags = n, similarity = s, unique = u)
    }
  }
}


tictoc::toc()  # time the script