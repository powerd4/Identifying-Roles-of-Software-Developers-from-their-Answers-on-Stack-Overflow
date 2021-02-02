tictoc::tic('Timing the script')

library(tidyverse)
library(readr)
library(scales)

year <- 2018

preprocess <- function(year) {
  # read and concatenate all files in folder
  list.files(path = paste0("data/raw/", year, "_data"), full.names = TRUE) %>%
    lapply(read_csv) %>%
    bind_rows() %>%
    unique() %>%
    mutate(
      ParentId = as.character(ParentId),
      OwnerUserId = as.character(OwnerUserId),
      
      # keep only the top tag per question.
      Tags = word(Tags, 1, sep = ">"),
      Tags = str_remove(Tags, "<"),
      
      # group some common tag synonyms together.
      Tags = case_when(
        str_detect(Tags, 'angular') ~ 'angular',
        str_detect(Tags, 'apache') ~ 'apache',
        str_detect(Tags, 'asp.net') ~ 'asp.net',
        str_detect(Tags, 'google') ~ 'google',
        str_detect(Tags, 'sql') ~ 'sql',
        str_detect(Tags, 'python') ~ 'python',
        str_detect(Tags, 'react') ~ 'react',
        str_detect(Tags, 'ruby') ~ 'ruby',
        str_detect(Tags, 'spring') ~ 'spring',
        str_detect(Tags, 'visual') ~ 'visual',
        TRUE ~ Tags
      )
    ) %>%
    
    # tags per author.
    group_by(OwnerUserId) %>%
    mutate(user_tag_breadth = n_distinct(Tags)) %>%
    ungroup() %>%
    
    # remove authors with only one tag.
    filter(user_tag_breadth > 1)
}

df <- preprocess(year)

# most common tags answered.
wheelhouses <- df %>%
  arrange(Tags) %>%
  group_by(OwnerUserId) %>%
  summarise(taglist = toString(unique(Tags))) %>%
  group_by(taglist) %>%
  summarise(cnt = n_distinct(OwnerUserId)) %>%
  arrange(desc(cnt))

wheelhouses %>%
  mutate(num_tags = str_count(taglist, ',') + 1) %>%
  filter(num_tags == 3)


# inspect the tag popularity distribution.
# there are 6,888 unique questions, BUT:
# the top 150 tags covers off around 90% of questions.
# I will limit to the top 150 tags for computational efficiency.
df %>%
  group_by(Tags) %>%
  summarise(TagCount = n_distinct(ParentId)) %>%
  arrange(desc(TagCount)) %>%
  mutate(TagRank = row_number(),
         Prop = cumsum(TagCount / sum(TagCount))) %>%
  ggplot(aes(x = TagRank, y = Prop)) +
  geom_step() +
  scale_x_continuous(limits = c(0, 500), labels = label_number_si()) +
  scale_y_continuous(labels = label_percent(), breaks = seq(0, 1, 0.1)) +
  coord_flip() +
  labs(x = 'The top y tags...', y = '...are on x% of questions') +
  theme_light()

# create source-target-weight format df to create network.
networkise <-
  function(df,
           top_n_tags = 150,
           similarity = 'simple') {
    G <- df %>%
      inner_join(
        df %>%
          group_by(Tags) %>%
          summarise(TagCount = n_distinct(ParentId)) %>%
          arrange(desc(TagCount)) %>%
          head(top_n_tags)
      )
    
    if (similarity == 'simple') {
      G %>%
        widyr::pairwise_count(Tags, OwnerUserId) %>% 
        rename(Tags = item1,
               Tags_2 = item2,
               Weight = n) %>%
        # keep A-B and discard B-A
        filter(Tags < Tags_2) %>%
        arrange(desc(Weight)) %>%
        write_csv(paste0('data/preprocessed/', year, '_top_', top_n_tags, '_', similarity, '.csv'))
    } else if (similarity == 'cosine') {
      G %>%
        
        # tag cosine similarity.
        # this can be used to weight the network.
        # https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0109507
        group_by(Tags, OwnerUserId) %>%
        summarise(num_tags = n_distinct(ParentId)) %>%
        ungroup() %>%
        widyr::pairwise_similarity(Tags, OwnerUserId, num_tags) %>% 
        rename(Tags = item1,
               Tags_2 = item2,
               Weight = similarity) %>%
        # keep A-B and discard B-A
        filter(Tags < Tags_2) %>%
        arrange(desc(Weight)) %>%
        write_csv(paste0('data/preprocessed/', year, '_top_', top_n_tags, '_', similarity, '.csv'))
    } else {
      print('Error: similarity must be either \"simple\" or \"cosine\".')
    }
  }

networkise(df, top_n_tags = 150, similarity = 'simple')

tictoc::toc()  # time the script