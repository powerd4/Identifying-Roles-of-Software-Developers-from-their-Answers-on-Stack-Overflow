library(tidyverse)
library(readr)

# create the starting data frame from the raw data.
preprocess <- function(quarter = 'Q2Q3', year = 2018) {
  # read and concatenate all files in folder
  df <-
    list.files(path = paste0("data/raw/", quarter, year, "_data"),
               full.names = TRUE) %>%
    lapply(read_csv) %>%
    bind_rows() %>%
    unique() %>%
    mutate(
      ParentId = as.character(ParentId),
      OwnerUserId = as.character(OwnerUserId),
      
      # keep only the top tag per question.
      Tag = word(Tags, 1, sep = ">"),
      Tag = str_remove(Tag, "<")
    ) %>%
    
    # remove rows with missing data.
    drop_na()
  
  # there are a lot of tag synonyms such as python-3.x, azure-cdn etc.
  # make a rule that if the substring before the first hyphen is also a tag,
  # then rename the tag as that substring.
  df <- df %>%
    mutate(prehyphen = str_split(Tag, '-', simplify = TRUE)[, 1]) %>%
    left_join(
      df %>%
        select(Tag) %>%
        unique() %>%
        mutate(prehyphen_is_tag = 1),
      by = c('prehyphen' = 'Tag')
    ) %>%
    mutate(TagNew = case_when(
      prehyphen_is_tag == 1 & Tag != 'ruby-on-rails' ~ prehyphen,
      TRUE ~ Tag
    )) %>%
    select(Id, ParentId, OwnerUserId, TagNew) %>%
    rename(Tag = TagNew) %>%
    
    # some ad-hoc synonyms from inspection.
    mutate(
      Tag = case_when(
        str_detect(Tag, 'sql') ~ 'sql',
        str_detect(Tag, 'google') ~ 'google',
        str_detect(Tag, '\\.net') ~ '.net',
        str_detect(Tag, 'react') ~ 'react',
        str_detect(Tag, 'angular') ~ 'angular',
        str_detect(Tag, 'amazon') ~ 'amazon',
        str_detect(Tag, 'html') ~ 'html',
        str_detect(Tag, '\\.js|typescript') ~ 'javascript',
        str_detect(Tag, 'ms-') ~ 'microsoft',
        Tag == 'tensorflow|pandas|scikit-learn' ~ 'python',
        TRUE ~ Tag
      )
    ) %>%
    
    # remove some irrelevant but popular tags as per analysis below.
    filter(
      !Tag %in% c(
        'excel',
        'twitter',
        'facebook',
        'regex',
        'unit-testing',
        'json',
        'list',
        'arrays',
        'xml',
        'multithreading',
        'batch-file',
        'string',
        'shell'
      )
    ) %>%
    
    # user-level fields
    group_by(OwnerUserId) %>%
    mutate(user_tag_breadth = n_distinct(Tag),
           user_question_total = n_distinct(ParentId)) %>%
    ungroup() %>%
    
    # tag-level fields
    group_by(Tag) %>%
    mutate(tag_count = n_distinct(ParentId)) %>%
    ungroup() %>%
    
    # user-tag-level fields
    group_by(OwnerUserId, Tag) %>%
    mutate(user_tag_count = n_distinct(ParentId)) %>%
    ungroup() %>%
    
    # remove authors with only one tag as it is useless to us.
    filter(user_tag_breadth > 1)
  
  saveRDS(df, file = paste0('data/preprocessed/df_', quarter, year, '.rds'))
}


# create source-target-weight format df to create network.
networkise <- function(df,
                       sample_method = 'full',
                       size = -1,
                       similarity = 'cosine',
                       quarter = 'Q2Q3',
                       year = 2018,
                       output_to_file = FALSE) {
  if (size > df %>% summarise(n_distinct(OwnerUserId)) %>% pull()) {
    print('Error: size must be less than or equal to the number of users.')
  }
  
  if (sample_method == 'random') {
    G <- df %>%
      inner_join(df %>%
                   select(OwnerUserId) %>%
                   unique() %>%
                   slice_sample(n = size),
                 by = "OwnerUserId")
  } else if (sample_method == 'top') {
    G <- df %>%
      inner_join(
        df %>%
          select(OwnerUserId, user_question_total) %>%
          unique() %>%
          arrange(desc(user_question_total)) %>%
          head(top_n_users),
        by = "OwnerUserId"
      )
  } else if (sample_method == 'full') {
    size <- 'all'
    G <- df
  } else {
    print('Error: sample_method must be either \"random\" or \"top\".')
  }
  
  if (similarity == 'simple') {
    G <- G %>%
      widyr::pairwise_count(OwnerUserId, Tag) %>%
      rename(User = item1,
             User_2 = item2,
             weight = n) %>%
      # keep A-B and discard B-A
      filter(User < User_2) %>%
      arrange(desc(weight)) %>%
      # max normalisation to keep between 0 and 1
      mutate(weight = weight / max(weight))
    
  } else if (similarity == 'cosine') {
    G <- G %>%
      
      # tag cosine similarity.
      # this can be used to weight the network.
      # https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0109507
      widyr::pairwise_similarity(OwnerUserId, Tag, user_tag_count) %>%
      rename(User = item1,
             User_2 = item2,
             weight = similarity) %>%
      # keep A-B and discard B-A
      filter(User < User_2) %>%
      arrange(desc(weight))
    
  } else {
    print('Error: similarity must be either \"simple\" or \"cosine\".')
  }
  
  if (output_to_file == TRUE) {
    G %>%
      write_csv(
        paste0(
          'data/preprocessed/',
          quarter,
          year,
          '_',
          sample_method,
          '_',
          size,
          '_',
          similarity,
          '.csv'
        )
      )
  } else {
    return(G)
  }
}


# import preprocessed network-form data (networkise output)
import_network <-
  function(quarter,
           year,
           sample_method,
           size,
           similarity) {
    read_csv(
      paste0(
        'data/preprocessed/',
        quarter,
        year,
        '_',
        sample_method,
        '_',
        size,
        '_',
        similarity,
        '.csv'
      )
    )
  }