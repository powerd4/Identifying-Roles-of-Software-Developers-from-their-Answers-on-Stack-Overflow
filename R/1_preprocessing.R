source('R/functions.R')

quarter <- 'Q2Q3'
year <- 2018

preprocess(quarter, year, only_ground_truth_tags = 'only_ground_truth_tags')

df <-
  readRDS(paste0('data/preprocessed/df_', quarter, year, only_ground_truth_tags, '.rds'))

networkise(df,
           sample_method = 'full',
           similarity = 'cosine',
           quarter = quarter,
           year = year,
           only_ground_truth_tags = 'only_ground_truth_tags')