source('R/functions.R')

quarter <- 'Q2Q3'
year <- 2018

preprocess(quarter, year)

df <-
  readRDS(paste0('data/preprocessed/df_', quarter, year, '.rds'))

networkise(df,
           sample_method = 'full',
           similarity = 'cosine',
           quarter = quarter,
           year = year)

networkise(df,
           sample_method = 'full',
           similarity = 'simple',
           quarter = quarter,
           year = year)
