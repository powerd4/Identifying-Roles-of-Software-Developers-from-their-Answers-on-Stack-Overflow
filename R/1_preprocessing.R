source('R/functions.R')

quarter <- 'Q2Q3'
year <- 2018

# this function saves preprocessed df as a file.
preprocess(quarter, year)

df <-
  readRDS(paste0(
    'data/preprocessed/df_',
    quarter = quarter,
    year = year,
    '.rds'
  ))

# save the entire network as a file.
networkise(
  df,
  sample_method = 'full',
  similarity = 'cosine',
  quarter = quarter,
  year = year,
  output_to_file = TRUE
)
