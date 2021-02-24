library(tidyverse)
library(igraph)
library(scales)
source('R/functions.R')

# to suppress the many non-critical dplyr warnings that will show
options(dplyr.summarise.inform = F)

quarter <- 'Q2Q3'
year <- 2018

df <-
  readRDS(paste0(
    'data/preprocessed/df_',
    quarter = quarter,
    year = year,
    '.rds'
  ))

ground_truth <- readr::read_csv('data/raw/ground_truth.csv') %>%
  rename('cat' = `Expertise Role`,
         'object' = `In-Demand Skills`) %>%
  mutate(cat = tolower(cat),
         object = tolower(object)) %>%
  mutate(object = strsplit(object, ", ")) %>%
  unnest(object) %>%
  select(cat, object) %>%
  unique() %>%
  mutate(
    cat = case_when(
      str_detect(cat, 'sql') ~ 'sql',
      str_detect(cat, 'google') ~ 'google',
      str_detect(cat, '\\.net') ~ '.net',
      str_detect(cat, 'react') ~ 'react',
      str_detect(cat, 'angular') ~ 'angular',
      str_detect(cat, 'amazon') ~ 'amazon',
      str_detect(cat, 'html') ~ 'html',
      str_detect(cat, '\\.js|typescript') ~ 'javascript',
      str_detect(cat, 'ms-') ~ 'microsoft',
      cat == 'tensorflow|pandas|scikit-learn' ~ 'python',
      TRUE ~ cat
    )
  )

# harmonic F1 measure.
# https://arxiv.org/pdf/1902.01691.pdf
F1_h <- function(quarter, year) {
  precision <- function(A, B) {
    return(length(intersect(A, B)) / length(A))
  }
  
  recall <- function(A, B) {
    return(length(intersect(A, B)) / length(B))
  }
  
  F1 <- function(A, B) {
    return(2 * ((precision(A, B) * recall(A, B)) / (precision(A, B) + recall(A, B))))
  }
  
  F_XY <- function(X, Y) {
    F1_list <- c()
    for (i in X$cat %>% unique()) {
      X_objs <- X %>%
        filter(cat == i) %>%
        select(object) %>%
        unique() %>%
        pull()
      
      value_max <- -1
      
      for (j in Y$cat %>% unique()) {
        Y_objs <- Y %>%
          filter(cat == j) %>%
          select(object) %>%
          unique() %>%
          pull()
        
        F1_obj <- F1(X_objs, Y_objs)
        
        if (is.na(F1_obj)) {
          
        } else if (F1_obj > value_max) {
          value_max <- F1_obj
        }
      }
      if (value_max == -1) {
        F1_list <- F1_list %>% append(NA)
      } else {
        F1_list <- F1_list %>% append(value_max)
      }
    }
    return(sum(F1_list, na.rm = TRUE) / length(F1_list))
  }
  
  return(2 * ((
    F_XY(communities, ground_truth) * F_XY(ground_truth, communities)
  ) /
    (
      F_XY(communities, ground_truth) + F_XY(ground_truth, communities)
    )))
}

F1_h_df <- tibble(i = seq(100))
F1_h_list = c()

# https://www.nature.com/articles/s41598-019-53166-6
for (i in seq(100)) {
  G <- networkise(
    df,
    sample_method = 'random',
    size = 1000,
    similarity = 'cosine',
    quarter = quarter,
    year = year
  )
  
  graph <- G %>%
    graph_from_data_frame(directed = FALSE) %>%
    set_edge_attr("weight", value = G$weight)
  remove(G)
  
  communities_louvain <- graph %>%
    cluster_louvain()
  
  # add clusters back to iGraph object
  graph <- graph %>%
    set_vertex_attr(name = 'communities_louvain',
                    index = communities_louvain$names,
                    value = communities_louvain$membership)
  
  df_communities <- df %>%
    inner_join(
      tibble(
        OwnerUserId = communities_louvain$names,
        community = communities_louvain$membership
      ),
      by = "OwnerUserId"
    )
  
  # What are the main tags per community?
  community_detail <- df_communities %>%
    group_by(community, Tag) %>%
    summarise(num_tag_per_community = n_distinct(ParentId)) %>%
    group_by(community) %>%
    slice_max(order_by = num_tag_per_community, n = 5) %>%
    # normalise the proportions per community to see which ones to keep
    group_by(community) %>%
    mutate(
      prop_tag_per_community = num_tag_per_community / sum(num_tag_per_community),
      rank_tag_per_community = row_number()
    )
  
  communities <- community_detail %>%
    rename('cat' = 'community',
           'object' = 'Tag') %>%
    ungroup() %>%
    select(cat, object) %>%
    unique()
  
  F1_h_val <- F1_h(quarter = quarter, year = year)
  F1_h_list <- F1_h_list %>% append(F1_h_val)
  print(paste('Finished', i))
  
}

F1_h_df <- F1_h_df %>%
  mutate(F1_h_vals = F1_h_list)

# distribution of harmonic F1 scores.
F1_h_df %>%
  ggplot(aes(x = F1_h_vals)) +
  geom_histogram(binwidth = 0.01)

qqnorm(F1_h_df$F1_h_vals)
qqline(F1_h_df$F1_h_vals)


# T-test to determine if the harmonic F1 score is statistically significantly 1.
# This will prove that the communities are different to ground truth.
# if p-value < confidence level then alternative hypothesis is accepted.
t.test(
  F1_h_df$F1_h_vals,
  alternative = "less",
  mu = 1,
  conf.level = 0.95
)
