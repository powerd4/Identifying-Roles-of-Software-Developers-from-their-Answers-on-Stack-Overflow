library(tidyverse)
library(igraph)
library(scales)
source('R/functions.R')

quarter <- 'Q2Q3'
year <- 2018

G <- import_network('Q2Q3', '2018', 'full', 'all', 'cosine')

# edge weight distribution.
G %>%
  slice_sample(prop = 0.1) %>%
  ggplot(aes(x = weight)) +
  geom_step(aes(y = 1 - ..y..), stat = 'ecdf') +
  scale_x_continuous() +
  scale_y_continuous(labels = label_percent(), breaks = seq(0, 1, 0.1)) +
  labs(x = '... Are Greater Than X', y = 'Y% of Weights...') +
  theme_light()

# iGraph will be used to cluster using Louvain and modularity.
graph <- G %>%
  graph_from_data_frame(directed = FALSE) %>%
  set_edge_attr("weight", value = G$weight)

# graph <- readRDS(file = paste0('data/preprocessed/graph_', quarter, year, '.rds'))

# lots of memory issues with this huge network so drop this huge object.
remove(G)

# weighted clustering
communities_louvain <- graph %>%
  cluster_louvain()

# add clusters back to iGraph object
graph <- graph %>%
  set_vertex_attr(name = 'communities_louvain',
                  index = communities_louvain$names,
                  value = communities_louvain$membership)

# save graph as object
saveRDS(graph, file = paste0('data/preprocessed/graph_', quarter, year, '.rds'))

# save graph in format readable to Gephi (sample - Gephi takes ~ 1M edges)
graph %>%
  delete.vertices(sample.int(vcount(graph), vcount(graph) - 1000)) %>%
  rgexf::igraph.to.gexf() %>%
  rgexf::write.gexf(output = 'data/preprocessed/for_gephi.gexf')


# Analyse communities
df <-
  readRDS(paste0('data/preprocessed/df_', quarter, year, '.rds'))

df_communities <- df %>%
  inner_join(
    tibble(
      OwnerUserId = communities_louvain$names,
      community = communities_louvain$membership
    )
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

community_detail %>%
  group_by(community) %>%
  summarise(type = paste(unique(Tag), collapse = ", "))

community_detail %>%
  select(Tag,
         community,
         rank_tag_per_community,
         prop_tag_per_community) %>%
  unique() %>%
  ggplot(
    aes(
      x = rank_tag_per_community,
      y = prop_tag_per_community,
      col = as.character(community),
      group = as.character(community)
    )
  ) +
  geom_point() +
  geom_line()


# hierarchical clustering (non-network)
# G_dist_mat <- G %>% 
#   mutate(distance = 1 - weight) %>% 
#   select(User, User_2, distance) %>% 
#   unique() %>% 
#   reshape2::acast(User ~ User_2, value.var = 'distance')




