library(tidyverse)
library(scales)
library(patchwork)


quarter <- 'Q2Q3'
year <- 2018

df <-
  readRDS(paste0(
    'data/preprocessed/df_',
    quarter = quarter,
    year = year,
    '.rds'
  ))

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


# Analysis on ground-truth roles.

ground_truth <- readr::read_csv('data/raw/ground_truth.csv') %>%
  rename('role' = `Expertise Role`,
         'skill' = `In-Demand Skills`) %>%
  mutate(role = tolower(role),
         skill = tolower(skill)) %>%
  mutate(skill = strsplit(skill, ", ")) %>%
  unnest(skill) %>%
  unique() %>%
  mutate(
    skill = case_when(
      str_detect(skill, 'sql') ~ 'sql',
      str_detect(skill, 'google') ~ 'google',
      str_detect(skill, '\\.net') ~ '.net',
      str_detect(skill, 'react') ~ 'react',
      str_detect(skill, 'angular') ~ 'angular',
      str_detect(skill, 'amazon') ~ 'amazon',
      str_detect(skill, 'html') ~ 'html',
      str_detect(skill, '\\.js|typescript') ~ 'javascript',
      str_detect(skill, 'ms-') ~ 'microsoft',
      skill == 'tensorflow|pandas|scikit-learn' ~ 'python',
      TRUE ~ skill
    )
  )

# There is some degenerecy in roles as they have the same skills.
# These roles can be merged.
merged_roles <- ground_truth %>% 
  arrange(role, skill) %>% 
  group_by(role) %>%
  summarise(skillset = paste(unique(skill), collapse = ", ")) %>% 
  group_by(skillset) %>% 
  mutate(num_roles = n_distinct(role)) %>% 
  ungroup() %>% 
  filter(num_roles > 1) %>% 
  select(role) %>% 
  pull()

ground_truth <- ground_truth %>% 
  mutate(role = case_when(role %in% merged_roles ~ 'Full-Stack/Software',
                          TRUE ~ role))

ground_truth <- ground_truth %>% 
  inner_join(ground_truth %>% 
               select(role, `Rate (%)`) %>% 
               unique() %>% 
               group_by(role) %>% 
               summarise(`Rate (%)` = sum(`Rate (%)`)),
             by = 'role') %>% 
  rename(rate = `Rate (%).y`) %>% 
  select(role, skill, rate) %>% 
  unique()
  
# How many skills per role?
ground_truth %>% 
  group_by(role) %>% 
  summarise(num_skills_per_role = n_distinct(skill)) %>% 
  group_by(num_skills_per_role) %>% 
  summarise(num_roles_per_num_skills_per_role = n_distinct(role)) %>% 
  ggplot(aes(x = num_skills_per_role, y = num_roles_per_num_skills_per_role)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(limits = c(0, 15)) +
  labs(x = '... Have X Skills', y = 'Y Roles...') +
  theme_light()


# How popular are the skills as tags in Stack Overflow?
pop_1 <- df %>% 
  select(Tag, tag_count) %>% 
  unique() %>% 
  arrange(desc(tag_count)) %>% 
  mutate(tag_rank = row_number()) %>% 
  inner_join(ground_truth, by = c('Tag' = 'skill')) %>% 
  group_by(role) %>% 
  mutate(role_popularity = sum(tag_count)) %>% 
  arrange(desc(role_popularity), role, tag_rank) %>% 
  group_by(role) %>% 
  mutate(tag_popularity_within_role = row_number(),
         median_tag_rank = median(tag_rank)) %>%
  select(role, median_tag_rank, rate) %>% 
  unique() %>% 
  arrange(median_tag_rank) %>% 
  mutate(role_and_rate = paste0(role, ' (', rate, '%)')) %>% 
  ggplot(aes(x = reorder(role_and_rate, rate), y = median_tag_rank)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  scale_y_continuous(labels = label_number_si()) +
  labs(y = 'Mean Stack Overflow Tag Rank', x = 'Role') +
  theme_light()

pop_2 <- ground_truth %>% 
  mutate(role_and_rate = paste0(role, ' (', rate, '%)')) %>% 
  select(role_and_rate, rate) %>% 
  unique() %>% 
  ggplot(aes(x = reorder(role_and_rate, rate), y = rate)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  scale_y_continuous(labels = label_number_si()) +
  labs(y = 'Rate', x = 'Role') +
  theme_light()


pop_1 + pop_2

