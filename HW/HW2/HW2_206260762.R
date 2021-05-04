library(tidyr)
library(readxl)
library(tidyverse)
setwd('../')


# ***** Q1 *****
df <- read_csv('Data/income_2017.csv') %>%
  filter(!Age_group %in% c('15-17', '18-24', '65+'))


# ***** Q2 *****
df <- df %>%
  mutate(Age_group = recode(Age_group,
                           '25-29'='25-35',
                           '30-34'='25-35',
                           '55-59'='55-64',
                           '60-64'='55-64')) %>%
  mutate(Age_group = factor(Age_group, levels = c('25-35',    
                                                  '35-44',    
                                                  '45-54',    
                                                  '55-64'), ordered = TRUE))
# ***** Q3 *****
df %>%
  ggplot(mapping = aes(x=Age_group, weight=weight)) +
  geom_bar() +
  ggtitle('People in the working age, by age groups')

age_pop <- df %>%
  summarize(sum(weight))
print(paste('There are', age_pop, 'people in the working age'))


# ***** Q4 *****

# We begin by creating the relevant y values

pop_share_by_ages <- df %>%
  group_by(Age_group) %>%
  summarize(sum(weight)/1000000)%>%
  pull()

pop_share_by_ages[1]
  
df %>%
  ggplot(mapping = aes(x=Age_group, weight=weight/1000000)) + 
  geom_bar() +
  annotate(geom = 'text',
           y=pop_share_by_ages[1] -0.02,
           x='25-35',
           label = round(pop_share_by_ages[1], digits = 2),
           color = 'white',) +
  
  annotate(geom = 'text',
           y=pop_share_by_ages[2] -0.02,
           x='35-44',
           label = round(pop_share_by_ages[2], digits = 2),
           color = 'white',) +
  
  annotate(geom = 'text',
           y=pop_share_by_ages[3] -0.02,
           x='45-54',
           label = round(pop_share_by_ages[3], digits = 2),
           color = 'white',) +
  
  annotate(geom = 'text',
           y=pop_share_by_ages[4] -0.02,
           x='55-64',
           label = round(pop_share_by_ages[4], digits = 2),
           color = 'white',)


df %>%
  group_by(Population_sector) %>%
  ggplot(mapping = aes(x=Age_group, weight = weight, fill=Population_sector)) +
  geom_bar(position = 'fill') +
  ggtitle('Population rate by age group') +
  ylab('Percent')

# This is the analytical calculation 
df %>%
  group_by(Age_group) %>%
  mutate(pop_total = sum(weight)) %>%
  group_by(Population_sector, Age_group) %>%
  summarize(People = sum(weight), pop_total = first(pop_total)) %>%
  mutate(pop_share = People / pop_total) %>%
  select(Population_sector, Age_group, pop_share)
