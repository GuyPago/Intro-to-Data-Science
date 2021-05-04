# Lecture 6

library(tidyverse)
library(readxl)


GDP_per_capita <- read_excel('./Data/GDP_Per_Capita_OT.xlsx', skip=4)
GDP_per_capita <- GDP_per_capita %>%
  slice(-1) %>%
  select(-2,-3) %>%
  rename(country = Time) %>%
  print()
  

GDP_per_capita_tidy <- GDP_per_capita %>%
  gather(key = 'year', value = 'level', -country) %>%
  print()

GDP_per_capita_tidy <- GDP_per_capita_tidy %>%
  mutate(level = as.numeric(level)) %>%
  print()

# g = (Y2/Y1)^(1/n)-1
avg_growth_rates <- GDP_per_capita_tidy %>%
  filter(year %in% c(1970, 2019)) %>%
  spread(key=year, value=level) %>%
  print()

avg_growth_rates <- avg_growth_rates %>%
  mutate(growth_rate = 100*((`2019`/`1970`)^(1/(2019-1970))-1)) %>%
  select(country, growth_rate)%>%
  na.omit() %>%
  print()


first_year <- min(as.numeric(GDP_per_capita_tidy$year))
last_year <- max(as.numeric(GDP_per_capita_tidy$year))

avg_growth_rates <- GDP_per_capita_tidy %>%
  filter(year %in% c(first_year, last_year)) %>%
  spread(key=year, value=level) %>%
  print()

avg_growth_rates %>%
  mutate(first_year_thousands = get(as.character(first_year)) / 1000) %>%
  print()

avg_growth_rates <- avg_growth_rates %>%
  mutate(growth = (100*(get(as.character(last_year))/get(as.character(first_year))^(1/(last_year / first_year))-1))) %>%
  select(country,growth) %>%
  na.omit() %>%
  print()


avg_growth_rates %>% 
  ggplot(aes(x=country, y=growth)) +
  geom_col() +
  ggtitle('Average annual growth rates',
          subtitle = paste(first_year, '-', last_year))

avg_growth_rates %>% 
  ggplot(aes(x=country, y=growth)) +
  geom_col() +
  coord_flip() +
  ggtitle('Average annual growth rates',
          subtitle = paste(first_year, '-', last_year))

avg_growth_rates %>% 
  ggplot(aes(x=country, y=growth)) +
  geom_col() +
  coord_flip() +
  ggtitle('Average annual growth rates',
          subtitle = paste(first_year, '-', last_year))


