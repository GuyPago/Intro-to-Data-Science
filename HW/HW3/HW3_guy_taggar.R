library(tidyr)
library(readxl)
library(tidyverse)
library(writexl)
setwd('Intro-to-Data-Science/')


# ***** Q1 *****
african_countries <- read_excel('Data/African countries.xlsx')
df <- read_excel('Data/population-by-broad-age-group.xlsx')

# checks for missing countries
african_countries %>%
  anti_join(df, by='Country')
unique(df$Country)

# Recode and create the desired data.
df <- df %>%
  mutate(Country = recode(Country, 'Democratic Republic of Congo' = 'DR Congo')) %>%
  inner_join(african_countries, by='Country')



# ***** Q2 *****
df <- df %>%
  gather(key = 'Age_group', value = 'Level', -Code, -Country, -Subregion, -Year) %>%
  mutate(Age_group = factor(Age_group, c("65_or_over","25_64", "15_24", "5_14", "Under_5"), ordered = TRUE))

df %>%
  filter(Year == 1950,
         Subregion == "Eastern Africa" ) %>%
  ggplot(mapping = aes(x = Country, y = Level, fill = Age_group)) +
  geom_col(position = 'fill') +
  ggtitle('Age group share',
          subtitle = 'Eastern Africa countries') 



# ***** Q3 *****
year <- max(df$Year)

df_max_year <- df %>%
  filter(Year == max(Year))

df_max_1 <- df_max_year %>%
  summarize(Country = 'avg_all', Level = mean(Level), Subregion = 'All regions') %>%
  bind_rows(df_max_year)


df_max <- df_max_1 %>%
  group_by(Subregion) %>%
  summarize(Level = mean(Level)) %>%
  rename(Country = Subregion) %>%
  filter(Country != 'All regions') %>%
  mutate(Subregion = 'Sub regions') %>%
  bind_rows(df_max_1)

df_max %>%
  ggplot(mapping = aes(x=reorder(Country, Level),
                       y=Level,
                       fill = Subregion)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  ggtitle('Total population of countries',
          subtitle = paste('Africa', year)) +
  scale_fill_manual(values = c("Eastern Africa" = "grey45",
                               "Northern Africa" = "grey45",
                               "Middle Africa" = "grey45",
                               "Southern Africa" = "grey45",
                               "Western Africa" = "grey45",
                               'Sub regions' = 'navyblue',
                               'All regions' = 'darkgreen'))
  
rm(df_max_1, df_max_year)



# ***** Q4 *****

df %>%
  filter(Year %in% c(min(Year), max(Year))) %>%
  group_by(Subregion, Year) %>%
  summarize(Level = sum(Level)) %>%
  mutate(Period = max(Year) - min(Year),
         Ref = ifelse(Year == min(Year), 'First', 'Last')) %>%
  select(-Year) %>%
  spread(key = Ref, value = Level) %>%
  mutate(Growth = 100*((Last/First)^(1/Period) - 1)) %>%
  select(-First, -Last) %>%
  ggplot(mapping = aes(x = Subregion, y = Growth)) +
  geom_col() +
  ggtitle('Average growth rate by Subregion',
          subtitle = paste(max(df$Year) - min(df$Year), 'Years')) +
  geom_text(mapping = aes(label = paste(round(Growth, digits = 2), '%')), vjust = -0.2)
  
  

# ***** Q5 *****

lst <- list()
year_vec <- c(1950, 1960, 1970, 1980, 1990, 2000, 2010) 
i = 1
for (j in year_vec) {
  lst[[i]] <- df %>%
    filter(Year %in% c(j, j + 10)) %>%
    group_by(Country, Year) %>%
    summarize(Level = sum(Level)) %>%
    mutate(Period = max(Year) - min(Year),
           Ref = ifelse(Year == min(Year), 'First', 'Last'),
           Decade = paste(j, '-', j + 10, sep='')) %>%
    select(-Year) %>%
    spread(key = Ref, value = Level) %>%
    mutate(Growth = 100*((Last/First)^(1/Period) - 1)) %>%
    select(-First, -Last, -Period)
  i = i + 1
}

# Finally, the requested DF:
df_growth_decade <- bind_rows(lst)



# ***** Q6 *****
df_growth_decade <- df_growth_decade %>%
  spread(key = Decade, value = Growth)

write_xlsx(df_growth_decade,
           path = './Results/Country_growth_decade.xlsx')


# ***** Q7 *****
Africa_pop_20 <- df %>%
  filter(Year == max(Year)) %>%
  summarize(Level = sum(Level)) %>%
  pull()

Africa_pop_21 <- df %>%
  filter(Year == max(Year)) %>%
  group_by(Country) %>%
  summarize(Level = sum(Level)) %>%
  inner_join(df_growth_decade, by = 'Country') %>%
  filter(Decade == '2010-2020') %>%
  mutate(exp_pop = Level * (1+Growth/100)) %>%
  summarise(sum(exp_pop)) %>%
  pull()

print(paste('Africa is expected to have',
            round(Africa_pop_21),
            'people in 2021'))



# ***** Q8 *****

j <- 1
i <- Africa_pop_20
while(i < 2 * Africa_pop_20) {
  i <- df %>%
    filter(Year == max(Year)) %>%
    group_by(Country) %>%
    summarize(Level = sum(Level)) %>%
    inner_join(df_growth_decade, by = 'Country') %>%
    filter(Decade == '2010-2020') %>%
    mutate(exp_pop = Level * (1+Growth/100)^j) %>%
    summarise(sum(exp_pop)) %>%
    pull()
  j <- j + 1
}

print(paste('It will take', j,
            'years for Africa to double its population'))
