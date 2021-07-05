
library(tidyverse)
library(readxl)


# *****Q1*****
PWT <- read_excel('Data/pwt100.xlsx', sheet = 'Data') %>%
  select(-rgdpe) %>%
  rename(gdp = rgdpo) %>%
  mutate(gdp_per_capita = gdp / pop) %>%
  print()

# *****Q2*****
Income <- read_excel('Data/country_categories.xlsx', sheet = 'income')
Continent <- read_excel('Data/country_categories.xlsx', sheet = 'continent')

PWT <- PWT %>%
  inner_join(Income, by = 'code') %>%
  inner_join(Continent, by = 'code')
  

# *****Q3*****
PWT %>%
  group_by(year) %>%
  filter(!is.na(pop)) %>%
  summarize(count = n())

# Method 2 - (Almost) one liner :)
pop.per.year <- PWT %>%
  group_by(year) %>%
  summarise(count = sum(!is.na(pop)))

amount <- 100
over_100_year <- pop.per.year %>%
  filter(count >= amount) %>%
  filter(year == min(year)) %>%
  select(year) %>%
  pull()

print(paste(over_100_year, 'was the first year to feature population data',
            'of more than',amount, 'countries'))  


# *****Q4*****
PWT %>%
  filter(year == over_100_year) %>%
  ggplot(mapping = aes(x=hc, y=gdp_per_capita)) +
  geom_point() +
  geom_smooth(method = 'lm' ,se = FALSE, mapping = aes(group = income)) +
  xlab('Human capital index') +
  ylab('GDP per capita (M$)') +
  ggtitle('GDP per capita as a function of Human capital index',
          subtitle = paste(over_100_year, ', regression line per income group'))


# *****Q5*****
gdp_share_chart <- PWT %>%
  filter(year == over_100_year) %>%
  group_by(continent) %>%
  summarise(gdp.share = 100 * sum(gdp, na.rm = TRUE) / sum(.$gdp, na.rm = TRUE)) %>%
  ggplot(mapping = aes(x=continent, y=gdp.share)) +
  geom_col() +
  geom_text(mapping=aes(label = paste(round(gdp.share), '%', sep = '')),
            vjust = -0.4) +
  theme(axis.title.x = element_blank()) +
  ylab('GDP share') +
  ggtitle('Global GDP share by continent', subtitle = over_100_year)
  

# *****Q6*****
starting_year <- 2000

HC_change <- PWT %>%
  filter(year %in% c(starting_year,max(year))) %>%
  group_by(country) %>%
  select(country, continent, year, hc,) %>%
  spread(key = year, value = hc) %>%
  mutate(hc_change = get(as.character(max(PWT$year))) - get(as.character(starting_year))) %>%
  select(country, continent, hc_change)


# *****Q7*****
HC_high_low <- HC_change %>%
  group_by(continent) %>%
  filter(hc_change %in% c(min(hc_change, na.rm = TRUE), max(hc_change, na.rm = TRUE)))

HC_high_low <- HC_change %>%
  ungroup() %>%
  summarise(country = 'Global Average',
            continent = 'Global Average',
            hc_change = mean(hc_change, na.rm = TRUE)) %>%
  bind_rows(HC_high_low)


# *****Q8*****
HC_high_low %>%
  ggplot(mapping=aes(x=reorder(country, hc_change), y=hc_change,
                     fill = ifelse(country == 'Global Average',
                                   'y', ifelse(hc_change > 0, 'n', 'k')))) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  ylab('HC Change') +
  theme(axis.title.y = element_blank()) +
  ggtitle('Change in Human capital index, by country',
          subtitle = paste(starting_year, '-', max(PWT$year)))
  

# *****Q9*****

# Particular case
PWT %>%
  filter(income == 'Advanced Economies') %>%
  group_by(year, continent) %>%
  summarize(income = income,
            gdp_per_capita = weighted.mean(gdp_per_capita, pop, na.rm = TRUE)) %>%
  ggplot(mapping = aes(x=year, y=gdp_per_capita, color=continent)) +
  geom_line() +
  theme(axis.title.x = element_blank(), legend.position = 'bottom') +
  ylab('GDP per capita (M$)') +
  ggtitle('Average GDP per capita by continent',
          subtitle = paste('Advanced Economies', 'Countries'))


# The general case
income_plots <- lst() 
for(i in unique(PWT$income)) {
  
  tmp <- PWT %>%
    filter(income == i) %>%
    group_by(year, continent) %>%
    summarize(income = income,
              gdp_per_capita = weighted.mean(gdp_per_capita, pop, na.rm = TRUE)) %>%
    ggplot(mapping = aes(x=year, y=gdp_per_capita, color=continent)) +
    geom_line() +
    theme(axis.title.x = element_blank(), legend.position = 'bottom') +
    ylab('GDP per capita (M$)') +
    ggtitle('Average GDP per capita by year',
            subtitle = paste(i , 'Countries'))
  
  income_plots[[length(income_plots) + 1]] <- tmp
}  

print(paste('The list contains', length(income_plots), 'plots'))



# *****Q10*****
PWT %>%
  group_by(year, income) %>%
  summarise(gdp_per_capita = weighted.mean(gdp_per_capita, pop, na.rm = TRUE)) %>%
  spread(key = income, value = gdp_per_capita) %>%
  mutate(advanced_low = `Low Income Developing Countries` / `Advanced Economies`,
         advanced_medium = `Emerging and developing economies` / `Advanced Economies`) %>%
  select(-2,-3,-4) %>%
  gather(key = 'Ratio_type', value = 'Ratio', -year) %>%
  ggplot(mapping=aes(x=year, y=Ratio, color=Ratio_type)) +
  geom_line() +
  theme(legend.position = 'bottom', axis.title.x = element_blank()) +
  ggtitle('GDP per capita ratio of low and emerging countries',
          subtitle = ' in comparison to advanced economics')

  
