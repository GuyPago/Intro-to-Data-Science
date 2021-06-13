library(tidyverse)
library(readxl)

# Functions:
OECD_clean_data <- function(filename, oecd_countries_list, sheet = 1) {
  
  # load the data
  OECD_Data <- read_excel(filename, sheet = sheet, skip = 4) %>%
    slice(-1) %>%
    select(-2,-3) %>%
    rename(Country = Time)
  
  # keep only OECd countries
  OECD_Data <- OECD_Data %>%
    inner_join(oecd_countries_list, by = "Country")
  
  return(OECD_Data)
}
OECD_tidy_data <- function(OECD_Data, value) {
  
  tidyData <- OECD_Data %>%
    gather(key = "Year", value = "Level", -Country) %>%
    mutate(Year = as.numeric(Year),
           Level = as.numeric(Level))
  
  # change the name of the 3rd variable
  names(tidyData)[3] = value
  
  return(tidyData)
}

country_list <- read_excel('Data/OECDData.xlsx') %>%
  select(Country)

country_cont <- read_excel('Data/OECDData.xlsx') %>%
  select(Country, Continent)

# ******Q1******
df_pop <- OECD_clean_data('Data/OECD_GDP_Population.xlsx', country_list, 3) %>%
  OECD_tidy_data(value = 'Population')

df_GDP <- OECD_clean_data('Data/OECD_GDP_Population.xlsx', country_list, 4) %>%
  OECD_tidy_data(value = 'GDP')


# ******Q2******

df <- df_GDP %>%
  inner_join(df_pop, by = c('Country', 'Year')) %>%
  mutate(GDP_per_capita = (GDP * 1000000) / (Population * 1000))


# ******Q3******
best_4 <- df %>%
  left_join(country_cont, by = 'Country') %>%
  select(1,6,2,3,4,5) %>%
  filter(Year == max(Year)) %>%
  group_by(Continent) %>%
  filter(GDP_per_capita == max(GDP_per_capita))


# ******Q4******
df <- df %>%
  filter(Country %in% best_4$Country)

df <-  df %>%
  group_by(Year) %>%
  summarize(across(is.numeric, mean, na.rm = TRUE)) %>%
  mutate(Country = 'Average') %>%
  bind_rows(df) %>%
  select(5,1,2,3,4) %>%
  arrange(Year)


# ******Q5******
Growth_df <- df %>%
  filter(Year %in% c(min(Year), max(Year))) %>%
  group_by(Country) %>%
  select(-GDP, -Population) %>%
  mutate(Period = max(Year) - min(Year)) %>%
  spread(key= 'Year', value = 'GDP_per_capita') %>%
  mutate(Growth = (`2019`/`1970`)^(1/Period) - 1) %>%
  select(-Period, -`1970`, -`2019`)


# ******Q6******
df %>%
  filter(Year == min(Year)) %>%
  left_join(Growth_df, by = 'Country') %>%
  select(-GDP, -Population) %>%
  mutate(exp_next_year = GDP_per_capita * (1+Growth))


# ******Q7******
df_2 <- df %>%
  filter(Year == min(Year)) %>%
  left_join(Growth_df) %>%
  select(-GDP, -Population)


lst <- list(df_2)
j=1
for (i in ((min(df$Year)+1):max(df$Year))){
lst[[length(lst) + 1]] <- df_2 %>%
    mutate(Year = i,
           GDP_per_capita = GDP_per_capita * (1+Growth)^j)
j = j+1
  
}

# Finally, the requested df:
df_expected <- bind_rows(lst) %>%
  select(-Growth)


# ******Q8******
df_comparison <- df_expected %>%
  rename(Trend = GDP_per_capita) %>%
  right_join(df, by = c('Country', 'Year')) %>%
  rename(Actual = GDP_per_capita) %>%
  select(-GDP,-Population)


# ******Q9******
df_comparison %>%
  ggplot() +
  geom_line(mapping = aes(x=Year, y=Actual, group = Country, color = Country)) +
  geom_line(mapping = aes(x=Year, y=Trend, group = Country, color = Country), linetype=2) +
  scale_color_manual(values = c("Average" = "grey75",
                                "Luxembourg" = "navyblue",
                                "Australia" = "red",
                                'United States' = 'green',
                                'Japan' = 'brown')) +
  ylab('GDP Per Capita') +
  ggtitle('GDP per capita', subtitle = 'Trend vs Actual')
  
