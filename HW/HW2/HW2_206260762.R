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


# ***** Q5 *****
df %>%
  group_by(Population_sector) %>%
  ggplot(mapping = aes(x=Age_group, weight = weight, fill=Population_sector)) +
  geom_bar(position = 'fill') +
  ggtitle('Population rate by age group') +
  ylab('Percent')

# This is the calculation corresponding to the plot
df %>%
  group_by(Age_group) %>%
  mutate(pop_total = sum(weight)) %>%
  group_by(Population_sector, Age_group) %>%
  summarize(People = sum(weight), pop_total = first(pop_total)) %>%
  mutate(pop_share = People / pop_total) %>%
  select(Population_sector, Age_group, pop_share)


# ***** Q6 *****
df %>%
  filter(Gender == 'Male') %>%
  group_by(Age_group, Population_sector) %>%
  summarize(Employed ,emp_rate = weighted.mean(Employed == 1)) %>%
  ggplot(mapping = aes(x=Age_group, y=emp_rate, fill = Population_sector)) +
  geom_col(position = 'dodge') +
  ggtitle('Employment rate by age group and sector')
  


# ***** Q7 *****
#Filter data
df <- df %>%
  filter(!is.na(Hourly_wage), !is.na(Monthly_hours)) %>%
  mutate(monthly_income = Hourly_wage * Monthly_hours)

df %>%
  ggplot(mapping=aes(x=monthly_income, weight=weight)) +
  geom_histogram() +
  ggtitle('Monthly income')


# ***** Q8 *****
# Average monthly income, monthly working hours and hourly wage data frame
df %>%
  summarize(avg_month_inc = mean(monthly_income),
            avg_work_hrs = mean(Monthly_hours),
            avg_hourly_wage = mean(Hourly_wage))


# ***** Q9 *****

df %>%
  group_by(Gender, Population_sector) %>%
  summarize(avg_month_inc = round(weighted.mean(monthly_income,weight), digits = 0)) %>%
  ggplot(mapping = aes(x=Gender, y=avg_month_inc)) +
  geom_col() +
  facet_wrap(~ Population_sector) +
  geom_text(aes(label= avg_month_inc, vjust=1.5), color='white') +
  ggtitle('Monthly income by gender and sector')


# ***** Q10 *****
df <- df %>%
  filter(Gender == 'Male', Population_sector != 'Arabs')


# ***** Q11 *****
# The 'check_mean' column checks whether the result contradicts Q9.

orthodox.df <- df %>%
  filter(Population_sector == 'Ultra-orthodox') %>%
  mutate(all_weights =sum(weight)) %>%
  group_by(branch) %>%
  mutate(sector_sum = sum(weight)/all_weights) %>%
  summarize(share = first(sector_sum), monthly_income = weighted.mean(monthly_income, weight)) %>%
  mutate(check_mean = weighted.mean(monthly_income, share))


# ***** Q12 *****
orthodox.df <- orthodox.df %>%
  select(-4)


orthodox.df %>%
  mutate(QTR = ntile(monthly_income, 4)) %>%
  mutate(QTR = factor(QTR, levels = c(1,2,3,4), ordered = 'True')) %>%
  ggplot(mapping = aes(x=branch, y=share, fill=QTR)) +
  geom_col() +
  ggtitle('Employment share per branch', subtitle = ' Orthodox')



# ***** Q13 *****

non_ortho.df <- df %>%
  filter(Population_sector == 'Non-orthodox Jews', branch %in% orthodox.df$branch) %>%
  mutate(all_weights =sum(weight)) %>%
  group_by(branch) %>%
  mutate(sector_sum = sum(weight)/all_weights) %>%
  summarize(share = first(sector_sum), monthly_income = weighted.mean(monthly_income, weight))


# ***** Q14 *****
orthodox.df %>%
  mutate(QTR = ntile(monthly_income, 4)) %>%
  mutate(QTR = factor(QTR, levels = c(1,2,3,4), ordered = 'True')) %>%
  ggplot() +
  geom_col(mapping = aes(x=branch, y=share, fill=QTR)) +
  geom_point(mapping = aes(x=branch, y=non_ortho.df$share), color='red')
  ggtitle('Employment share per branch', subtitle = 'With dots marking non-orthodx share')

  
# ***** Q15 *****
orthodox.wage.plot <- orthodox.df %>%
    ggplot(mapping = aes(x=branch, y=monthly_income)) +
    geom_col() +
    geom_hline(yintercept = weighted.mean(orthodox.df$monthly_income, orthodox.df$share),
               color='red') +
    ggtitle('Average monthly income per branch', subtitle = 'Orthodox')


# ***** Q16 *****
improved_wage <- orthodox.df %>%
  inner_join(non_ortho.df, by='branch') %>%
  summarize(weighted.mean(monthly_income.x, share.y))%>%
  pull()
  


# ***** Q17 *****
orthodox.wage.plot +
  geom_hline(yintercept = improved_wage, color = 'green') +
  annotate(geom = 'text',
           y = improved_wage + 700,
           x = 0.5,
           hjust = 0,
           label = 'Improved',
           color = 'green') +
  
  annotate(geom = 'text',
           y = weighted.mean(orthodox.df$monthly_income, orthodox.df$share) + 700,
           x = 0.5,
           hjust = 0,
           label = 'Today',
           color = 'red')
  
  