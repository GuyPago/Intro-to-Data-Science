# Guy Taggar 206260762

library(tidyverse)
library(readxl)
setwd('Intro to Data Science/HW/HW1/')

# Q1
HPRICE <- read_excel('HPRICE1.xlsx', sheet = 'Data')
print(paste('HPRICE contains' , nrow(HPRICE) , 'observations.')) 


# Q2
HPRICE <- select(HPRICE, -lprice, -lassess, -llotsize, -lsqrft )


# Q3
five_BR_prc <- round(100*(nrow(filter(HPRICE, bdrms > 5))/nrow(HPRICE)),
                     digits = 1 )
print(paste(five_BR_prc,'% ', 'of the houses have more than 5 bedrooms.',
            sep = ''))
HPRICE <- filter(HPRICE, bdrms <= 5)


#Q4
avg_price_col_2000_sqrft <- mean((HPRICE %>% filter(colonial == 1, sqrft > 2000))$price)
print(paste('The average price of a colonial house with more than 2000 sqrft is approx ',
            round(avg_price_col_2000_sqrft),'$', sep = ''))


#Q5
HPRICE <- arrange(HPRICE, desc(lotsize))
# Our data is arranged by descending lot size, thus the house
#  with the biggest lot size should appear on the first row. namely:
print(paste('The assessed value of the house with the biggest lot size is ',
            HPRICE[1,]$assess, '$', sep = '')) 


#Q6 

print(paste(nrow(filter(HPRICE, is.na(lotsize))),
            'houses are missing the lot size value')) 

HPRICE <- filter(HPRICE, !is.na(lotsize))   


#Q7
colonial_ppsqrft_160 <- nrow(HPRICE %>% mutate(pp_sqrft = price/sqrft) %>%
  filter(colonial == 1, pp_sqrft > 160))
print(paste(colonial_ppsqrft_160,
            'colonial houses have a price per sqrft above 160'))
  

#Q8
HPRICE <- HPRICE %>% mutate(pp_bdrm = price/bdrms) %>%
  arrange(pp_bdrm)


#Q9
ggplot(HPRICE) +
  geom_point(mapping = aes(x=sqrft, y=assess))


#Q10
HPRICE %>% mutate(is.colonial = colonial==1) %>%
  ggplot(., mapping = aes(x=sqrft,
                          y=assess,
                          color = is.colonial)) +
  geom_point() +
  geom_smooth(method = 'lm', se=FALSE)


#Q11
HPRICE %>% mutate(is.colonial= colonial==1, size.type = ifelse(bdrms > median(bdrms),
                                     'Many bedrooms',
                                     'Few bedrooms')) %>%
  ggplot(.,mapping = aes(x=sqrft,
                          y=assess,
                          size = lotsize,
                          color = is.colonial)) +
  geom_point() +
  facet_wrap(~size.type) +
  geom_smooth(method = 'lm', se=FALSE)+
  
  #Q12
  ggtitle('sqrft ~ assess with respect to number of bedrooms')
