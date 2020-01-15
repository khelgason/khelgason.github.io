library(readr)
library(dplyr)
library(scales)
library(forcats)

d_calendar <- read_csv('data/d_calendar.csv')
d_parking <- read_csv('data/d_parking.csv')
f_parking <- read_csv('data/f_parking_usage.csv')

summary(d_parking)
table(d_parking$gjaldskyld, useNA = 'always')

# Change non ordinary numerics to factors and handle NAs
d_parking$gjaldskyld <- factor(d_parking$gjaldskyld, 
                               labels = c('Ógjaldskylt', 'Gjaldskylt')) %>%
  forcats::fct_explicit_na('(Vantar)')

d_parking$eigandi <- factor(d_parking$eigandi) %>% 
  forcats::fct_explicit_na("(Vantar)")

d_parking$postnumer <- factor(d_parking$postnumer)

# Parking per postnumer
t_parking_by_pn <- d_parking %>% 
  group_by(postnumer) %>% 
  tally() %>% 
  mutate(Hlutfall = n / sum(n),
         Hlutfall = percent(Hlutfall, accuracy = 0.1))


# Parking per eigandi
t_parking_by_eig <- d_parking %>% 
  group_by(eigandi) %>% 
  tally() %>% 
  arrange(-n) %>% 
  mutate(Hlutfall = n / sum(n),
         Hlutfall = percent(Hlutfall, accuracy = 0.01))

