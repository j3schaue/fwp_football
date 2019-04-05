###--------------------------------------------------------------###
###--------------------------------------------------------------###
###--------------------------------------------------------------###
### EDA: Data visualizations
###--------------------------------------------------------------###
###--------------------------------------------------------------###
###--------------------------------------------------------------###

# install.packages('tidyverse')
library(tidyverse)

##----------------------------------------##
## PFR Example
##----------------------------------------##
receiving = readRDS("./data/PFR/pfr_receiving_1985_2018.RDS")

head(receiving)

ggplot(receiving) + 
  geom_point(aes(tgt, y_r)

ggplot(receiving) + 
  geom_point(aes(tgt, y_r, color = as.factor(age > 30)), alpha = 0.5)

ggplot(receiving) + 
  geom_smooth(aes(tgt, y_r))

ggplot(receiving) + 
  geom_smooth(aes(tgt, y_r, color = as.factor(age > 30)))


### Passing
passing = readRDS("./data/PFR/pfr_passing_1985_2018.RDS")
head(passing)

ggplot(passing) + 
  geom_point(aes(int, td, color = as.factor(age > 30)))

ggplot(passing) + 
  geom_jitter(aes(int, td, color = as.factor(age > 30)))

ggplot(passing) + 
  geom_density(aes(int/td, color = as.factor(age > 30)))

ggplot(filter(passing, td > 0)) + 
  geom_smooth(aes(age, int/td))


### Expanding data
passing$probowl = grepl("\\*", passing$player)
passing$ft_allpro = grepl("\\+", passing$player)
passing$player = gsub("\\+|\\*", "", passing$player)

ggplot(passing) + 
  geom_point(aes(td, int, color = probowl))

ggplot(passing) + 
  geom_density2d(aes(td, int, color = probowl))

ggplot(passing) + 
  geom_point(aes(int, rate, color = probowl))



## Sportrac
stof = readRDS("./data/sportrac/offense.RDS")
head(stof)

ggplot(stof) + 
  geom_point(aes(age, yrs))

ggplot(stof) + 
  geom_point(aes(yrs, percent_gteed)) + 
  geom_smooth(aes(yrs, percent_gteed))

ggplot(filter(stof, age > 10)) + 
  geom_jitter(aes(age + yrs, percent_gteed)) + 
  geom_smooth(aes(age + yrs, percent_gteed))


### Data wrangling
# prep contract data and then join it to the PFR data
qbs = filter(stof, pos == "QB") %>%
  mutate(player = as.character(player)) %>%
  inner_join(passing, ., by = "player") %>%
  mutate(yr_rel = year - start_yr)

med_dollars = quantile(stof$dollars, 0.5)
q3_dollars = quantile(stof$dollars, 0.75)

head(qbs)

ggplot(filter(qbs, abs(yr_rel) < 3)) + 
  geom_smooth(aes(yr_rel, qbr, 
                color = factor(dollars > med_dollars)))

ggplot(filter(qbs, abs(yr_rel) < 3)) + 
  geom_smooth(aes(yr_rel, qbr, 
                  color = factor(dollars > q3_dollars)))

ggplot(filter(qbs, abs(yr_rel) < 3)) + 
  geom_smooth(aes(yr_rel, gs, 
                  color = factor(dollars > med_dollars)))

ggplot(filter(qbs, abs(yr_rel) < 3)) + 
  geom_smooth(aes(yr_rel, gs, 
                  color = factor(dollars > q3_dollars)))
  
