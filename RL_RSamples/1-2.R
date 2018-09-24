library(tidyverse)
bakers_dozen <- read_csv("http://bit.ly/conj620-bakers-dozen")

our_data <- bakers_dozen %>%
filter(dataset==1)

our_data %>%
summarize_all(funs(mean, sd))

our_data %>% 
ggplot() +
  geom_point(aes(x,y))


# Mine

bakers_dozen %>%
group_by(dataset) %>%
summarize_all(., funs(mean, sd))


bakers_dozen %>% 
  ggplot() +
  geom_point(aes(x,y)) +
  facet_wrap(~ dataset)