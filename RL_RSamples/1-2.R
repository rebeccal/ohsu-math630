library(tidyverse)
bakers_dozen <- read_csv("http://bit.ly/conj620-bakers-dozen")

our_data <- bakers_dozen %>%
filter(dataset==1)

# One way
our_data %>%
  summarise(x_mean=mean(x), x_sd=sd(x), y_mean=mean(y), y_sd=sd(y))

# Another way
summarise(our_data, x_mean=mean(x), x_sd=sd(x), y_mean=mean(y), y_sd=sd(y))

# Yet another
summarise(our_data, mean(x), sd(x), mean(y), sd(y))

#And...
our_data %>%
  summarize_all(funs(mean, sd))


our_data %>% 
ggplot() +
  geom_point(aes(x,y))


# All
bakers_dozen %>%
  group_by(dataset) %>%
  summarize_all(funs(mean, sd))


bakers_dozen %>% 
  ggplot() +
  geom_point(aes(x,y)) +
  facet_wrap(~ dataset)