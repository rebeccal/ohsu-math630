library(moderndive)
library(tidyverse)
library(skimr)

crimenames <- c("county", "region_name", "region_code",
                "criminals", "public_houses", "school_attendance",
                "worship_attendance")

crime <- read_table("https://ohsu-math630-fall-2018.netlify.com/data/beerhall.dat", col_names=crimenames)

# crime <- read_table(here::here("data", "beerhall.dat"), col_names = crimenames)


#4.1
glimpse(crime)

#4.2
crime %>% 
  select(criminals, public_houses) %>% 
  skim()

crime %>% 
  summarize(corr = cor(criminals, public_houses))

#4.3
ggplot(crime, aes(x = public_houses, y = criminals)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)