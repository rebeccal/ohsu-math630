library(moderndive)
library(tidyverse)
library(skimr)

crimenames <- c("county", "region_name", "region_code",
                "criminals", "public_houses", "school_attendance",
                "worship_attendance")

crime <- read_table("https://ohsu-math630-fall-2018.netlify.com/data/beerhall.dat", col_names=crimenames)

# crime <- read_table(here::here("data", "beerhall.dat"), col_names = crimenames)

#From Lab 2.1 = 2.2-2

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
  geom_text(aes(label=county)) +
  geom_smooth(method = "lm", se = FALSE)



















#New numbering for Lab 2.2

#3.1
beerhall_lm <- crime %>% 
  lm(criminals ~ public_houses,.)

get_regression_table(beerhall_lm)

#4.1
beerhall_lm_pts <- get_regression_points(beerhall_lm)

beerhall_lm_pts %>%
  filter(ID==20 | ID==23) %>% 
  arrange(ID)

#4.2
beerhall_lm_pts %>%
  filter(criminals==min(criminals) | residual==min(residual)) 

#5.1
ggplot(beerhall_lm_pts, aes(x=public_houses, y=residual)) +
  geom_text(aes(label=ID)) +
  geom_hline(yintercept = 0, col = "blue")

#SOOoo risky - need to check resulting df for dups/mismatches, because public_house+criminals is NOT a key  
lm_join <- inner_join(beerhall_lm_pts,crime)

ggplot(lm_join, aes(x=public_houses, y=residual)) +
  geom_text(aes(label=county)) +
  geom_hline(yintercept = 0, col = "blue")


#5.2
ggplot(beerhall_lm_pts, aes(residual)) +
  geom_density() +
  geom_vline(xintercept = 0, col="red")


