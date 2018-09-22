library(readr)
quartz <- read_csv("quartz-bad-data-guide.csv")

library(tidyr)
library(dplyr)
quartz <- quartz %>% 
  separate(issue, into = "issue", sep = "\\(#", extra = "drop") %>% 
  select(issue)
write_csv(quartz, "quartz-bad-data-checklist.csv")

