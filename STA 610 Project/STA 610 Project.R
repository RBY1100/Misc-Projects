library(tidyverse)
library(readxl)

gvsu <- read_excel(path = here::here("STA 610 Project", "GVSU (2).xlsx"))

subgvsu <- gvsu %>%
  select(STUDY)

recodegvsu <- gvsu %>%
  mutate(SEX = fct_recode(SEX,
                          "0" = "F",
                          "1" = "M"))

newgvsu <- gvsu %>%
  mutate(pct = (TEXTBOOKS / TUITION)*100)
