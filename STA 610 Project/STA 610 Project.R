library(tidyverse)
library(haven)
library(stats)

library(haven)

gvsu <- read_sav(here::here("STA 610 Project", "GVSU.sav"))

sum(is.na(gvsu))
apply(gvsu, 2, function(col)sum(is.na(col))/length(col))


recodegvsu <- gvsu %>%
  mutate(SEX = fct_recode(SEX,
                          "Female" = "F",
                          "Male" = "M"),
         YEAR = fct_recode(YEAR,
                           "Freshman" = "F",
                           "Sophomore" = "So",
                           "Junior" = "J",
                           "Senior" = "S"),
         BUS = fct_recode(BUS,
                          "Yes" = "Y",
                          "No" = "N"))

filtergvsu <- recodegvsu %>%
  mutate(CREDITCOST = (TUITION / CREDITS))%>%
  filter(TUITION < 150001, CREDITS > 0)


selectgvsu <- filtergvsu %>%
  select(SEX, YEAR, SLEEP, CREDITCOST, STUDY, BUS) %>%
  na.omit()

model <- lm(formula = SLEEP ~ CREDITCOST + SEX + YEAR + STUDY + BUS, data = selectgvsu)
summary(model)