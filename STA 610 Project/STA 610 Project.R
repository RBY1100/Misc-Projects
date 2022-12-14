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

mutategvsu <- gvsu %>%
  mutate(CREDITCOST = TUITION / CREDITS)%>%
  filter(TUITION < 150001, CREDITS > 0)

mutategvsu1 <- gvsu %>%
  mutate(SLEEPMINUTES = SLEEP * 60)

mutategvsu2 <- gvsu %>%
  mutate(GOODSLEEP = ifelse(SLEEP >= 8, "Yes", "No"))
         

filtergvsu <- gvsu %>%
  filter(SEX == "F")

filter1gvsu <- gvsu %>%
  filter(TUITION <= 150000)

filter2gvsu <- gvsu %>%
  filter(SEX =="F", TUITION <= 150000)



selectgvsu <- gvsu %>%
  select(SEX, YEAR, SLEEP, STUDY, BUS)

select1gvsu <- gvsu %>%
  select(-SEX)


recodegvsu <- mutate(gvsu, SEX = fct_recode(SEX,
                                            "Female" = "F",
                                            "Male" = "M"))
filtergvsu <- mutate(recodegvsu, CREDITCOST = (TUITION / CREDITS))
filter1gvsu <- filter(filtergvsu, TUITION < 150001, CREDITS > 0)
finalgvsu <- select(filter1gvsu, SEX, YEAR, SLEEP, CREDITCOST, STUDY, BUS)


finalgvsu <- gvsu %>%
  mutate(SEX = fct_recode(SEX,
                          "Female" = "F",
                          "Male" = "M")) %>%
  mutate(CREDITCOST = (TUITION / CREDITS)) %>%
  filter(TUITION < 150001, CREDITS > 0) %>%
  select(SEX, YEAR, SLEEP, CREDITCOST, STUDY, BUS)







model <- lm(formula = SLEEP ~ CREDITCOST + SEX + YEAR + STUDY + BUS, data = selectgvsu)
summary(model)