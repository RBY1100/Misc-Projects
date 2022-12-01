#Regression

chicago <- read_xlsx(here::here("Chicagodata.xlsx"))

model <- lm(Price_Index ~ Unemployment, data=chicago)
summary(model)

model <- lm(Price_Index ~ Income_per_Capita, data=chicago)
summary(model)

model <- lm(Price_Index ~ Population_Change, data=chicago)
summary(model)



dallas <- read_xlsx(here::here("Dallasdata.xlsx"))

model <- lm(Price_Index ~ Unemployment, data=dallas)
summary(model)

model <- lm(Price_Index ~ Income_per_Capita, data=dallas)
summary(model)

model <- lm(Price_Index ~ Population_Growth, data=dallas)
summary(model)



LA <- read_xlsx(here::here("LAdata.xlsx"))

model <- lm(Price_Index ~ Unemployment, data=LA)
summary(model)

model <- lm(Price_Index ~ Income_per_Capita, data=LA)
summary(model)

model <- lm(Price_Index ~ Population_Growth, data=LA)
summary(model)


US <- read_xlsx(here::here("USdata.xlsx"))

model <- lm(Price_Index ~ Unemployment, data=US)
summary(model)

model <- lm(Price_Index ~ Income_per_Capita, data=US)
summary(model)

model <- lm(Price_Index ~ Population_Growth, data=US)
summary(model)
