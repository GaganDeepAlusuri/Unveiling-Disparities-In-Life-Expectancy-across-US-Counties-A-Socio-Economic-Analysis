rm(list=ls())
library(rio)
library(dplyr)
d = import("Final2019-2023.xlsx") 
d <- d[complete.cases(d), ]

d$StateAndCounty  = paste(d$county, d$state,sep = ", ")
colnames(d)
#Converted # to %
d$percent.food.insecure                               =     (d$no.food.insecure / d$population) * 100.0
d$percent.adult.uninsured                             =     (d$no.adult.uninsured / d$population) * 100.0
d$percent.child.uninsured                             =     (d$no.children.uninsured / d$population) * 100.0
#d$percent.households.with.severe.cost.burden         =     (d$no.households.with.severe.cost.burden / d$population) * 100.0
d$percent.rural                                       =     (d$no.rural / d$population) * 100.0
d$average.number.of.physically.unhealthy.days         =     d$average.number.of.physically.unhealthy.days * 12
d$average.number.of.mentally.unhealthy.days           =     d$average.number.of.mentally.unhealthy.days * 12


#Checking correlation
str(d)
boxplot(life.expectancy~ factor(presence.of.water.violation), data = d)

str(d)
numeric_cols <- d %>% select(c(4,5,6,7,9,12,13,14,15,16,18,20,22,24,26,27,29,30,31,32,33,34,35,36,37,39,41,42,43,44))
library(openxlsx)
# Compute the correlation matrix using Pearson correlation
cor_matrix <- cor(numeric_cols, method = "pearson")
# Create an Excel workbook
wb <- createWorkbook()

# Add a sheet to the workbook
addWorksheet(wb, "Correlation Matrix")

# Write the column names to the sheet
writeData(wb, "Correlation Matrix", colnames(cor_matrix), startCol = 2, startRow = 1)

# Write the row names and correlation matrix to the sheet
writeData(wb, "Correlation Matrix", cbind(rownames(cor_matrix), cor_matrix), startCol = 1, startRow = 2)

# Save the Excel workbook
saveWorkbook(wb, "correlation_matrix.xlsx", overwrite = TRUE)

#High correlation between---
#Removing percent.frequent.mental.distress, percent.insufficient.sleep, population, percent, 
#average.number.of.physically.unhealthy.days, average.number.of.mentally.unhealthy.days,
#no.mental.health.providers, percent.food.insecure, percent.child.uninsured

#Converting to factors.
col <- c("year", "county", "state", "presence.of.water.violation")
d[col] <- lapply(d[col], factor)

attach(d)
library(lme4)
re <- lmer(life.expectancy ~ year + percent.frequent.physical.distress  +percent.diabetic  + percent.food.insecure+ percent.adult.uninsured+
             + household.income + no.households.with.severe.cost.burden + percent.65.and.over
           + percent.female +  percent.smokers + percent.adults.with.obesity + food.environment.index + percent.excessive.drinking +
             + no.primary.care.physicians  + percent.rural +
             + average.daily.pm2.5 + presence.of.water.violation
            + (1 | state), data=d, REML=FALSE)

ranef(re)

d$state = relevel(d$state, 'Minnesota')
linear <- lm(life.expectancy ~  state + year + percent.frequent.physical.distress + percent.diabetic  + percent.food.insecure+ percent.adult.uninsured+
                household.income + no.households.with.severe.cost.burden + percent.65.and.over
             + percent.female +  percent.smokers + percent.adults.with.obesity + food.environment.index + percent.excessive.drinking +
               + no.primary.care.physicians  + percent.rural +
               + average.daily.pm2.5 + presence.of.water.violation, data=d)





library(stargazer)
stargazer(linear, re, type="html",out="OutputOfModels.htm")

library('car')
vif(re)
library(gvlma)
residuals <- resid(re)
durbinWatsonTest(residuals)
