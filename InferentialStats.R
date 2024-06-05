library(rmarkdown)
library(readxl)
library(tidyverse)
library(dplyr)
library(knitr)
library(kableExtra)
library(magrittr)
library(RColorBrewer)
library(tables)
library(ggpubr)
library(corrgram)
library(psych)
library(pander)
library(dplyr) 
library(plotrix)
library(ggplot2)
library(psych)
library(broom)

df = read_excel("R Studio/stormscleaned.xlsx")
# Question 1 :Hypothesis test for wind speeds between tropical depressions and tropical storms

# Subset the data for tropical depressions and tropical storms
depressions = df[df$status == "tropical depression", "wind"]
storms = df[df$status == "tropical storm", "wind"]

t.test(depressions, storms)

# Question 2 :Hypothesis test for the effect of storm category on storm pressure

# Perform linear regression
lm_test = lm(pressure ~ as.factor(status), data = df)

# Check the significance of the storm category variable
summary(lm_test)

# question 3 :Hypothesis test for the correlation between wind speed and storm pressure
cor.test(df$wind, df$pressure, method = "pearson")
  






