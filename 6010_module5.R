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
library(gridExtra)   
library(grid)        
library(pander)
library(psych)
library(ggplot2)
library(graphics)
library(corrplot)
library(corrgram)

df= read.csv("C:\\Users\\sreej\\OneDrive\\Documents\\Cloud Computing\\coviddataset.csv", header=TRUE, stringsAsFactors=FALSE)
df
summary(df)  #descriptive stats
str(df)      # first few values of all variables 
names(df)   #to know the variables of the dataset

# Subset the numeric variables for correlation analysis
numerics = c("Confirmed", "Deaths", "Recovered", "Active", "New.cases")

numericssubset = df[, numerics]

cor_table = cor(numericssubset)

print(cor_table)

corrplot(cor_table, method = "number", type = "lower", tl.col = "black" , tl.srt = 45)


write.csv(cor_table, "correlation_table.csv", row.names = TRUE)


getwd() #to check the working directory where the correlation table is saved


#TASK 2

# Load the required packages
library(lmtest)
library(sandwich)
install.packages("stargazer")
library(stargazer)

# Perform linear regression
model = lm(Deaths ~ Confirmed + Recovered + Active, data = df)

# Compute robust standard errors
robust_model = coeftest(model, vcov. = vcovHC(model, type = "HC3"))

# Generate regression table
reg_table = stargazer(robust_model, type = "text")

# Export the regression table to a file
writeLines(reg_table, "regression_table.txt")












