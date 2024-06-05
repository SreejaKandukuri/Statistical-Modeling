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

df = read.csv("C:\\Users\\sreej\\OneDrive\\Documents\\R Studio\\coviddataset.csv", header=TRUE, stringsAsFactors=FALSE)

deathsmean=mean(df$Deaths)
deathsmean    


# Conduct one-sample t-test
ttestresult = t.test(df$Deaths, mu = deathsmean)


print(ttestresult) 



#Hypothesis testing for another variable 
result2 = t.test(df$Recovered, mu = mean(df$Recovered))


print(result2)


#Two variables Sample Test assuming unequal variances  

t2result = t.test(df$Active, df$Confirmed, paired = TRUE)


print(t2result)

#hypothesis test 

t.test(df$Deaths, mu = 10000)$p.value

#Hypothesis testing for p-value using the Deaths Variable
specificvalue = 10   #specific value for comparison

# Perform the hypothesis test for the p-value
result = prop.test(sum(df$Deaths > specificvalue), length(df$Deaths))
print(result)
# Extract the test results
p_value = result$p.value
sample_proportion = result$estimate
# Print the test results
cat("p-value =", p_value, "Sample proportion:", sample_proportion, "\n")



