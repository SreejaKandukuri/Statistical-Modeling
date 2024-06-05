

install.packages("MASS")
library(MASS)

#accessing the cats dataset
data(cats)


# Subset the dataset to create separate vectors for male and female cat body weights
male = subset(cats, subset = (cats$Sex == "M"))$Bwt
female = subset(cats, subset = (cats$Sex == "F"))$Bwt

task1result = t.test(male, female)

print(task1result)

#task 2 
beforeworkshop = c(4.6, 7.8, 9.1, 5.6, 6.9, 8.5, 5.3, 7.1, 3.2, 4.4)
afterworkshop = c(6.6, 7.7, 9.0, 6.2, 7.8, 8.3, 5.9, 6.5, 5.8, 4.9)

task2result = t.test(beforeworkshop, afterworkshop, paired = TRUE)

print(task2result)

#histogram
hist(main= "Frequency of Cats Body Weight", cats$Bwt ,xlab="Body Weight" )
#boxplot 
boxplot(cats$Hwt)

