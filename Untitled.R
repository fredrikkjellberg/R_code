All my home works

# Loads iris data
data(iris)
# 
with(iris, table(Species == "setosa"))

with(iris,Species == "setosa")



#bootstrapping also known as resampling. 
for (i in 1:reps) {
  means[i] <- mean(sample(chisq[,1],40))
}
hist(means,main="Sampling Distribution of X-bar",xlab="Sample Mean")


foo <- cbind(c(7.5,6,5),c(1,2,3))
> foo
[,1] [,2]
[1,]  7.5    1
[2,]  6.0    2
[3,]  5.0    3
foo
# will print the 1st row,
foo[1,] # [1,]  7.5    1
will print the second colum
foo[,2] # [,2] 1     2     3


#compares each entry in the rst column of foo to one and inserts the row corresponding to each match into
#smallfoo. We can also reorder data. If wealth is a dataframe with columns year, gdp, and gnp, we could
#sort the data by year using order() or extract a period of years using the colon operator
wealth <- wealth[order(wealth$year),]
firstten <- wealth[1:10,]
eighty <- wealth[wealth$year==1980,]

# If goo is a 3x4 data frame with titles age, gender, education, and salary, then we can print the salary column with the command
goo$salary
goo[,4]

