# =======================================================
# REFERENCE LEVELS
# =======================================================
# We'll do an example with the popcorn data set 
# (available in my lab folder on CTools).

# Import Popcorn Data
Popcorn <- read.delim(file.choose())
View(Popcorn)

# NOTE: Initial popcorn data may include some columns and rows incorrectly
# interpreted by R as NA's. So we remove these with the following command:
# This is mainly a technical step, not conceptually important.
PopDat<-Popcorn[1:32,1:5]

# R groups together regression and ANOVA in some sense, under the heading of 
# a "linear model," which is what lm stands for. What this means is that
# while in class, ANOVA is presented in terms of effects for each factor along
# with a grand mean, R thinks of things a little bit differently.
lm(X.Unpopped~Brand+Microwave,data=PopDat)

# Notice that even though the factors are "Brand" and "Microwave",
# the output of this command doesn't contain a number for Brand
# or Microwave. This is because the effects estimated are actually
# for something else. They are the effect of having the Brand factor
# level be Pop Secret, and the Microwave factor level be Room respectively.
# This means that if we wanted to predict the percent unpopped given that
# we know that the brand of popcorn is pop secret, and we popped it in
# the microwave in the room rather than the lounge, we would get our
# prediction by adding together the intercept value with these other two.
PopSecretRoomPred<-19.1666+0.8231-1.844

# What would we do to get a prediction for a bag of Fastco popcorn
# that was popped in the Lounge?
FastcoLoungePred<-
  
  # What would we do to get a prediction for a bag of Pop Secret popcorn
  # that was popped in the Lounge?
  PopSecretLoungePred<-
  
  # What would we do to get a prediction for a bag of Fastco popcorn
  # that was popped in the Room?
  FastcoRoomPred<-
  
  PredTable<-as.table(matrix(c(PopSecretRoomPred,PopSecretLoungePred,FastcoRoomPred,FastcoLoungePred),nrow=2,byrow=T))
colnames(PredTable)<-c("Room","Lounge")
rownames(PredTable)<-c("Pop Secret","Fastco")
PredTable



# =======================================================
# FLOWCHART EXAMPLE
# =======================================================


# Import Popcorn Data
Popcorn <- read.delim(file.choose())
View(Popcorn)

# NOTE: Initial popcorn data may include some columns and rows incorrectly
# interpreted by R as NA's. So we remove these with the following command:
# This is mainly a technical step, not conceptually important.
PopDat<-Popcorn[1:32,1:5]
View(PopDat)

# Notice that the estimate from the linear model explained only by an intercept
# is the same as the mean of the response variable.
with(PopDat,mean(X.Unpopped))
with(PopDat,lm(X.Unpopped~1))
with(PopDat,lm(X.Unpopped~Brand))
with(PopDat,lm(X.Unpopped~Microwave))
with(PopDat,lm(X.Unpopped~Brand*Microwave))

# We can test the models against each other as follows:
anova(lm(X.Unpopped~1,data=PopDat),lm(X.Unpopped~Brand,data=PopDat))
anova(lm(X.Unpopped~1,data=PopDat),lm(X.Unpopped~Microwave,data=PopDat))

# sex is not signigicant
# trait is not really significant
# interaction effect or addition effects

# In neither case does the non-experimental factor we add seem to improve
# the model in a statistically significant way. Therefore if we follow the
# procedure on the flowchart, we would not include either factor, and
# run a one-way ANOVA including only our experimental factor:
anova(lm(X.Unpopped~Time,data=PopDat))

# Import body shame data.
# Notice that this data is in a .csv file, which is different than a .txt one.
# R has a separate built in command for this, called read.csv(). You can import
# the data by running this command, and then finding the file in your file manager.
ShameDat<-read.csv(file.choose())

# Try to do this based on the flowchart you came up with for the body shame data.

