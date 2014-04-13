# NOTE: I have recieved feedback that I went too quickly
# and was not clear enough in the lab where this was discussed. 
# Message heard, I will try to slow down in the future. But
# in the meantime I have added additional comments to this
# file in order to hopefully help compensate.
# Another place you can look for a description of this project
# is in the textbook starting on pg. 204, as described in the
# lecture slides. This will hopefully be helpful if my
# presentation of the material is still confusing to you.

# If you have any questions about this, or any other feedback
# about improving the quality of labs that I could consider,
# please don't hesitate to tell me or send me an email (hallac@umich.edu).
# - Adam


library(mosaic)

# =========================================================
# PREAMBLE. CODE IN THIS SECTION IS OPTIONAL TO UNDERSTAND.
# =========================================================
expect.count <- function(cont.table) {
  if (!is(cont.table, "table")) {
    stop("You must pass a contingency table.")
  }
  if (length(dim(cont.table)) != 2) {
    stop("The contingency table must be 2x2.")
  }
  outer(margin.table(cont.table, 1),
        margin.table(cont.table, 2))/margin.table(cont.table)
}


stackedbarchart <- function(cont.table,
                            legend.position="topleft") {
  if (!is(cont.table, "table")) {
    stop("You must pass a contingency table.")
  }
  if (length(dim(cont.table)) != 2) {
    stop("The contingency table must be 2x2.")
  }
  if (!(legend.position %in% c("topright", "topleft"))) {
    stop("legend.position must be either \"topright\" or \"topleft\"")
  }
  barplot(cont.table, legend.text=paste(names(dimnames(cont.table))[1],
                                        rownames(cont.table), sep=" "),
          names.arg=colnames(cont.table),
          xlab=names(dimnames(cont.table))[2],
          args.legend=list(x=legend.position))
}

baboon.fix.order <- function(bdata) {
  bdata$Mother.Rank <- factor(bdata$Mother.Rank,
                              levels=c("Hi", "Mid", "Low"))
  bdata$Handler.Rank <- factor(bdata$Handler.Rank,
                               levels=c("Hi", "Mid", "Low"))
  bdata
}

baboon.shuffle <- function(bdata) {
  handlers <- unique(bdata[,3:4])
  handlers$Handler.Rank <- shuffle(handlers$Handler.Rank)
  bdata$Handler.Rank <- handlers$Handler.Rank[bdata$Handler]
  
  mothers <- unique(bdata[,1:2])
  mothers$Mother.Rank <- shuffle(mothers$Mother.Rank)
  bdata$Mother.Rank <- mothers$Mother.Rank[bdata$Infant]
  bdata
}

# ====================================================================
# END OF PREAMBLE
# ====================================================================

# Import the data from the CSV file
baboon<-read.csv(file.choose())

# Run a "fixing" command on this data that's not important to understand
# for the purposes of this course.
baboon<-baboon.fix.order(baboon)
View(baboon)

# We can make a contingency table here similar to the one in the slides, 
# and see what the data looks like from that perspective again:
babtab<-table(baboon$Mother.Rank,baboon$Handler.Rank)
babtab

# The chi-squared test of independence implicitly compares this table to
# a table of expected counts, which we can compute using a function in the
# code at the top of this file:
expect.count(babtab)

# After running the code in the preamble to this file, we can also visualize
# the data using a stacked bar chart:
stackedbarchart(babtab,"topleft")

# We can use the "chisq.test" command here to do the test:
chisq.test(babtab)

# The p-value is small here, but the independence assumption isn't really
# met here. So this is maybe an unreliable test.

# Rather than doing this kind of test, we may choose to do a permutation
# test instead. However, owing to issues with our assumptions, we cannot
# do the shuffling in this test in exactly the same way we usually do.
# You can use the "baboon.shuffle" function from the preamble to handle this 
# shuffling for you. No need to worry about the precise details of how this
# works, but they are described in the textbook if you are curious.
shuffled.baboon<-baboon.shuffle(baboon)
View(shuffled.baboon)

# Using this function we can do a permutation test in the usual way, if we can
# find a test statistic that is appropriate for our hypothesis. What is a good
# statistic to use to test that females will handle infants whose parents ranked 
# lower than or equal to themselves?

# Look again at the table:
babtab

# Intuitively, the cells in the top corner cells represent situations in which
# mother's rank was higher than handler's rank. These cells would contradict our
# hypothesis if they were large enough. By contrast, the lower cells and the
# diagonal are in line with our hypothesis.

# We might want to take a weighted sum that assigns a negative value to observations
# that contradict our hypothesis, and a positive value to cells that match it.
# One way to do this is to do the following:

# Define a matrix of the weights.
LTE<- matrix(c( 1,-1,-1,
                1, 1,-1,
                1, 1, 1),nrow=3,byrow=TRUE)

# The point of this matrix is that there will be a "1" in all the cells where
# the research hypothesis is true, and a "-1" in all the cells where the research hypothesis
# is contradicted. So in the test we are doing here, the cells in the top right of the
# table correspond to situations in which the rank of the handler was greater than
# the rank of the infant, e.g. higher ranked infants being handled by lower ranked baboons.
# Hence the LTE matrix has -1 in the cells in the upper right corner, and 1 in the rest.


# The matrix command has the "nrow" argument, which tells it that the matrix should
# have that number of rows. "byrow" has to be set to be true, so that the matrix
# will fill in row first rather than column first.
# i.e. if we did not have the byrow=TRUE command, the matrix would look like this:
# 1  1  1
#-1  1  1
#-1 -1  1
# Which is not what we want.

# Take the dot product (NOT matrix product!) between this and the observed counts.
# This will just multiply cell (1,1) in the table by 1, cell (1,2) in the table by
# -1, etc. Each cell in the data table will be multiplied by the corresponding cell
# in the LTE table.
babtab*LTE

# And then take the sum. This should just add all the cells in the matrix together.
obsLTE<-sum(babtab*LTE)

# We can use this sum as our "test statistic". 
# It will be bigger if there are lots of cases where the research hypothesis looks true,
# and smaller if there are lots of cases where the research hypothesis looks false.

# Using our special shuffle function, we can then do a permutation test step as per normal:
shuffbab<-baboon.shuffle(baboon)
simtab<-table(shuffbab$Mother.Rank,shuffbab$Handler.Rank)
simLTE<-LTE*simtab

# Go ahead and try to run a simulation test on your own.
# HINT: It may be helpful to try to write a function that does the above three steps,
# so that you can use the do() command, similar to what we did before.

# MY SOLUTION:
babsim<-function(babdat,mask)
{
  # create a table of the shuffled data.
  # This step is similar to the table from before with the observed
  # data.
  sbtab<-with(baboon.shuffle(babdat),table(Mother.Rank,Handler.Rank))
  
  # multiply whatever was passed in as the "mask" argument
  # by the table of shuffled data. The idea is that "mask" is a 
  # dummy variable for the LTE matrix. Having the function set up
  # in this way means that we can easily change what matrix we pass
  # the function, and in this way change what the
  # hypothesis considered in the test statistic is.
  sum(mask*sbtab)
}

# In this function we omitted the "return" statement. If no return
# statement is explicitly stated, R will default to returning the
# last thing that it calculates in the function. So the function
# will return the sum(mask*sbtab) as desired.

simoutput<-do(1000)*babsim(baboon,LTE)

# We look at whether the test statistic output by the simulation is
# bigger than or equal to the observed test statistic, e.g. we count
# the number of times we see a statistic by chance in our shuffled data 
# that is as or more supportive of the hypothesis than what we observed.
# Then we divide this by the number of simulations we did, in order to
# get an estimate of our p-value.
sum(simoutput>=obsLTE)/1000

# Now try changing the hypothesis tested to being that females will handle
# infants ranked strictly lower than themselves. (The current code is set up to test
# whether rank is lower than OR EQUAL TO the handler rank). 
# How would you do this test?

# Solution: Code should be the same, just with different weight matrix.
LTE2<- matrix(c(-1,-1,-1,
                 1,-1,-1,
                 1, 1,-1),nrow=3,byrow=TRUE)

# In this matrix, the 1's in the diagonal are also negative.
# This reflects the fact that our hypothesis has changed from
# "rank less than or equal to" to just "rank less than".
# So we're now counting cells in which handler ranks were equal
# as contradicting our research hypothesis, instead of
# the way we did it before, where they were in line with it.

# The rest of the simulation is run
# as before:
simoutput<-do(1000)*babsim(baboon,LTE2)
sum(simoutput>=obsLTE)/1000