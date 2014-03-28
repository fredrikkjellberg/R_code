library(mosaic)

# First we can import all the data sets we need.
# Note that the first line of the data in these text files is
# a header; thus when importing the data we want to click
# "Yes" in the header check box.

# ==================================================
# MANAGING / REMOVING NA'S
# ==================================================

# NA is a special object in R that represents the lack of knowledge
# about an observation. Any operation applied to NA should return NA:
NA + 1
NA - 57
NA/2
NA*2342
mean(c(NA,1:5))
NA > 5

# Note that we cannot use the == comparison operator with NA's.
# Just like with the > operator, the result will always be NA!
2 == NA
NA == NA

# So how to we check whether things are NA or not?
# R has a few useful built in functions for NA's.
is.na(NA)
is.na(1)
is.na(c(NA,1,2,NA,4))

# You may recall from earlier in the semester that ! is the "not"
# command. It reverses something from TRUE to FALSE. You can check
# whether something is NOT an NA by using this in conjunction with
# is.na
!is.na(NA)
!is.na(1)
!is.na(c(NA,1,2,NA,4))

# This can be useful for eliminating all the elements that are NA
# from a data vector:
q1a<-with(rusagefull,q1a)
q1a
!is.na(q1a)
q1a[!is.na(q1a)]

# Another way to do this:
na.omit(q1a)

# ==================================================
# CONVERTING LIKERT SCALES TO NUMERIC RESPONSES
# ==================================================

# Keeping the data in its original form can be
# problematic with some operations that want
# numeric input.
q2<-with(rusagefull,q2)
q2
mean(q2)
with(rusagefull,boxplot(q2~Lab))

# Potential solution: We can use the command
# 'as.numeric' to convert a, b, c, d, e
# to 1, 2, 3, 4, 5 automatically.
q2.num<-with(rusagefull,as.numeric(q2))
q2.num
mean(q2.num)
with(rusagefull,boxplot(q2.num~Lab))


# ==================================================
# POOLING / MERGING CATEGORIES ON LIKERT SCALES
# ==================================================

q3<-with(rusagefull,q3)
q3
summary(q3)

# In a Likert scale we have data about whether respondants
# SA / A / N / D / SD with a proposition, or something similar. 
# But maybe we want to analyze along the linse of
# agree / neutral / disagree only.

# We can transform the data set in a way that makes that easier.

# STEP 1: Create a vector as long as q3, containing only "abcde".
# This string is arbitrary, and will eventually be replaced.
q3_collapsed<-rep("abcde",length(q3))

# STEP 2: Update the vector so that for every entry where q3 is
# either d or e, "abcde" is replaced by "Disagree".
q3_collapsed[q3 == 'd' | q3 == 'e']<-"Disagree"
q3_collapsed

# STEP 3: Update the vector so that for every entry where q3 is
# either a or b, "abcde" is replaced by "Agree"
q3_collapsed[q3 == 'a' | q3 == 'b']<-"Agree"
q3_collapsed

# STEP 4: Update the vector so that for every entry where q3 is
# c, "abcde" is replaced by "Neutral"
q3_collapsed[q3 == 'c']<-"Neutral"
q3_collapsed

# Since there is no missing data in this question, this should produce
# a vector collapsing categories "a" and "b" into "Agree",
# "c" into "Neutral", and "d" and "e" into "Disagree".

# Some methods in R will want the output to be a "factor" rather than
# a vector to use it. To convert it to a factor, we can use the as.factor function:
fq3_collapsed<-as.factor(q3_collapsed)
fq3_collapsed

# This kind of logic can be extended to pool data however you wish.