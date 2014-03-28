source("~/Downloads/usefulRfunctions.R")
source("~/Downloads/baboonfunctions.R")
library(mosaic)

baboon <- baboon.fix.order(baboon)

btab <- with(baboon, table(Mother.Rank, Handler.Rank))

stackedbarchart(btab, "topleft")

expect.count(btab)
btab

chisq.test(btab)



LTE <-matrix(c(1, -1, -1,
           1,  1, -1,
           1,  1,  1),
         nrow=3, ncol=3, byrow=TRUE)

obsLTE <- sum(LTE*btab)


shuffled.baboon <- baboon.shuffle(baboon)

sbtab <- with(shuffled.baboon, table(Mother.Rank, Handler.Rank))

sum(LTE*sbtab)

baboon.perm.test <- function(mask) {
  shuffled.baboon <- baboon.shuffle(baboon)  
  sbtab <- with(shuffled.baboon, table(Mother.Rank, Handler.Rank))
  sum(mask*sbtab)
}

LTEstats <- do(1000)*baboon.perm.test(LTE)
head(LTEstats)
with(LTEstats, table(result >= obsLTE))


LT <-matrix(c(-1, -1, -1,
               1, -1, -1,
               1,  1, -1),
             nrow=3, ncol=3, byrow=TRUE)
obsLT <- sum(LT*btab)

LTstats <- do(1000)*baboon.perm.test(LT)
head(LTstats)
with(LTstats, table(result >= obsLT))
