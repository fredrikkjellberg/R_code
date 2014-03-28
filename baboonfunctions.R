# Does some trivial code fixes that just ensures R looks at the levels
# of the variables (hi, mid, low) in the proper order.
baboon.fix.order <- function(bdata) {
  bdata$Mother.Rank <- factor(bdata$Mother.Rank,
                              levels=c("Hi", "Mid", "Low"))
  bdata$Handler.Rank <- factor(bdata$Handler.Rank,
                               levels=c("Hi", "Mid", "Low"))
  bdata
}

# Since the data is 'clustered' by handler, we want shuffling to take
# place across cluster. E.g, when shuffling, a given handler gets new
# random rank, not each different interaction a handler had.
baboon.shuffle <- function(bdata) {
  handlers <- unique(bdata[,3:4])
  handlers$Handler.Rank <- shuffle(handlers$Handler.Rank)
  bdata$Handler.Rank <- handlers$Handler.Rank[bdata$Handler]

  mothers <- unique(bdata[,1:2])
  mothers$Mother.Rank <- shuffle(mothers$Mother.Rank)
  bdata$Mother.Rank <- mothers$Mother.Rank[bdata$Infant]
  bdata
}



babsim <- fction(babdad, mask){
  sbrb<-with(babdad)
}

# why do we have a matrix?



with baboob
