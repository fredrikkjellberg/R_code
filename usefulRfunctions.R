# These are functions that may be useful. If you are going to use
# them, you can either place them in your own file, or just run them
# once you open R, or use `source("path/to/usefulRfunctions.R").

# You do not need to include these functions when submitting code. You
# can simply call and use them as necessary.


# This function will plot a stacked bar chart of a given contingency
# table passed to it. A second argument, legend.position, can be used
# to define where the legend is. Possible values are "topright" or
# "topleft".
stackedbarchart <- function(cont.table,legend.position="topright") {
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

# For a given two-way contingency table, this will produce the
# expected counts for the table.
expect.count <- function(cont.table) {
  if (!is(cont.table, "table")) {
    stop("You must pass a contingency table.")
  }
  if (length(dim(cont.table)) != 2) {
    stop("The contingency table must be 2x2.")
  }
  outer(margin.table(cont.table, 1),margin.table(cont.table, 2))/margin.table(cont.table)
}

expect.count(donner)
