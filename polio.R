# The data from Table 1 from the reading.
# For simplicity, lets ignore the non-consenting group.
n <- c(200000, 200000)
rates <- c(28,71)/100000

d <- data.frame("treatment" = rep(c("treatment", "control"), n),
                "polio_status" = rep(0, sum(n)))

# This generates the "correct" incidence rates in each group
d$polio_status[1                 : round(rates[1] * n[1]) ] <- 1
d$polio_status[(n[1] + 1)        : (n[1] + round(rates[2] * n[2])) ] <- 1

with(d, table(treatment, polio_status))

library(mosaic)

# Let's use treatment - control as the test statistic
do(1)*with(d, mean(polio_status ~ treatment))
do(1)*with(d, diff(mean(polio_status ~ treatment)))
# Here's one permutation
do(1)*with(d, diff(mean(polio_status ~ shuffle(treatment))))
# Much closer to zero!

results <- do(100)*with(d, diff(mean(polio_status ~ shuffle(treatment))))

with(results, table(treatment < -0.00043 | treatment > 0.00043))
with(results, histogram(~treatment, groups=treatment < -0.00043 | treatment > 0.00043))
