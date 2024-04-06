
# Import the data
G17.pilot.data <- read.csv("G17.pilot.data.csv")

# Calculate SD
sd(G17.pilot.data$tot.vase.days)

# Use power.t.test function, as we have big sample size, so normal distribution can be assumed
power.t.test(n = , delta = 1, sig.level = 0.05, sd = 4, power = 0.8, type = "two.sample",
alternative = "one.sided", strict = TRUE) # 199

power.t.test(n = , delta = 1, sig.level = 0.05, sd = 4, power = 0.9, type = "two.sample",
             alternative = "one.sided", strict = TRUE) # 275

# The total sample size is the product of 1 compound with 15
sample_size <- 199 * 15

