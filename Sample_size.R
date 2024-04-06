
G17.pilot.data <- read.csv("G17.pilot.data.csv")

sd(G17.pilot.data$tot.vase.days)

power.t.test(n = , delta = 1, sig.level = 0.05, sd = 4, power = 0.8, type = "two.sample",
alternative = "one.sided", strict = TRUE)

power.t.test(n = , delta = 1, sig.level = 0.05, sd = 4, power = 0.9, type = "two.sample",
             alternative = "one.sided", strict = TRUE)

sample_size <- 199 * 15

hist(G17.pilot.data$tot.vase.days)


