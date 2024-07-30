pdf("graph.pdf")

problems.1 <- read.csv("problems-1.csv", header = TRUE)
problems.2 <- read.csv("problems-2.csv", header = TRUE)

names = problems.1[[1]]

sums.1 <- c()
sums.2 <- c()
for (i in 1:length(names)) {
    sum <- 0.0
    for (j in 2:length(problems.1)) {
        sum <- sum + problems.1[[j]][i]
    }
    sums.1 <- append(sums.1, sum)
}

# plot(sums.1, xaxt = "n")
# axis(1, at = 1:length(names), labels = 1:length(names))

plot(1:length(names) ~ sums.1, yaxt = "n", xaxt = "n", xlab = "# of problems solved", ylab = "students")

axis(2, at = 1:length(names), labels = 1:length(names))
axis(1, at = 0:12, labels = paste(0:12))
