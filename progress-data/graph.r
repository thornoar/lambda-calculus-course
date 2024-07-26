pdf("graph.pdf")

problems.1 <- read.csv("problems-1.csv", header = TRUE)

names = problems.1[[1]]

sums <- c()
for (i in 1:length(names)) {
    sum <- 0
    for (j in 2:length(problems.1)) {
        sum <- sum + problems.1[[j]][i]
    }
    sums <- append(sums, sum)
}

# names(sums) = names

plot(sums, xaxt = "n")
# axis(1, at = 1:length(names), labels = names)
