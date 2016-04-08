# Student Number: 14205449

# Question 2

# Clear variables and Set working directory
rm(list=ls())
setwd("Assignment02")

library(class)
library(klaR)

# (a)

# Load the voting data

load("2016_First6Votes_PresentAbsent.Rdata")
head(votes)

# votes.cat <- lapply(votes, factor)

cl <- kmodes(votes, k)

## and visualize with some jitter:
plot(jitter(votes.cat), col = cl$cluster)
points(cl$modes, col = 1:5, pch = 8)

plot(votes, col = cl$cluster)
load("PartyMembership.Rdata")

par(mfrow=c(1, 1))
barplot(votes.tab.ED1, ylim = c(0,130))
barplot(votes.tab.ED2, ylim = c(0,130))
barplot(votes.tab.Credit, ylim = c(0,130))
barplot(votes.tab.Confidence1, ylim = c(0,130))
barplot(votes.tab.Confidence2, ylim = c(0,130))
barplot(votes.tab.Trade)

# votes.cat <- lapply(votes, factor)
set.seed(123)
K <- 10

withindiffs <- rep(0,K)

for (k in 1:K) {
  
  withindiffs[k] <- sum(kmodes(votes, k)$withindiff)
}

K <- 10
silwidth <- rep(0, K)
silwidth[1] <- 0
for (k in 2:K) {
  silwidth[k] <- pam(d,k)$silinfo$avg.width
}

print(cbind(1:K, silwidth))


set.seed(1)
x <- rbind(matrix(rbinom(250, 2, 0.25), ncol = 5),
           matrix(rbinom(250, 2, 0.75), ncol = 5))
colnames(x) <- c("a", "b", "c", "d", "e")

## run algorithm on x:
(cl <- kmodes(x, 3))

cl## and visualize with some jitter:
plot(jitter(x), col = cl$cluster)
points(cl$modes, col = 1:5, pch = 8)

table(votes)

plot(DS0012$BMI.category, DS0012$age.category, xlab = "BMI", ylab = "age")

plot(votes.cat$ED1, votes.cat$ED2, ylab = "ED2", xlab = "ED1")
plot(votes.cat)

counts <- table(votes)
plot(counts)

adjRand <- adjustedRandIndex(votes.hclust.average.hcl, votes.lc$predclass)
adjRand
table(votes.hclust.average.hcl, votes.lc$predclass)
