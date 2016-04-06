setwd("C:/Users/milosma/OneDrive/Documents/MSc in Data Analytics/Year 2 - Semester 2/STAT40740 - Multivariate Analysis/Lab 04")
rm(list=ls())

# 1 Linear discriminant analysis (LDA)
library(MASS)
# 1.1 Using linear discriminant analysis

diabetes <- read.csv("diabetes.csv")

lda.res <-lda(diabetes$class~diabetes$glucose+diabetes$insulin+diabetes$sspg)
lda.res <-lda(class~glucose+insulin+sspg, data=diabetes)

attributes(lda.res)
lda.res$prior
lda.res$means

N <- nrow(diabetes)
G <- length(levels(diabetes$class))

diabetes.norm <- subset(diabetes, class == "normal")
diabetes.chem <- subset(diabetes, class == "chemical")
diabetes.over <- subset(diabetes, class == "overt")
cov_norm <- cov(diabetes.norm[2:4])
cov_chem <- cov(diabetes.chem[2:4])
cov_over <- cov(diabetes.over[2:4])

cov_all <- ((cov_norm * (nrow(diabetes.norm)-1))
           +(cov_chem * (nrow(diabetes.chem)-1))
           +(cov_over * (nrow(diabetes.over)-1))) / (N-G)

ldf <- function(x, prior, mu, covar) {
  x <- matrix(as.numeric(x), ncol = 1)
  log(prior) - (0.5*t(mu)%*%solve(covar)%*%mu) + (t(x)%*%solve(covar)%*%mu)
}

id <- 1
dfs <- rep(0, G)
for (g in 1:G) {
  dfs[g] <- ldf(diabetes[id,2:4], lda.res$prior[g], lda.res$means[g,], cov_all)
}
dfs
levels(diabetes$class)[dfs == max(dfs)]

# 1.3 Cross validation
lda.res.cv <- lda(class ~ glucose + insulin + sspg, CV=TRUE, data=diabetes)
table(lda.res.cv$class, diabetes$class)

round(exp(dfs)/sum(exp(dfs)), 4)
round(lda.res.cv$posterior[id,],4)

qda.res.cv <- qda(class ~ glucose + insulin + sspg, CV = TRUE, data = diabetes)
table(qda.res.cv$class, diabetes$class)
