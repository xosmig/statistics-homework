
cnt <- c(60, 140, 125, 155)
n <- sum(cnt)
k <- length(cnt)

mean <- 960 / n
p <- c(sapply(0:2, function(x) dpois(x, lambda=mean)), 1 - ppois(2, lambda=2))
p <- round(p, 3)

T <- n * sum((cnt / n - p)^2 / p)
print(sprintf("chi.sq = %f", T, 3))

pvalue <- 1 - pchisq(T, df=k - 2)
print(sprintf("pvalue = %f", pvalue))

print(chisq.test(cnt, p=p)$statistic)
