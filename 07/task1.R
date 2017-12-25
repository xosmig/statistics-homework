
xs <- c(1, 4, 3, 6, 5, 0, 7, 2, 1, 4, 3, 6, 5, 0, 7, 2, 1, 4, 3, 6, 5, 0, 7, 2, 1, 4, 3, 6, 5, 0, 7, 2, 1, 4, 3, 6, 5, 0, 7, 2, 1, 4, 3, 6, 5, 0, 7, 2, 1, 4)
n <- length(xs)
cnt <- aggregate(xs, list(value=xs), length)$x
print(cnt)
k <- length(cnt)
p = 1 / k

T <- n * sum((cnt / n - p)^2 / p)

print(sprintf("chi.sq = %f", T))
print(sprintf("pvalue = %f", 1 - pchisq(T, df=k-1)))


print(chisq.test(cnt, p=rep(p, k)))
