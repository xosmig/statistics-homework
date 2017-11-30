N = 100
UNIF.MAX = 2
CONF = 0.95

print.result <- function(p.value, test.name) {
  status <- if (p.value <= 1 - CONF) "rejected" else "not rejected"
  print(sprintf("The hypothesis is %s by %s test with p.value=%f", status, test.name, p.value))
}

print.chisq.test <- function(sample) {
  NUM.OF.INTERVALS = 10
  
  rs <- seq(from=0, to=UNIF.MAX, length.out=NUM.OF.INTERVALS+1)
  cnts <- sapply(1:NUM.OF.INTERVALS, function(i) {
    length(which(sample >= rs[i] & sample < rs[i + 1]))
  })
  p.value <- chisq.test(cnts, p=rep(1 / NUM.OF.INTERVALS, NUM.OF.INTERVALS))$p.value
  print.result(p.value, "chi square")
}

print.ks.test <- function(sample) {
  p.value <- ks.test(sample, function(...) punif(..., max=UNIF.MAX))$p.value
  print.result(p.value, "Kolmogorov-Smirnov")
}

sample <- runif(N, max=UNIF.MAX)
print.chisq.test(sample)
print.ks.test(sample)

