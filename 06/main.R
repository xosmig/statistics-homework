N = 100
UNIF.MAX = 2
ALT.UNIF.MAX = 2.2

print.test.result <- function(p.value, test.name, signif=0.95) {
  status <- if (p.value <= signif) "rejected" else "not rejected"
  res <- sprintf("The hypothesis is %s by %s test with p.value=%f",
                 status, test.name, p.value)
  print(res, quote=FALSE)
}

unif.ks.test <- function(sample) {
  p.value <- ks.test(sample, function(...) punif(..., max=UNIF.MAX))$p.value
}

plot.task.a <- function() {
  signifs <- seq(from=0, to=1, length.out=100)
  samples <- lapply(1:1000, function(ignored) runif(N, max=ALT.UNIF.MAX))
  p.values <- sapply(samples, function(sample) unif.ks.test(sample))
  accepted <- sapply(signifs, function(signif) length(which(p.values > signif)))
  plot(x=signifs, y=accepted / length(samples),
       xlab="Significance Level",
       ylab="Second Type Error Frequency",
       type="l")
}

plot.task.b <- function() {
  samples <- lapply(1:1000, function(ignored) rnorm(N))
  p.values <- sapply(samples, function(sample) {
    ks.test(sample, function(...) pnorm(..., mean=mean(sample), sd=sd(sample)))$p.value
  })
  plot(ecdf(p.values), main="ECDF For P Value In KS-Test", ylab="F(x)", xlab="P-Value")
}

show.plots <- function() {
  par(mfrow=c(1, 2))
  plot.task.a()
  plot.task.b()
}

save.plots.jpeg <- function(filename) {
  jpeg(filename, width=960, height=480)
  show.plots()
  dev.off()
}

save.plots.jpeg("06/plot.jpg")
