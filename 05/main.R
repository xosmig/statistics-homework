N = 100
UNIF.MAX = 2

print.test.result <- function(p.value, test.name, signif=0.05) {
  status <- if (p.value <= signif) "rejected" else "not rejected"
  res <- sprintf("The hypothesis is %s by %s test with p.value=%f",
                 status, test.name, p.value)
  print(res, quote=FALSE)
}

unif.chisq.test <- function(sample) {
  NUM.OF.INTERVALS = 10

  int.sep <- seq(from=0, to=UNIF.MAX, length.out=NUM.OF.INTERVALS + 1)
  cnt <- sapply(1:NUM.OF.INTERVALS, function(i) {
    length(which(sample >= int.sep[i] & sample < int.sep[i + 1]))
  })
  p.value <- chisq.test(cnt, p=rep(1 / NUM.OF.INTERVALS, NUM.OF.INTERVALS))$p.value
}

unif.ks.test <- function(sample) {
  p.value <- ks.test(sample, function(...) punif(..., max=UNIF.MAX))$p.value
}

task.a <- function() {
  sample <- runif(N, max=UNIF.MAX)
  print.test.result(unif.chisq.test(sample), "chi square")
  print.test.result(unif.ks.test(sample), "Kolmogorov-Smirnov")
}

plot.task.b <- function() {
  signifs <- seq(from=0, to=1, length.out=100)
  samples <- lapply(1:1000, function(ignored) runif(N, max=UNIF.MAX))
  p.values <- sapply(samples, function(sample) unif.ks.test(sample))
  rejected <- sapply(signifs, function(signif) length(which(p.values <= signif)))
  plot(x=signifs, y=rejected / length(samples),
       xlab="Significance Level",
       ylab="Rejected Samples Rate",
       type="l")
}

save.task.b.jpeg <- function(filename) {
  jpeg(filename)
  plot.task.b()
  dev.off()
}

task.a()
save.task.b.jpeg("05/task_b.jpg")
