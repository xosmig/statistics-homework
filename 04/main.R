
task.a <- function() {
  ns <- seq(from=100, by=10, length.out=100)
  widths <- sapply(ns, function(n) {
    int <- t.test(rnorm(n))$conf.int
    width <- int[2] - int[1]
  })
  plot(x=ns, y=widths,
       main="Normal Distribution, Variance = 1",
       xlab="Sample Size", ylab="Confidence Interval Width")
}

task.b <- function() {
  n <- 1000
  sds <- seq(from=3, to=0.1, length.out=100)
  widths <- sapply(sds, function(sd) {
    int <- t.test(rnorm(n, sd=sd))$conf.int
    width <- int[2] - int[1]
  })
  print(sds)
  print(widths)
  plot(x=sds, y=widths,
       main=sprintf("Normal Distribution, Sample Size = %d", n),
       xlab="Standard Deviation", ylab="Confidence Interval Width",
       xlim=rev(range(x)))
}

show.plots <- function() {
  par(mfrow=c(1, 2))
  task.a()
  task.b()
}

save.plots.jpeg <- function(filename) {
  jpeg(filename, width=960, height=480)
  show.plots()
  dev.off()
}

save.plots.jpeg("04/plot.jpeg")
