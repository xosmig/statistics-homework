
n.width.plot <- function(sd, col) {
  ns <- seq(from=100, to=1500, length.out=100)
  widths <- sapply(ns, function(n) {
    int <- t.test(rnorm(n, sd=sd))$conf.int
    width <- int[2] - int[1]
  })
  lines(ns, widths, col=col)
}

task.a <- function() {
  sds <- c(2, 1, 0.5, 0.3)
  cols <- c("green", "black", "blue", "purple")
  
  plot(x=NULL, y=NULL,
       xlab="Sample Size", ylab="Confidence Interval Width",
       type="l",
       xlim=c(100, 1500),
       ylim=c(0, 0.8))
  for (i in seq_along(sds)) {
    n.width.plot(sds[i], cols[i])
  }
  legend("topright", legend=sds, col=cols, lty=1, title="Standard Deviation")
}

sd.width.plot <- function(n, col) {
  sds <- seq(from=3, to=0.1, length.out=100)
  widths <- sapply(sds, function(sd) {
    int <- t.test(rnorm(n, sd=sd))$conf.int
    width <- int[2] - int[1]
  })
  lines(sds, widths, col=col)
}

task.b <- function() {
  ns <- c(300, 500, 1000, 2000)
  cols <- c("green", "black", "blue", "purple")
  
  plot(x=NULL, y=NULL,
       xlab="Standard Deviation", ylab="Confidence Interval Width",
       type="l",
       xlim=c(0, 3),
       ylim=c(0, 0.8))
  for (i in seq_along(ns)) {
    sd.width.plot(ns[i], cols[i])
  }
  legend("topleft", legend=ns, col=cols, lty=1, title="Sample Size")
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
