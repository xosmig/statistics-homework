
EST.SAMPLE.SIZE = 1000
VARIANCE.SAMPLE.SIZE = 2000
THETA = 1 

est.unif <- function(k) {
  x <- runif(EST.SAMPLE.SIZE, max=THETA)
  est <- (mean(x^k * (k + 1)))^(1/k)
}

est.exp <- function(k) {
  x <- rexp(EST.SAMPLE.SIZE, rate=1/THETA)
  est <- (mean(x^k) / gamma(k + 1))^(1/k)
}

plot.variance <- function(est.function, ks, main.title="") {
  variance <- function(k) {
    ests <- sapply(rep(k, VARIANCE.SAMPLE.SIZE), est.function)
    variance <- mean((ests - THETA)^2)
  }
  vars <- sapply(ks, variance)
  plot(x=ks, y=sqrt(vars), type="l",
       main=main.title,
       xlab="Moment order",
       ylab="Standard deviation")
}

show.plots <- function() {
  par(mfrow=c(1, 2))
  plot.variance(est.unif, seq(from=0.1, to=10, length.out=15), "Uniform distribution")
  plot.variance(est.exp, seq(from=0.1, to=2.5, length.out=15), "Exponential distribution")
}

save.plots.jpeg <- function(filename) {
  jpeg(filename, width=960, height=480)
  show.plots()
  dev.off()
}

#show.plots()
save.plots.jpeg("01.jpeg")
