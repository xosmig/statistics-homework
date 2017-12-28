
gen <- function(n, rho) {
  x <- rnorm(n)
  z <- rnorm(n)
  y <- x * rho + sqrt(1 - rho^2) * z
  list(x=x, y=y)
}

N = 50
rhos = c(0.9, 0.5, 0, -0.5, -0.9)
cols <- c("green", "black", "blue", "purple", "yellow")

plot.first.type.error <- function() {
  plot(x=NULL, y=NULL,
       xlab="P-Value", xlim=c(0, 1),
       ylab="Distribution Function", ylim=c(0, 1))
  for (i in seq_along(rhos)) {
    rho <- rhos[i]
    col <- cols[i]
    pvalues <- sapply(1:1000, function(ignored) {
      sample <- gen(N, rho)
      t.test(sample$x, sample$y, var.equal=TRUE)$p.value
    })
    lines(ecdf(pvalues), col=col)
  }
  legend("topleft", legend=rhos, col=cols, lty=1, title="rho")
}

plot.statistic.density <- function() {
  plot(x=NULL, y=NULL,
       xlab="t.test statistic", xlim=c(-4, 4),
       ylab="Density", ylim=c(0, 1.3))
  for (i in seq_along(rhos)) {
    rho <- rhos[i]
    col <- cols[i]
    stats <- sapply(1:1000, function(ignored) {
      sample <- gen(N, rho)
      t.test(sample$x, sample$y, var.equal=TRUE)$statistic
    })
    lines(density(stats), col=col)
  }
  legend("topleft", legend=rhos, col=cols, lty=1, title="rho")
}

show.plots <- function() {
  par(mfrow=c(1, 2))
  
  plot.first.type.error()
  plot.statistic.density()
}

save.plots.jpeg <- function(filename) {
  jpeg(filename, width=960, height=480)
  show.plots()
  dev.off()
}

save.plots.jpeg("07/plot.jpeg")

