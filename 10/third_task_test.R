n = 100

ksi = matrix(rnorm(n))

`%==%` <- function(x, y) abs(x - y) < 1e-7

B = matrix(1 / n, n, n)
C = diag(n) - matrix(1 / n, n, n)

print(as.vector(t(ksi) %*% B %*% ksi) %==% (n * mean(ksi)^2))

n.times.var = sum((ksi - mean(ksi))^2)
print(as.vector(t(ksi) %*% C %*% ksi) %==% n.times.var)

print(all(B %*% C %==% 0))
