muX = 39217; muY = 43121
sX = 12210; sY = 17020
n = 100
alpha = 0.01

df <- (n - 1) * (sX^2 + sY^2)^2 / (sX^4 + sY^4)
print(sprintf("df = %f", df))

sXY <- sqrt(sX^2 / nX + sY^2 / nY)
T <- (muX - muY) / sXY
print(sprintf("t = %f", T))

print(sprintf("conf.int = (%f; %f)", qt(alpha / 2, df=df), qt(1 - alpha / 2, df=df)))

sampleX <- rnorm(nX, mean=muX, sd=sX)
sampleY <- rnorm(nY, mean=muY, sd=sY)

print(t.test(sampleX, sampleY, conf.level=0.99))
