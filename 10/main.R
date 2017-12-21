
f.test <- function(table) {
  alpha = .05
  
  all <- unlist(table)
  means <- sapply(table, function(x) mean(x))
  print(sprintf("means = %s", paste(means, collapse=" ")))
  g.mean <- mean(all)
  
  df.bg <- length(table) - 1
  ss.bg <- sum(sapply(table, function(x) { length(x) * (mean(x) - g.mean)^2 }))
  print(sprintf("df.bg = %d", df.bg))
  print(sprintf("ss.bg = %f", ss.bg))
  
  ss.wg <- sum(sapply(table, function(x) { sum((x - mean(x))^2) }))
  df.wg <- length(all) - length(table)
  print(sprintf("df.wg = %d", df.wg))
  print(sprintf("ss.wg = %f", ss.wg))
  
  f.value <- (ss.bg / df.bg) / (ss.wg / df.wg)
  print(sprintf("f.value = %f", f.value))

  crit.value <- qf(1 - alpha, df1=df.bg, df2=df.wg)
  print(sprintf("crit.value = %f", crit.value))
  
  print(f.value < crit.value)
}


first.task <- function() {
  print("FIRST TASK")
  A <- c(78, 74, 85, 95, 93)
  B <- c(67, 79, 85, 79, 86)
  C <- c(75, 95, 69, 60, 94)
  D <- c(89, 86, 87, 87, 73)

  table <- list(A, B, C, D)
  f.test(table)
}


second.task <- function() {
  print("SECOND TASK")
  A <- c(85, 57, 92, 83, 84, 83, 75, 73)
  B <- c(84, 95, 87, 73, 83, 73, 85, 72)
  C <- c(63, 73, 83, 64, 79, 74, 65, 98)
  
  table <- list(A, B, C)
  f.test(table)
}

first.task()
print("========")
second.task()

