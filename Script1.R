setwd("C:/Users/dmillan/Source/Repos/ts_dist")

library(data.table)
library(ggplot2)
library(viridis)
library(fields)

# Load test data
sample <- fread(
  file = "AReM/cycling/dataset3.csv",
  col.names = c(
    "time",
    "avg_rss12", "var_rss12",
    "avg_rss13", "var_rss13",
    "avg_rss23", "var_rss23"
    ),
  skip = 5
  )

ggplot(sample) +
  geom_line(aes(x = time, y = avg_rss13))

mirror <- fread(
  file = "AReM/sitting/dataset4.csv",
  col.names = c(
    "time",
    "avg_rss12", "var_rss12",
    "avg_rss13", "var_rss13",
    "avg_rss23", "var_rss23"
    ),
  skip = 5
  )

ggplot(mirror) +
  geom_line(aes(x = time, y = avg_rss13), color = "blue")

window_size <- 30

ts <- sample[, .(avg_rss13)]
tm <- mirror[, .(avg_rss13)]

set_index <- function(dt) {  
  dt[, index := .I]
  dt[, w_index := index + window_size]
  dt[w_index > nrow(ts) + 1, w_index := NA]
  dt[w_index <= nrow(ts) + 1, w_index := 1]
}

set_index(ts)
set_index(tm)

ts_m <- t(
          sapply(
            1:(nrow(ts) - window_size + 1),
            function(i) t(ts[i:(i + window_size - 1), avg_rss13])
            )
            )

#tm <- data.table(avg_rss13 = runif(1e6))
#set_index(tm)
tm_m <- t(
          sapply(
          1:(nrow(tm) - window_size + 1),
          function(i) t(tm[i:(i + window_size - 1), avg_rss13])
          )
          )

m <- rbind(ts_m, tm_m)
m <- as.matrix(scale(m))

d <- as.matrix(dist(m))
diag(d) <- NA
d[lower.tri(d)] <- NA
d[1:10, 1:10]

x <- 1:dim(d)[1]
y <- 1:dim(d)[2]
image.plot(
      x, y, d,
      xlab = "From", ylab = "To",
      col = viridis(100), useRaster = TRUE, axes = FALSE
      )
axis(1, at = seq(1, dim(d)[1], by = 50), tick = TRUE)
axis(2, at = seq(1, dim(d)[2], by = 50), tick = TRUE)
box()
title(main = "Distance matrix", font.main = 4)

di <- as.data.table(which(d < 4, arr.ind = TRUE))
di[col > row + nrow(tm) - window_size + 1]

summary(as.vector(d))

plot(m[222, ], type = "l")
lines(m[686, ], col = "red")
