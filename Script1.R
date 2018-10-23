setwd("C:/Users/dmillan/Source/Repos/ts_dist")

library(data.table)
library(ggplot2)
library(viridis)
library(fields)

# Load test data
sample <- fread(
  file = "AReM/cycling/dataset1.csv",
  col.names = c(
    "time", "avg_rss12", "var_rss12",
    "avg_rss13", "var_rss13", "avg_rss23", "var_rss23"
    ),
  skip = 5
  )

ggplot(sample) +
  geom_line(aes(x = time, y = avg_rss13))

mirror <- fread(
  file = "AReM/cycling/dataset2.csv",
  col.names = c(
    "time", "avg_rss12", "var_rss12",
    "avg_rss13", "var_rss13", "avg_rss23", "var_rss23"
    ),
  skip = 5
  )

ggplot(mirror) +
  geom_line(aes(x = time, y = avg_rss13), color = "blue")

window_size <- 10

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

ts_m <- NULL
for (i in 1:(nrow(ts) - window_size + 1)) {
  ts_m <- rbind(ts_m, t(ts[i:(i + window_size - 1), avg_rss13]))
}

tm_m <- NULL
for (i in 1:(nrow(tm) - window_size + 1)) {
  tm_m <- rbind(tm_m, t(tm[i:(i + window_size - 1), avg_rss13]))
}

m <- rbind(ts_m, tm_m)
m <- as.matrix(scale(m))

d <- as.matrix(dist(m))
diag(d) <- NA
d[lower.tri(d)] <- NA
d[1:10, 1:10]

heatmap(
  d,
  col = viridis(10),
  Rowv = NA, Colv = NA, revC = TRUE
  )
image(d, col = viridis(10))
title(main = "Distance matrix", font.main = 4)

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

which(d < 1, arr.ind = TRUE)

plot(m[210,], type = "l")
lines(m[784, ], col = "red")