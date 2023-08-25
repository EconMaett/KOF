# load libraries ######

library(xts)
library(leaps)
library(midasr)
library(forecast)

# Benchmark calculation ######
# Load data
load("R_input_data.RData")

y <- imf.gdp.ag
y <- window(y, start = c(1992, 1))
y0 <- y[-c(1, 2, 3, 4)]
y1 <- y[-c(1, 2, 3, (length(y)))]
y2 <- y[-c(1, 2, (length(y) - 1), (length(y)))]
y3 <- y[-c(1, (length(y) - 2), (length(y) - 1), (length(y)))]
y4 <- y[-c((length(y) - 3), (length(y) - 2), (length(y) - 1), (length(y)))]

# forceast with rolling window
RMSE <- list()
MAE <- list()
RES <- list()
YHT <- list()
ny <- length(y)
psta <- c(2002, 3) # Start der Prognosephase
ytp <- window(imf.gdp.ag, start = psta)
np <- length(ytp)
##############################

# Forecast without AR(1)

df3 <- data.frame(y0 = y0, y2 = y2, y3 = y3)
md.yrec <- rep(0, np)
zzm <- rep(0, np)
for (i in 1:np) {
  mdc <- regsubsets(y0 ~ ., df3[i:(37 + i), ], nbest = 1, nvmax = 12, force.in = NULL, method = "exhaustive")
  z <- which.min(summary(mdc)$bic)
  zz <- coef(mdc, z)
  zzz <- paste(names(zz)[-1], collapse = "+")
  zm <- paste("y0", zzz, sep = "~")
  zzm[i] <- zm
  md.rec <- lm(as.formula(zm), data = df3[i:(37 + i), ])
  md.yrec[i] <- predict(md.rec, newdata = df3[(38 + i), ])
}
yhtp <- ts(md.yrec, start = c(2002, 3), freq = 4)
YHT$aro1 <- md.yrec
RES$aro1 <- yhtp - ytp
RMSE$aro1 <- sqrt(sum((yhtp - ytp)**2) / length(ytp))
MAE$aro1 <- sum(abs(yhtp - ytp)) / length(ytp)

# Forecast without AR(1) and AR(2)

df4 <- data.frame(y0 = y0, y3 = y3)
md.yrec <- rep(0, np)
zzm <- rep(0, np)
for (i in 1:np) {
  zm <- "y0~y3"
  zzm[i] <- zm
  md.rec <- lm(as.formula(zm), data = df4[i:(37 + i), ])
  # acf(md.rec$residuals)
  md.yrec[i] <- predict(md.rec, newdata = df4[(38 + i), ])
}
yhtp <- ts(md.yrec, start = c(2002, 3), freq = 4)
YHT$aro2 <- md.yrec
RES$aro2 <- yhtp - ytp
RMSE$aro2 <- sqrt(sum((yhtp - ytp)**2) / length(ytp))
MAE$aro2 <- sum(abs(yhtp - ytp)) / length(ytp)

# OECD #################
# load data

# Relatime forcasts, data situation last month of quartal

md.yrec <- rep(0, np)
zzm <- rep(NA, np)

for (i in 1:(np)) {
  xt <- ts(oecd.dat[, (16 + (3 * i))], start = c(1984, 1), freq = 12)
  x <- window(xt, start = c(1992, 1), end = c(2018, 6), extend = T)
  xmm <- mls(x, 0:9, 3)
  xmm <- xmm[-c(1:4), ]
  df.o1 <- data.frame(
    y0 = y0[i:(37 + i)], # y1=y1,
    y2 = y2[i:(37 + i)], y3 = y3[i:(37 + i)], # y4=y4[i:(36+i)],
    # x0=xmm[i:(36+i),c(1)],x1=xmm[,c(2)],
    x2 = xmm[i:(37 + i), c(3)], x3 = xmm[i:(37 + i), c(4)],
    x4 = xmm[i:(37 + i), c(5)], x5 = xmm[i:(37 + i), c(6)],
    x6 = xmm[i:(37 + i), c(7)], x7 = xmm[i:(37 + i), c(8)],
    x8 = xmm[i:(37 + i), c(9)], x9 = xmm[i:(37 + i), c(10)]
  )
  # x10=xmm[i:(36+i),c(11)],x11=xmm[i:(36+i),c(12)],
  # x12=xmm[i:(36+i),c(13)],x13=xmm[i:(36+i),c(14)],
  # x14=xmm[i:(36+i),c(15)])
  mdc <- regsubsets(y0 ~ ., df.o1, nbest = 1, nvmax = 19, force.in = NULL, method = "exhaustive")
  z <- which.min(summary(mdc)$bic)
  zz <- coef(mdc, z)
  zzz <- paste(names(zz)[-1], collapse = "+")
  zm <- paste("y0", zzz, sep = "~")
  zzm[i + 1] <- zm
  rec.o <- lm(as.formula(zm), df.o1)

  xt <- ts(oecd.dat[, (16 + (3 * (i + 1)))], start = c(1984, 1), freq = 12)
  x <- window(xt, start = c(1992, 1), end = c(2018, 6), extend = T)
  xmm <- mls(x, 0:9, 3)
  xmm <- xmm[-c(1:4), ]
  df.p1 <- data.frame(
    y0 = y0[(38 + i)], # y1=y1,
    y2 = y2[(38 + i)], y3 = y3[(38 + i)], # y4=y4[(37+i)],
    # x0=xmm[,c(1)],x1=xmm[,c(2)],
    x2 = xmm[(38 + i), c(3)], x3 = xmm[(38 + i), c(4)],
    x4 = xmm[(38 + i), c(5)], x5 = xmm[(38 + i), c(6)],
    x6 = xmm[(38 + i), c(7)], x7 = xmm[(38 + i), c(8)],
    x8 = xmm[(38 + i), c(9)], x9 = xmm[(38 + i), c(10)]
  )
  # x10=xmm[(37+i),c(11)],x11=xmm[(37+i),c(12)],
  # x12=xmm[(37+i),c(13)],x13=xmm[(37+i),c(14)],
  # x14=xmm[(37+i),c(15)])
  # acf(md.rec$residuals)
  md.yrec[i] <- predict(rec.o, newdata = df.p1)
}
yhtp <- ts(md.yrec, start = c(2002, 3), freq = 4)
YHT$oecd3 <- md.yrec
RES$oecd3 <- yhtp - ytp
RMSE$oecd3 <- sqrt(sum((yhtp - ytp)**2) / (length(ytp)))
MAE$oecd3 <- sum(abs(yhtp - ytp)) / (length(ytp))

# Relatime forcasts, data situation second last month of quartal
# np kommt aus dem benchmark
md.yrec <- rep(0, np)
zzm <- rep(NA, np)

for (i in 1:(np)) {
  xt <- ts(oecd.dat[, (15 + (3 * i))], start = c(1984, 1), freq = 12)
  x <- window(xt, start = c(1992, 1), end = c(2018, 6), extend = T)
  xmm <- mls(x, 0:9, 3)
  xmm <- xmm[-c(1:4), ]
  df.o1 <- data.frame(
    y0 = y0[i:(37 + i)], # y1=y1,
    y2 = y2[i:(37 + i)], y3 = y3[i:(37 + i)], # y4=y4[i:(36+i)],
    # x0=xmm[,c(1)],x1=xmm[,c(2)],
    # x2=xmm[i:(36+i),c(3)],
    x3 = xmm[i:(37 + i), c(4)],
    x4 = xmm[i:(37 + i), c(5)], x5 = xmm[i:(37 + i), c(6)],
    x6 = xmm[i:(37 + i), c(7)], x7 = xmm[i:(37 + i), c(8)],
    x8 = xmm[i:(37 + i), c(9)], x9 = xmm[i:(37 + i), c(10)]
  )
  # x10=xmm[i:(36+i),c(11)],x11=xmm[i:(36+i),c(12)],
  # x12=xmm[i:(36+i),c(13)],x13=xmm[i:(36+i),c(14)],
  # x14=xmm[i:(36+i),c(15)])
  mdc <- regsubsets(y0 ~ ., df.o1, nbest = 1, nvmax = 19, force.in = NULL, method = "exhaustive")
  z <- which.min(summary(mdc)$bic)
  zz <- coef(mdc, z)
  zzz <- paste(names(zz)[-1], collapse = "+")
  zm <- paste("y0", zzz, sep = "~")
  zzm[i + 1] <- zm
  rec.o <- lm(as.formula(zm), df.o1)

  xt <- ts(oecd.dat[, (15 + (3 * (i + 1)))], start = c(1984, 1), freq = 12)
  x <- window(xt, start = c(1992, 1), end = c(2018, 6), extend = T)
  xmm <- mls(x, 0:9, 3)
  xmm <- xmm[-c(1:4), ]
  df.p1 <- data.frame(
    y0 = y0[(38 + i)], # y1=y1,
    y2 = y2[(38 + i)], y3 = y3[(38 + i)], # y4=y4[(37+i)],
    # x0=xmm[,c(1)],x1=xmm[,c(2)],
    # x2=xmm[(37+i),c(3)],
    x3 = xmm[(38 + i), c(4)],
    x4 = xmm[(38 + i), c(5)], x5 = xmm[(38 + i), c(6)],
    x6 = xmm[(38 + i), c(7)], x7 = xmm[(38 + i), c(8)],
    x8 = xmm[(38 + i), c(9)], x9 = xmm[(38 + i), c(10)]
  )
  # x10=xmm[(37+i),c(11)],x11=xmm[(37+i),c(12)],
  # x12=xmm[(37+i),c(13)],x13=xmm[(37+i),c(14)],
  # x14=xmm[(37+i),c(15)])
  # acf(md.rec$residuals)
  md.yrec[i] <- predict(rec.o, newdata = df.p1)
}
yhtp <- ts(md.yrec, start = c(2002, 3), freq = 4)
YHT$oecd2 <- md.yrec
RES$oecd2 <- yhtp - ytp
RMSE$oecd2 <- sqrt(sum((yhtp - ytp)**2) / (length(ytp)))
MAE$oecd2 <- sum(abs(yhtp - ytp)) / (length(ytp))

# Relatime forcasts, data situation first month of quartal
# np kommt aus dem benchmark
md.yrec <- rep(0, np)
zzm <- rep(NA, np)

for (i in 1:(np)) {
  xt <- ts(oecd.dat[, (14 + (3 * i))], start = c(1984, 1), freq = 12)
  x <- window(xt, start = c(1992, 1), end = c(2018, 6), extend = T)
  xmm <- mls(x, 0:9, 3)
  xmm <- xmm[-c(1:4), ]
  df.o1 <- data.frame(
    y0 = y0[i:(37 + i)], # y1=y1,
    y2 = y2[i:(37 + i)], y3 = y3[i:(37 + i)], # y4=y4[i:(36+i)],
    # x0=xmm[,c(1)],x1=xmm[,c(2)],
    # x2=xmm[i:(36+i),c(3)],
    # x3=xmm[i:(36+i),c(4)],
    x4 = xmm[i:(37 + i), c(5)], x5 = xmm[i:(37 + i), c(6)],
    x6 = xmm[i:(37 + i), c(7)], x7 = xmm[i:(37 + i), c(8)],
    x8 = xmm[i:(37 + i), c(9)], x9 = xmm[i:(37 + i), c(10)]
  )
  # x10=xmm[i:(36+i),c(11)],x11=xmm[i:(36+i),c(12)],
  # x12=xmm[i:(36+i),c(13)],x13=xmm[i:(36+i),c(14)],
  # x14=xmm[i:(36+i),c(15)])
  mdc <- regsubsets(y0 ~ ., df.o1, nbest = 1, nvmax = 19, force.in = NULL, method = "exhaustive")
  z <- which.min(summary(mdc)$bic)
  zz <- coef(mdc, z)
  zzz <- paste(names(zz)[-1], collapse = "+")
  zm <- paste("y0", zzz, sep = "~")
  zzm[i + 1] <- zm
  rec.o <- lm(as.formula(zm), df.o1)

  xt <- ts(oecd.dat[, (14 + (3 * (i + 1)))], start = c(1984, 1), freq = 12)
  x <- window(xt, start = c(1992, 1), end = c(2018, 6), extend = T)
  xmm <- mls(x, 0:9, 3)
  xmm <- xmm[-c(1:4), ]
  df.p1 <- data.frame(
    y0 = y0[(38 + i)], # y1=y1,
    y2 = y2[(38 + i)], y3 = y3[(38 + i)], # y4=y4[(37+i)],
    # x0=xmm[,c(1)],x1=xmm[,c(2)],
    # x2=xmm[(37+i),c(3)],
    # x3=xmm[(37+i),c(4)],
    x4 = xmm[(38 + i), c(5)], x5 = xmm[(38 + i), c(6)],
    x6 = xmm[(38 + i), c(7)], x7 = xmm[(38 + i), c(8)],
    x8 = xmm[(38 + i), c(9)], x9 = xmm[(38 + i), c(10)]
  )
  # x10=xmm[(37+i),c(11)],x11=xmm[(37+i),c(12)],
  # x12=xmm[(37+i),c(13)],x13=xmm[(37+i),c(14)],
  # x14=xmm[(37+i),c(15)])
  # acf(md.rec$residuals)
  md.yrec[i] <- predict(rec.o, newdata = df.p1)
}
yhtp <- ts(md.yrec, start = c(2002, 3), freq = 4)
YHT$oecd1 <- md.yrec
RES$oecd1 <- yhtp - ytp
RMSE$oecd1 <- sqrt(sum((yhtp - ytp)**2) / (length(ytp)))
MAE$oecd1 <- sum(abs(yhtp - ytp)) / (length(ytp))

# Relatime forcasts, Datensitioantion data situation last month of previuos quartal

md.yrec <- rep(0, np)
zzm <- rep(NA, np)

for (i in 1:(np)) {
  xt <- ts(oecd.dat[, ((16 + (3 * i)) - 3)], start = c(1984, 1), freq = 12)
  x <- window(xt, start = c(1992, 1), end = c(2018, 6), extend = T)
  xmm <- mls(x, 0:9, 3)
  xmm <- xmm[-c(1:3), ]
  df.o1 <- data.frame(
    y0 = y0[i:(37 + i)], # y1=y1,
    # y2=y2[i:(36+i)],
    y3 = y3[i:(37 + i)], # y4=y4[i:(36+i)],
    # x0=xmm[,c(1)],x1=xmm[,c(2)],
    x2 = xmm[i:(37 + i), c(3)],
    x3 = xmm[i:(37 + i), c(4)],
    x4 = xmm[i:(37 + i), c(5)],
    x5 = xmm[i:(37 + i), c(6)],
    x6 = xmm[i:(37 + i), c(7)]
  ) # ,x7=xmm[i:(35+i),c(8)],
  # x8=xmm[i:(35+i),c(9)],x9=xmm[i:(35+i),c(10)])
  # x10=xmm[i:(36+i),c(11)],x11=xmm[i:(36+i),c(12)],
  # x12=xmm[i:(36+i),c(13)],x13=xmm[i:(36+i),c(14)],
  # x14=xmm[i:(36+i),c(15)])
  mdc <- regsubsets(y0 ~ ., df.o1, nbest = 1, nvmax = 19, force.in = NULL, method = "exhaustive")
  z <- which.min(summary(mdc)$bic)
  zz <- coef(mdc, z)
  zzz <- paste(names(zz)[-1], collapse = "+")
  zm <- paste("y0", zzz, sep = "~")
  zzm[i + 1] <- zm
  rec.o <- lm(as.formula(zm), df.o1)

  xt <- ts(oecd.dat[, ((16 + (3 * (i + 1))) - 3)], start = c(1984, 1), freq = 12)
  x <- window(xt, start = c(1992, 1), end = c(2018, 6), extend = T)
  xmm <- mls(x, 0:9, 3)
  xmm <- xmm[-c(1:3), ]
  df.p1 <- data.frame(
    y0 = y0[(38 + i)], # y1=y1,
    # y2=y2[(37+i)],
    y3 = y3[(38 + i)], # y4=y4[(37+i)],
    # x0=xmm[,c(1)],x1=xmm[,c(2)],
    x2 = xmm[(38 + i), c(3)],
    x3 = xmm[(38 + i), c(4)],
    x4 = xmm[(38 + i), c(5)],
    x5 = xmm[(38 + i), c(6)],
    x6 = xmm[(38 + i), c(7)]
  ) # ,x7=xmm[(36+i),c(8)],
  # x8=xmm[(36+i),c(9)],x9=xmm[(36+i),c(10)])
  # x10=xmm[(37+i),c(11)],x11=xmm[(37+i),c(12)],
  # x12=xmm[(37+i),c(13)],x13=xmm[(37+i),c(14)],
  # x14=xmm[(37+i),c(15)])
  # acf(md.rec$residuals)
  md.yrec[i] <- predict(rec.o, newdata = df.p1)
}
yhtp <- ts(md.yrec, start = c(2002, 3), freq = 4)
ts.plot(ytp, yhtp, col = c(1, 2))
YHT$oecd3qm1 <- md.yrec
RES$oecd3qm1 <- yhtp - ytp
RMSE$oecd3qm1 <- sqrt(sum((yhtp - ytp)**2) / (length(ytp)))
MAE$oecd3qm1 <- sum(abs(yhtp - ytp)) / (length(ytp))
acf(RES$oecd3qm1)
ts.plot((RES$oecd3qm1))

# Relatime forcasts, data situation second last month of previuos quartal

md.yrec <- rep(0, np)
zzm <- rep(NA, np)

for (i in 1:(np)) {
  xt <- ts(oecd.dat[, ((15 + (3 * i)) - 3)], start = c(1984, 1), freq = 12)
  x <- window(xt, start = c(1992, 1), end = c(2018, 6), extend = T)
  xmm <- mls(x, 0:9, 3)
  xmm <- xmm[-c(1:3), ]
  df.o1 <- data.frame(
    y0 = y0[i:(37 + i)], # y1=y1,
    # y2=y2[i:(36+i)],
    y3 = y3[i:(37 + i)], # y4=y4[i:(36+i)],
    # x0=xmm[,c(1)],x1=xmm[,c(2)],
    # x2=xmm[i:(36+i),c(3)],
    x3 = xmm[i:(37 + i), c(4)],
    x4 = xmm[i:(37 + i), c(5)],
    x5 = xmm[i:(37 + i), c(6)],
    x6 = xmm[i:(37 + i), c(7)]
  ) # ,x7=xmm[i:(35+i),c(8)],
  # x8=xmm[i:(35+i),c(9)],x9=xmm[i:(35+i),c(10)])
  # x10=xmm[i:(36+i),c(11)],x11=xmm[i:(36+i),c(12)],
  # x12=xmm[i:(36+i),c(13)],x13=xmm[i:(36+i),c(14)],
  # x14=xmm[i:(36+i),c(15)])
  mdc <- regsubsets(y0 ~ ., df.o1, nbest = 1, nvmax = 19, force.in = NULL, method = "exhaustive")
  z <- which.min(summary(mdc)$bic)
  zz <- coef(mdc, z)
  zzz <- paste(names(zz)[-1], collapse = "+")
  zm <- paste("y0", zzz, sep = "~")
  zzm[i + 1] <- zm
  rec.o <- lm(as.formula(zm), df.o1)

  xt <- ts(oecd.dat[, ((15 + (3 * (i + 1))) - 3)], start = c(1984, 1), freq = 12)
  x <- window(xt, start = c(1992, 1), end = c(2018, 6), extend = T)
  xmm <- mls(x, 0:9, 3)
  xmm <- xmm[-c(1:3), ]
  df.p1 <- data.frame(
    y0 = y0[(38 + i)], # y1=y1,
    # y2=y2[(37+i)],
    y3 = y3[(38 + i)], # y4=y4[(37+i)],
    # x0=xmm[,c(1)],x1=xmm[,c(2)],
    # x2=xmm[(37+i),c(3)],
    x3 = xmm[(38 + i), c(4)],
    x4 = xmm[(38 + i), c(5)],
    x5 = xmm[(38 + i), c(6)],
    x6 = xmm[(38 + i), c(7)]
  ) # ,x7=xmm[(36+i),c(8)],
  # x8=xmm[(36+i),c(9)],x9=xmm[(36+i),c(10)])
  # x10=xmm[(37+i),c(11)],x11=xmm[(37+i),c(12)],
  # x12=xmm[(37+i),c(13)],x13=xmm[(37+i),c(14)],
  # x14=xmm[(37+i),c(15)])
  # acf(md.rec$residuals)
  md.yrec[i] <- predict(rec.o, newdata = df.p1)
}
yhtp <- ts(md.yrec, start = c(2002, 3), freq = 4)
ts.plot(ytp, yhtp, col = c(1, 2))
YHT$oecd2qm1 <- md.yrec
RES$oecd2qm1 <- yhtp - ytp
RMSE$oecd2qm1 <- sqrt(sum((yhtp - ytp)**2) / (length(ytp)))
MAE$oecd2qm1 <- sum(abs(yhtp - ytp)) / (length(ytp))
acf(RES$oecd2qm1)
ts.plot((RES$oecd2qm1))

# Relatime forcasts, data situation firts month of previous quartal
md.yrec <- rep(0, np)
zzm <- rep(NA, np)

for (i in 1:(np)) {
  xt <- ts(oecd.dat[, ((14 + (3 * i)) - 3)], start = c(1984, 1), freq = 12)
  x <- window(xt, start = c(1992, 1), end = c(2018, 6), extend = T)
  xmm <- mls(x, 0:9, 3)
  xmm <- xmm[-c(1:3), ]
  df.o1 <- data.frame(
    y0 = y0[i:(37 + i)], # y1=y1,
    # y2=y2[i:(36+i)],
    y3 = y3[i:(37 + i)], # y4=y4[i:(36+i)],
    # x0=xmm[,c(1)],x1=xmm[,c(2)],
    # x2=xmm[i:(36+i),c(3)],
    # x3=xmm[i:(36+i),c(4)],
    x4 = xmm[i:(37 + i), c(5)],
    x5 = xmm[i:(37 + i), c(6)],
    x6 = xmm[i:(37 + i), c(7)]
  ) # ,
  # x7=xmm[i:(35+i),c(8)],
  # x8=xmm[i:(35+i),c(9)],x9=xmm[i:(35+i),c(10)])
  # x10=xmm[i:(36+i),c(11)],x11=xmm[i:(36+i),c(12)],
  # x12=xmm[i:(36+i),c(13)],x13=xmm[i:(36+i),c(14)],
  # x14=xmm[i:(36+i),c(15)])
  mdc <- regsubsets(y0 ~ ., df.o1, nbest = 1, nvmax = 19, force.in = NULL, method = "exhaustive")
  z <- which.min(summary(mdc)$bic)
  zz <- coef(mdc, z)
  zzz <- paste(names(zz)[-1], collapse = "+")
  zm <- paste("y0", zzz, sep = "~")
  zzm[i + 1] <- zm
  rec.o <- lm(as.formula(zm), df.o1)

  xt <- ts(oecd.dat[, ((14 + (3 * (i + 1))) - 3)], start = c(1984, 1), freq = 12)
  x <- window(xt, start = c(1992, 1), end = c(2018, 6), extend = T)
  xmm <- mls(x, 0:9, 3)
  xmm <- xmm[-c(1:3), ]
  df.p1 <- data.frame(
    y0 = y0[(38 + i)], # y1=y1,
    # y2=y2[(37+i)],
    y3 = y3[(38 + i)], # y4=y4[(37+i)],
    # x0=xmm[,c(1)],x1=xmm[,c(2)],
    # x2=xmm[(37+i),c(3)],
    # x3=xmm[(37+i),c(4)],
    x4 = xmm[(38 + i), c(5)],
    x5 = xmm[(38 + i), c(6)],
    x6 = xmm[(38 + i), c(7)]
  ) # ,
  # x7=xmm[(36+i),c(8)],
  # x8=xmm[(36+i),c(9)],x9=xmm[(36+i),c(10)])
  # x10=xmm[(37+i),c(11)],x11=xmm[(37+i),c(12)],
  # x12=xmm[(37+i),c(13)],x13=xmm[(37+i),c(14)],
  # x14=xmm[(37+i),c(15)])
  # acf(md.rec$residuals)
  md.yrec[i] <- predict(rec.o, newdata = df.p1)
}
yhtp <- ts(md.yrec, start = c(2002, 3), freq = 4)
ts.plot(ytp, yhtp, col = c(1, 2))
YHT$oecd1qm1 <- md.yrec
RES$oecd1qm1 <- yhtp - ytp
RMSE$oecd1qm1 <- sqrt(sum((yhtp - ytp)**2) / (length(ytp)))
MAE$oecd1qm1 <- sum(abs(yhtp - ytp)) / (length(ytp))
acf(RES$oecd1qm1)
ts.plot((RES$oecd1qm1))



# Test versus Benchmark Model
dm.test(RES$aro1, RES$oecd3, alternative = "greater", h = 1, power = 2)
dm.test(RES$aro1, RES$oecd3, alternative = "greater", h = 2, power = 2)

dm.test(RES$aro1, RES$oecd2, alternative = "greater", h = 1, power = 2)
dm.test(RES$aro1, RES$oecd2, alternative = "greater", h = 2, power = 2)

dm.test(RES$aro1, RES$oecd1, alternative = "greater", h = 1, power = 2)
dm.test(RES$aro1, RES$oecd1, alternative = "greater", h = 2, power = 2)

dm.test(RES$aro2, RES$oecd3qm1, alternative = "greater", h = 1, power = 2)
dm.test(RES$aro2, RES$oecd3qm1, alternative = "greater", h = 2, power = 2)

dm.test(RES$aro2, RES$oecd2qm1, alternative = "greater", h = 1, power = 2)
dm.test(RES$aro2, RES$oecd2qm1, alternative = "greater", h = 2, power = 2)

dm.test(RES$aro2, RES$oecd1qm1, alternative = "greater", h = 1, power = 2)
dm.test(RES$aro2, RES$oecd1qm1, alternative = "greater", h = 2, power = 2)

# WES ########
# Pseudo relatime forcasts
md.yrec <- rep(0, np)
zzm <- rep(NA, np)

for (i in 1:(np)) {
  xt <- ifo.wes.world
  x <- window(xt, start = c(1992, 1), end = c(2018, 2), extend = T)
  x0 <- x[5:110]
  x1 <- x[4:109]
  x2 <- x[3:108]
  x3 <- x[2:107]
  x4 <- x[1:106]
  df.w1 <- data.frame(
    y0 = y0[i:(37 + i)], # y1=y1,
    y2 = y2[i:(37 + i)], y3 = y3[i:(37 + i)], # y4=y4[i:(36+i)],
    x0 = x0[i:(37 + i)],
    x1 = x1[i:(37 + i)],
    x2 = x2[i:(37 + i)], x3 = x3[i:(37 + i)]
  ) # ,
  # x4=x4[i:(36+i)])
  mdc <- regsubsets(y0 ~ ., df.w1, nbest = 1, nvmax = 10, force.in = NULL, method = "exhaustive")
  z <- which.min(summary(mdc)$bic)
  zz <- coef(mdc, z)
  zzz <- paste(names(zz)[-1], collapse = "+")
  zm <- paste("y0", zzz, sep = "~")
  zzm[i + 1] <- zm
  rec.w <- lm(as.formula(zm), df.w1)


  df.p1 <- data.frame(
    y0 = y0[(38 + i)], # y1=y1,
    y2 = y2[(38 + i)], y3 = y3[(38 + i)], # y4=y4[(37+i)],
    x0 = x0[(38 + i)],
    x1 = x1[(38 + i)],
    x2 = x2[(38 + i)], x3 = x3[(38 + i)]
  ) # ,
  # x4=x4[(37+i)])
  md.yrec[i] <- predict(rec.w, newdata = df.p1)
}
yhtp <- ts(md.yrec, start = c(2002, 3), freq = 4)
YHT$ifo2 <- md.yrec
RES$ifo2 <- yhtp - ytp
RMSE$ifo2 <- sqrt(sum((yhtp - ytp)**2) / (length(ytp)))
MAE$ifo2 <- sum(abs(yhtp - ytp)) / (length(ytp))

# Pseudo relatime forcasts, data situation beginning of quartal

md.yrec <- rep(0, np)
zzm <- rep(NA, np)

for (i in 1:(np)) {
  xt <- ifo.wes.world
  x <- window(xt, start = c(1992, 1), end = c(2018, 2), extend = T)
  x0 <- x[5:110]
  x1 <- x[4:109]
  x2 <- x[3:108]
  x3 <- x[2:107]
  x4 <- x[1:106]
  df.w1 <- data.frame(
    y0 = y0[i:(37 + i)], # y1=y1,
    y2 = y2[i:(37 + i)], y3 = y3[i:(37 + i)], # y4=y4[i:(36+i)],
    # x0=x0[i:(36+i)],
    x1 = x1[i:(37 + i)],
    x2 = x2[i:(37 + i)], x3 = x3[i:(37 + i)]
  ) # ,
  # x4=x4[i:(36+i)])
  mdc <- regsubsets(y0 ~ ., df.w1, nbest = 1, nvmax = 10, force.in = NULL, method = "exhaustive")
  z <- which.min(summary(mdc)$bic)
  zz <- coef(mdc, z)
  zzz <- paste(names(zz)[-1], collapse = "+")
  zm <- paste("y0", zzz, sep = "~")
  zzm[i + 1] <- zm
  rec.w <- lm(as.formula(zm), df.w1)


  df.p1 <- data.frame(
    y0 = y0[(38 + i)], # y1=y1,
    y2 = y2[(38 + i)], y3 = y3[(38 + i)], # y4=y4[(37+i)],
    # x0=x0[(37+i)],
    x1 = x1[(38 + i)],
    x2 = x2[(38 + i)], x3 = x3[(38 + i)]
  ) # ,
  # x4=x4[(37+i)])
  # acf(md.rec$residuals)
  md.yrec[i] <- predict(rec.w, newdata = df.p1)
}
yhtp <- ts(md.yrec, start = c(2002, 3), freq = 4)
ts.plot(ytp, yhtp, col = c(1, 2))
YHT$ifo1 <- md.yrec
RES$ifo1 <- yhtp - ytp
RMSE$ifo1 <- sqrt(sum((yhtp - ytp)**2) / (length(ytp)))
MAE$ifo1 <- sum(abs(yhtp - ytp)) / (length(ytp))
acf(RES$ifo1)
ts.plot((RES$ifo1))



# Pseudo relatime forcasts, data situation middle of previous quartal
md.yrec <- rep(0, np)
zzm <- rep(NA, np)

for (i in 1:(np)) {
  xt <- ifo.wes.world
  x <- window(xt, start = c(1992, 1), end = c(2018, 2), extend = T)
  x0 <- x[5:110]
  x1 <- x[4:109]
  x2 <- x[3:108]
  x3 <- x[2:107]
  x4 <- x[1:106]
  df.w1 <- data.frame(
    y0 = y0[i:(37 + i)], # y1=y1,
    # y2=y2[i:(36+i)],
    y3 = y3[i:(37 + i)], # y4=y4[i:(36+i)],
    # x0=x0[i:(36+i)],
    x1 = x1[i:(37 + i)],
    x2 = x2[i:(37 + i)], x3 = x3[i:(37 + i)]
  ) # ,
  # x4=x4[i:(36+i)])
  mdc <- regsubsets(y0 ~ ., df.w1, nbest = 1, nvmax = 10, force.in = NULL, method = "exhaustive")
  z <- which.min(summary(mdc)$bic)
  zz <- coef(mdc, z)
  zzz <- paste(names(zz)[-1], collapse = "+")
  zm <- paste("y0", zzz, sep = "~")
  zzm[i + 1] <- zm
  rec.w <- lm(as.formula(zm), df.w1)


  df.p1 <- data.frame(
    y0 = y0[(38 + i)], # y1=y1,
    # y2=y2[(37+i)],
    y3 = y3[(38 + i)], # y4=y4[(37+i)],
    # x0=x0[(37+i)],
    x1 = x1[(38 + i)],
    x2 = x2[(38 + i)], x3 = x3[(38 + i)]
  ) # ,
  # x4=x4[(37+i)])
  # acf(md.rec$residuals)
  md.yrec[i] <- predict(rec.w, newdata = df.p1)
}
yhtp <- ts(md.yrec, start = c(2002, 3), freq = 4)
YHT$ifo2q1m <- md.yrec
RES$ifo2q1m <- yhtp - ytp
RMSE$ifo2q1m <- sqrt(sum((yhtp - ytp)**2) / (length(ytp)))
MAE$ifo2q1m <- sum(abs(yhtp - ytp)) / (length(ytp))

# Pseudo relatime forcasts, data situation beginning of previous quartal

md.yrec <- rep(0, np)
zzm <- rep(NA, np)

for (i in 1:(np)) {
  xt <- ifo.wes.world
  x <- window(xt, start = c(1992, 1), end = c(2018, 2), extend = T)
  x0 <- x[5:110]
  x1 <- x[4:109]
  x2 <- x[3:108]
  x3 <- x[2:107]
  x4 <- x[1:106]
  df.w1 <- data.frame(
    y0 = y0[i:(37 + i)], # y1=y1,
    # y2=y2[i:(36+i)],
    y3 = y3[i:(37 + i)], # y4=y4[i:(36+i)],
    # x0=x0[i:(36+i)],
    # x1=x1[i:(36+i)],
    x2 = x2[i:(37 + i)], x3 = x3[i:(37 + i)]
  ) # ,
  # x4=x4[i:(36+i)])
  mdc <- regsubsets(y0 ~ ., df.w1, nbest = 1, nvmax = 10, force.in = NULL, method = "exhaustive")
  z <- which.min(summary(mdc)$bic)
  zz <- coef(mdc, z)
  zzz <- paste(names(zz)[-1], collapse = "+")
  zm <- paste("y0", zzz, sep = "~")
  zzm[i + 1] <- zm
  rec.w <- lm(as.formula(zm), df.w1)


  df.p1 <- data.frame(
    y0 = y0[(38 + i)], # y1=y1,
    # y2=y2[(37+i)],
    y3 = y3[(38 + i)], # y4=y4[(37+i)],
    # x0=x0[(37+i)],
    # x1=x1[(37+i)],
    x2 = x2[(38 + i)], x3 = x3[(38 + i)]
  ) # ,
  # x4=x4[(37+i)])
  # acf(md.rec$residuals)
  md.yrec[i] <- predict(rec.w, newdata = df.p1)
}
yhtp <- ts(md.yrec, start = c(2002, 3), freq = 4)
YHT$ifo1q1m <- md.yrec
RES$ifo1q1m <- yhtp - ytp
RMSE$ifo1q1m <- sqrt(sum((yhtp - ytp)**2) / (length(ytp)))
MAE$ifo1q1m <- sum(abs(yhtp - ytp)) / (length(ytp))

# Test
dm.test(RES$aro1, RES$ifo2, alternative = "greater", h = 1, power = 2)
dm.test(RES$aro1, RES$ifo2, alternative = "greater", h = 2, power = 2)

dm.test(RES$aro1, RES$ifo1, alternative = "greater", h = 1, power = 2)
dm.test(RES$aro1, RES$ifo1, alternative = "greater", h = 2, power = 2)


dm.test(RES$aro2, RES$ifo2q1m, alternative = "greater", h = 1, power = 2)
dm.test(RES$aro2, RES$ifo2q1m, alternative = "greater", h = 2, power = 2)

dm.test(RES$aro2, RES$ifo1q1m, alternative = "greater", h = 1, power = 2)
dm.test(RES$aro2, RES$ifo1q1m, alternative = "greater", h = 2, power = 2)

dm.test(RES$ifo2, RES$oecd3, alternative = "greater", h = 1, power = 2)
dm.test(RES$ifo2, RES$oecd3, alternative = "greater", h = 2, power = 2)
dm.test(RES$ifo2, RES$oecd3, alternative = "two.sided", h = 2, power = 2)

dm.test(RES$ifo2, RES$oecd2, alternative = "greater", h = 1, power = 2)
dm.test(RES$ifo2, RES$oecd2, alternative = "greater", h = 2, power = 2)
dm.test(RES$ifo2, RES$oecd2, alternative = "two.sided", h = 2, power = 2)

dm.test(RES$ifo1, RES$oecd1, alternative = "greater", h = 1, power = 2)
dm.test(RES$ifo1, RES$oecd1, alternative = "greater", h = 2, power = 2)
dm.test(RES$ifo1, RES$oecd1, alternative = "two.sided", h = 2, power = 2)

dm.test(RES$ifo2q1m, RES$oecd3qm1, alternative = "greater", h = 1, power = 2)
dm.test(RES$ifo2q1m, RES$oecd3qm1, alternative = "greater", h = 2, power = 2)
dm.test(RES$ifo2q1m, RES$oecd3qm1, alternative = "two.sided", h = 2, power = 2)

dm.test(RES$ifo2q1m, RES$oecd2qm1, alternative = "greater", h = 1, power = 2)
dm.test(RES$ifo2q1m, RES$oecd2qm1, alternative = "greater", h = 2, power = 2)
dm.test(RES$ifo2q1m, RES$oecd2qm1, alternative = "two.sided", h = 2, power = 2)

dm.test(RES$ifo1q1m, RES$oecd1qm1, alternative = "greater", h = 1, power = 2)
dm.test(RES$ifo1q1m, RES$oecd1qm1, alternative = "greater", h = 2, power = 2)
dm.test(RES$ifo1q1m, RES$oecd1qm1, alternative = "two.sided", h = 2, power = 2)




# Global Baro coincident ######
kof.dat <- kof.dat1

# Relatime forcasts, data situation last month of quarter

md.yrec <- rep(0, np)
zzm <- rep(NA, np)

for (i in 1:(np)) {
  xk <- ts(kof.dat[, (4 + (3 * i))], start = c(1991, 7), freq = 12)
  x <- window(xk, start = c(1992, 1), end = c(2018, 6), extend = T)
  xmm <- mls(x, 0:9, 3)
  xmm <- xmm[-c(1:4), ]
  df.o1 <- data.frame(
    y0 = y0[i:(37 + i)], # y1=y1,
    y2 = y2[i:(37 + i)], y3 = y3[i:(37 + i)], # y4=y4[i:(36+i)],
    x0 = xmm[i:(37 + i), c(1)],
    x1 = xmm[i:(37 + i), c(2)],
    x2 = xmm[i:(37 + i), c(3)], x3 = xmm[i:(37 + i), c(4)],
    x4 = xmm[i:(37 + i), c(5)], x5 = xmm[i:(37 + i), c(6)],
    x6 = xmm[i:(37 + i), c(7)], x7 = xmm[i:(37 + i), c(8)],
    x8 = xmm[i:(37 + i), c(9)]
  ) # ,x9=xmm[i:(36+i),c(10)])
  # x10=xmm[i:(36+i),c(11)],x11=xmm[i:(36+i),c(12)],
  # x12=xmm[i:(36+i),c(13)],x13=xmm[i:(36+i),c(14)],
  # x14=xmm[i:(36+i),c(15)])
  mdc <- regsubsets(y0 ~ ., df.o1, nbest = 1, nvmax = 19, force.in = NULL, method = "exhaustive")
  z <- which.min(summary(mdc)$bic)
  zz <- coef(mdc, z)
  zzz <- paste(names(zz)[-1], collapse = "+")
  zm <- paste("y0", zzz, sep = "~")
  zzm[i + 1] <- zm
  rec.o <- lm(as.formula(zm), df.o1)

  xk <- ts(kof.dat[, (4 + (3 * (i + 1)))], start = c(1991, 7), freq = 12)
  x <- window(xk, start = c(1992, 1), end = c(2018, 6), extend = T)
  xmm <- mls(x, 0:9, 3)
  xmm <- xmm[-c(1:4), ]
  df.p1 <- data.frame(
    y0 = y0[(38 + i)], # y1=y1,
    y2 = y2[(38 + i)], y3 = y3[(38 + i)], # y4=y4[(37+i)],
    x0 = xmm[(38 + i), c(1)],
    x1 = xmm[(38 + i), c(2)],
    x2 = xmm[(38 + i), c(3)], x3 = xmm[(38 + i), c(4)],
    x4 = xmm[(38 + i), c(5)], x5 = xmm[(38 + i), c(6)],
    x6 = xmm[(38 + i), c(7)], x7 = xmm[(38 + i), c(8)],
    x8 = xmm[(38 + i), c(9)]
  ) # ,x9=xmm[(37+i),c(10)])
  # x10=xmm[(37+i),c(11)],x11=xmm[(37+i),c(12)],
  # x12=xmm[(37+i),c(13)],x13=xmm[(37+i),c(14)],
  # x14=xmm[(37+i),c(15)])
  # acf(md.rec$residuals)
  md.yrec[i] <- predict(rec.o, newdata = df.p1)
}
yhtp <- ts(md.yrec, start = c(2002, 3), freq = 4)
YHT$kof3 <- md.yrec
RES$kof3 <- yhtp - ytp
RMSE$kof3 <- sqrt(sum((yhtp - ytp)**2) / (length(ytp)))
MAE$kof3 <- sum(abs(yhtp - ytp)) / (length(ytp))

# Relatime forcasts, data situation second last month of quarter
md.yrec <- rep(0, np)
zzm <- rep(NA, np)

for (i in 1:(np)) {
  xk <- ts(kof.dat[, (3 + (3 * i))], start = c(1991, 7), freq = 12)
  x <- window(xk, start = c(1992, 1), end = c(2018, 6), extend = T)
  xmm <- mls(x, 0:9, 3)
  xmm <- xmm[-c(1:4), ]
  df.o1 <- data.frame(
    y0 = y0[i:(37 + i)], # y1=y1,
    y2 = y2[i:(37 + i)], y3 = y3[i:(37 + i)], # y4=y4[i:(36+i)],
    # x0=xmm[,c(1)],
    x1 = xmm[i:(37 + i), c(2)],
    x2 = xmm[i:(37 + i), c(3)],
    x3 = xmm[i:(37 + i), c(4)],
    x4 = xmm[i:(37 + i), c(5)], x5 = xmm[i:(37 + i), c(6)],
    x6 = xmm[i:(37 + i), c(7)], x7 = xmm[i:(37 + i), c(8)],
    x8 = xmm[i:(37 + i), c(9)]
  ) # ,x9=xmm[i:(36+i),c(10)])
  # x10=xmm[i:(36+i),c(11)],x11=xmm[i:(36+i),c(12)],
  # x12=xmm[i:(36+i),c(13)],x13=xmm[i:(36+i),c(14)],
  # x14=xmm[i:(36+i),c(15)])
  mdc <- regsubsets(y0 ~ ., df.o1, nbest = 1, nvmax = 19, force.in = NULL, method = "exhaustive")
  z <- which.min(summary(mdc)$bic)
  zz <- coef(mdc, z)
  zzz <- paste(names(zz)[-1], collapse = "+")
  zm <- paste("y0", zzz, sep = "~")
  zzm[i + 1] <- zm
  rec.o <- lm(as.formula(zm), df.o1)

  xk <- ts(kof.dat[, (3 + (3 * (i + 1)))], start = c(1991, 7), freq = 12)
  x <- window(xk, start = c(1992, 1), end = c(2018, 6), extend = T)
  xmm <- mls(x, 0:9, 3)
  xmm <- xmm[-c(1:4), ]
  df.p1 <- data.frame(
    y0 = y0[(38 + i)], # y1=y1,
    y2 = y2[(38 + i)], y3 = y3[(38 + i)], # y4=y4[(37+i)],
    # x0=xmm[,c(1)],
    x1 = xmm[(38 + i), c(2)],
    x2 = xmm[(38 + i), c(3)],
    x3 = xmm[(38 + i), c(4)],
    x4 = xmm[(38 + i), c(5)], x5 = xmm[(38 + i), c(6)],
    x6 = xmm[(38 + i), c(7)], x7 = xmm[(38 + i), c(8)],
    x8 = xmm[(38 + i), c(9)]
  ) # ,x9=xmm[(37+i),c(10)])
  # x10=xmm[(37+i),c(11)],x11=xmm[(37+i),c(12)],
  # x12=xmm[(37+i),c(13)],x13=xmm[(37+i),c(14)],
  # x14=xmm[(37+i),c(15)])
  # acf(md.rec$residuals)
  md.yrec[i] <- predict(rec.o, newdata = df.p1)
}
yhtp <- ts(md.yrec, start = c(2002, 3), freq = 4)
YHT$kof2 <- md.yrec
RES$kof2 <- yhtp - ytp
RMSE$kof2 <- sqrt(sum((yhtp - ytp)**2) / (length(ytp)))
MAE$kof2 <- sum(abs(yhtp - ytp)) / (length(ytp))

# Relatime forcasts, data situation first month of quarter
md.yrec <- rep(0, np)
zzm <- rep(NA, np)

for (i in 1:(np)) {
  xk <- ts(kof.dat[, (2 + (3 * i))], start = c(1991, 7), freq = 12)
  x <- window(xk, start = c(1992, 1), end = c(2018, 6), extend = T)
  xmm <- mls(x, 0:9, 3)
  xmm <- xmm[-c(1:4), ]
  df.o1 <- data.frame(
    y0 = y0[i:(37 + i)], # y1=y1,
    y2 = y2[i:(37 + i)], y3 = y3[i:(37 + i)], # y4=y4[i:(36+i)],
    # x0=xmm[,c(1)],x1=xmm[,c(2)],
    x2 = xmm[i:(37 + i), c(3)],
    x3 = xmm[i:(37 + i), c(4)],
    x4 = xmm[i:(37 + i), c(5)], x5 = xmm[i:(37 + i), c(6)],
    x6 = xmm[i:(37 + i), c(7)], x7 = xmm[i:(37 + i), c(8)],
    x8 = xmm[i:(37 + i), c(9)]
  ) # ,x9=xmm[i:(36+i),c(10)])
  # x10=xmm[i:(36+i),c(11)],x11=xmm[i:(36+i),c(12)],
  # x12=xmm[i:(36+i),c(13)],x13=xmm[i:(36+i),c(14)],
  # x14=xmm[i:(36+i),c(15)])
  mdc <- regsubsets(y0 ~ ., df.o1, nbest = 1, nvmax = 19, force.in = NULL, method = "exhaustive")
  z <- which.min(summary(mdc)$bic)
  zz <- coef(mdc, z)
  zzz <- paste(names(zz)[-1], collapse = "+")
  zm <- paste("y0", zzz, sep = "~")
  zzm[i + 1] <- zm
  rec.o <- lm(as.formula(zm), df.o1)

  xk <- ts(kof.dat[, (2 + (3 * (i + 1)))], start = c(1991, 7), freq = 12)
  x <- window(xk, start = c(1992, 1), end = c(2018, 6), extend = T)
  xmm <- mls(x, 0:9, 3)
  xmm <- xmm[-c(1:4), ]
  df.p1 <- data.frame(
    y0 = y0[(38 + i)], # y1=y1,
    y2 = y2[(38 + i)], y3 = y3[(38 + i)], # y4=y4[(37+i)],
    # x0=xmm[,c(1)],x1=xmm[,c(2)],
    x2 = xmm[(38 + i), c(3)],
    x3 = xmm[(38 + i), c(4)],
    x4 = xmm[(38 + i), c(5)], x5 = xmm[(38 + i), c(6)],
    x6 = xmm[(38 + i), c(7)], x7 = xmm[(38 + i), c(8)],
    x8 = xmm[(38 + i), c(9)]
  ) # ,x9=xmm[(37+i),c(10)])
  # x10=xmm[(37+i),c(11)],x11=xmm[(37+i),c(12)],
  # x12=xmm[(37+i),c(13)],x13=xmm[(37+i),c(14)],
  # x14=xmm[(37+i),c(15)])
  # acf(md.rec$residuals)
  md.yrec[i] <- predict(rec.o, newdata = df.p1)
}
yhtp <- ts(md.yrec, start = c(2002, 3), freq = 4)
YHT$kof1 <- md.yrec
RES$kof1 <- yhtp - ytp
RMSE$kof1 <- sqrt(sum((yhtp - ytp)**2) / (length(ytp)))
MAE$kof1 <- sum(abs(yhtp - ytp)) / (length(ytp))

# Relatime forcasts, data situation last month od previous quarter

md.yrec <- rep(0, np)
zzm <- rep(NA, np)

for (i in 1:(np)) {
  xk <- ts(kof.dat[, ((4 + (3 * i)) - 3)], start = c(1991, 7), freq = 12)
  x <- window(xk, start = c(1992, 1), end = c(2018, 6), extend = T)
  xmm <- mls(x, 0:9, 3)
  xmm <- xmm[-c(1:3), ]
  df.o1 <- data.frame(
    y0 = y0[i:(37 + i)], # y1=y1,
    # y2=y2[i:(36+i)],
    y3 = y3[i:(37 + i)], # y4=y4[i:(36+i)],
    x0 = xmm[i:(37 + i), c(1)],
    x1 = xmm[i:(37 + i), c(2)],
    x2 = xmm[i:(37 + i), c(3)],
    x3 = xmm[i:(37 + i), c(4)],
    x4 = xmm[i:(37 + i), c(5)],
    x5 = xmm[i:(37 + i), c(6)]
  ) # ,
  # x6=xmm[i:(35+i),c(7)],x7=xmm[i:(35+i),c(8)],
  # x8=xmm[i:(35+i),c(9)])#,x9=xmm[i:(36+i),c(10)])
  # x10=xmm[i:(36+i),c(11)],x11=xmm[i:(36+i),c(12)],
  # x12=xmm[i:(36+i),c(13)],x13=xmm[i:(36+i),c(14)],
  # x14=xmm[i:(36+i),c(15)])
  mdc <- regsubsets(y0 ~ ., df.o1, nbest = 1, nvmax = 19, force.in = NULL, method = "exhaustive")
  z <- which.min(summary(mdc)$bic)
  zz <- coef(mdc, z)
  zzz <- paste(names(zz)[-1], collapse = "+")
  zm <- paste("y0", zzz, sep = "~")
  zzm[i + 1] <- zm
  rec.o <- lm(as.formula(zm), df.o1)

  xk <- ts(kof.dat[, ((4 + (3 * (i + 1))) - 3)], start = c(1991, 7), freq = 12)
  x <- window(xk, start = c(1992, 1), end = c(2018, 6), extend = T)
  xmm <- mls(x, 0:9, 3)
  xmm <- xmm[-c(1:3), ]
  df.p1 <- data.frame(
    y0 = y0[(38 + i)], # y1=y1,
    # y2=y2[(37+i)],
    y3 = y3[(38 + i)], # y4=y4[(37+i)],
    x0 = xmm[(38 + i), c(1)],
    x1 = xmm[(38 + i), c(2)],
    x2 = xmm[(38 + i), c(3)],
    x3 = xmm[(38 + i), c(4)],
    x4 = xmm[(38 + i), c(5)],
    x5 = xmm[(38 + i), c(6)]
  ) # ,
  # x6=xmm[(36+i),c(7)],x7=xmm[(36+i),c(8)],
  # x8=xmm[(36+i),c(9)])#,x9=xmm[(37+i),c(10)])
  # x10=xmm[(37+i),c(11)],x11=xmm[(37+i),c(12)],
  # x12=xmm[(37+i),c(13)],x13=xmm[(37+i),c(14)],
  # x14=xmm[(37+i),c(15)])
  # acf(md.rec$residuals)
  md.yrec[i] <- predict(rec.o, newdata = df.p1)
}
yhtp <- ts(md.yrec, start = c(2002, 3), freq = 4)
YHT$kof3qm1 <- md.yrec
RES$kof3qm1 <- yhtp - ytp
RMSE$kof3qm1 <- sqrt(sum((yhtp - ytp)**2) / (length(ytp)))
MAE$kof3qm1 <- sum(abs(yhtp - ytp)) / (length(ytp))

# Relatime forcasts, data situation second moth of previous quarter
md.yrec <- rep(0, np)
zzm <- rep(NA, np)

for (i in 1:(np)) {
  xk <- ts(kof.dat[, ((3 + (3 * i)) - 3)], start = c(1991, 7), freq = 12)
  x <- window(xk, start = c(1992, 1), end = c(2018, 6), extend = T)
  xmm <- mls(x, 0:9, 3)
  xmm <- xmm[-c(1:3), ]
  df.o1 <- data.frame(
    y0 = y0[i:(37 + i)], # y1=y1,
    # y2=y2[i:(36+i)],
    y3 = y3[i:(37 + i)], # y4=y4[i:(36+i)],
    # x0=xmm[,c(1)],
    x1 = xmm[i:(37 + i), c(2)],
    x2 = xmm[i:(37 + i), c(3)],
    x3 = xmm[i:(37 + i), c(4)],
    x4 = xmm[i:(37 + i), c(5)],
    x5 = xmm[i:(37 + i), c(6)]
  ) # ,
  # x6=xmm[i:(35+i),c(7)],x7=xmm[i:(35+i),c(8)],
  # x8=xmm[i:(35+i),c(9)])#,x9=xmm[i:(36+i),c(10)])
  # x10=xmm[i:(36+i),c(11)],x11=xmm[i:(36+i),c(12)],
  # x12=xmm[i:(36+i),c(13)],x13=xmm[i:(36+i),c(14)],
  # x14=xmm[i:(36+i),c(15)])
  mdc <- regsubsets(y0 ~ ., df.o1, nbest = 1, nvmax = 19, force.in = NULL, method = "exhaustive")
  z <- which.min(summary(mdc)$bic)
  zz <- coef(mdc, z)
  zzz <- paste(names(zz)[-1], collapse = "+")
  zm <- paste("y0", zzz, sep = "~")
  zzm[i + 1] <- zm
  rec.o <- lm(as.formula(zm), df.o1)

  xk <- ts(kof.dat[, ((3 + (3 * (i + 1))) - 3)], start = c(1991, 7), freq = 12)
  x <- window(xk, start = c(1992, 1), end = c(2018, 6), extend = T)
  xmm <- mls(x, 0:9, 3)
  xmm <- xmm[-c(1:3), ]
  df.p1 <- data.frame(
    y0 = y0[(38 + i)], # y1=y1,
    # y2=y2[(37+i)],
    y3 = y3[(38 + i)], # y4=y4[(37+i)],
    # x0=xmm[,c(1)],
    x1 = xmm[(38 + i), c(2)],
    x2 = xmm[(38 + i), c(3)],
    x3 = xmm[(38 + i), c(4)],
    x4 = xmm[(38 + i), c(5)],
    x5 = xmm[(38 + i), c(6)]
  ) # ,
  # x6=xmm[(36+i),c(7)],x7=xmm[(36+i),c(8)],
  # x8=xmm[(36+i),c(9)])#,x9=xmm[(37+i),c(10)])
  # x10=xmm[(37+i),c(11)],x11=xmm[(37+i),c(12)],
  # x12=xmm[(37+i),c(13)],x13=xmm[(37+i),c(14)],
  # x14=xmm[(37+i),c(15)])
  # acf(md.rec$residuals)
  md.yrec[i] <- predict(rec.o, newdata = df.p1)
}
yhtp <- ts(md.yrec, start = c(2002, 3), freq = 4)
YHT$kof2qm1 <- md.yrec
RES$kof2qm1 <- yhtp - ytp
RMSE$kof2qm1 <- sqrt(sum((yhtp - ytp)**2) / (length(ytp)))
MAE$kof2qm1 <- sum(abs(yhtp - ytp)) / (length(ytp))

# Relatime forcasts, data situation forst month of previous quarter

md.yrec <- rep(0, np)
zzm <- rep(NA, np)

for (i in 1:(np)) {
  xk <- ts(kof.dat[, ((2 + (3 * i)) - 3)], start = c(1991, 7), freq = 12)
  x <- window(xk, start = c(1992, 1), end = c(2018, 6), extend = T)
  xmm <- mls(x, 0:9, 3)
  xmm <- xmm[-c(1:3), ]
  df.o1 <- data.frame(
    y0 = y0[i:(37 + i)], # y1=y1,
    # y2=y2[i:(36+i)],
    y3 = y3[i:(37 + i)], # y4=y4[i:(36+i)],
    # x0=xmm[,c(1)],x1=xmm[,c(2)],
    x2 = xmm[i:(37 + i), c(3)],
    x3 = xmm[i:(37 + i), c(4)],
    x4 = xmm[i:(37 + i), c(5)],
    x5 = xmm[i:(37 + i), c(6)]
  ) # ,
  # x6=xmm[i:(35+i),c(7)],
  # x7=xmm[i:(35+i),c(8)],
  # x8=xmm[i:(35+i),c(9)])#,x9=xmm[i:(36+i),c(10)])
  # x10=xmm[i:(36+i),c(11)],x11=xmm[i:(36+i),c(12)],
  # x12=xmm[i:(36+i),c(13)],x13=xmm[i:(36+i),c(14)],
  # x14=xmm[i:(36+i),c(15)])
  mdc <- regsubsets(y0 ~ ., df.o1, nbest = 1, nvmax = 19, force.in = NULL, method = "exhaustive")
  z <- which.min(summary(mdc)$bic)
  zz <- coef(mdc, z)
  zzz <- paste(names(zz)[-1], collapse = "+")
  zm <- paste("y0", zzz, sep = "~")
  zzm[i + 1] <- zm
  rec.o <- lm(as.formula(zm), df.o1)

  xk <- ts(kof.dat[, ((2 + (3 * (i + 1))) - 3)], start = c(1991, 7), freq = 12)
  x <- window(xk, start = c(1992, 1), end = c(2018, 6), extend = T)
  xmm <- mls(x, 0:9, 3)
  xmm <- xmm[-c(1:3), ]
  df.p1 <- data.frame(
    y0 = y0[(38 + i)], # y1=y1,
    # y2=y2[(37+i)],
    y3 = y3[(38 + i)], # y4=y4[(37+i)],
    # x0=xmm[,c(1)],x1=xmm[,c(2)],
    x2 = xmm[(38 + i), c(3)],
    x3 = xmm[(38 + i), c(4)],
    x4 = xmm[(38 + i), c(5)],
    x5 = xmm[(38 + i), c(6)]
  ) # ,
  # x6=xmm[(36+i),c(7)],
  # x7=xmm[(36+i),c(8)],
  # x8=xmm[(36+i),c(9)])#,x9=xmm[(37+i),c(10)])
  # x10=xmm[(37+i),c(11)],x11=xmm[(37+i),c(12)],
  #  x12=xmm[(37+i),c(13)],x13=xmm[(37+i),c(14)],
  #  x14=xmm[(37+i),c(15)])
  # acf(md.rec$residuals)
  md.yrec[i] <- predict(rec.o, newdata = df.p1)
}
yhtp <- ts(md.yrec, start = c(2002, 3), freq = 4)
YHT$kof1qm1 <- md.yrec
RES$kof1qm1 <- yhtp - ytp
RMSE$kof1qm1 <- sqrt(sum((yhtp - ytp)**2) / (length(ytp)))
MAE$kof1qm1 <- sum(abs(yhtp - ytp)) / (length(ytp))

dm.test(RES$aro1, RES$kof3, alternative = "greater", h = 1, power = 2)
dm.test(RES$aro1, RES$kof3, alternative = "greater", h = 2, power = 2)

dm.test(RES$aro1, RES$kof2, alternative = "greater", h = 1, power = 2)
dm.test(RES$aro1, RES$kof2, alternative = "greater", h = 2, power = 2)

dm.test(RES$aro1, RES$kof1, alternative = "greater", h = 1, power = 2)
dm.test(RES$aro1, RES$kof1, alternative = "greater", h = 2, power = 2)

dm.test(RES$aro2, RES$kof3qm1, alternative = "greater", h = 1, power = 2)
dm.test(RES$aro2, RES$kof3qm1, alternative = "greater", h = 2, power = 2)

dm.test(RES$aro2, RES$kof2qm1, alternative = "greater", h = 1, power = 2)
dm.test(RES$aro2, RES$kof2qm1, alternative = "greater", h = 2, power = 2)

dm.test(RES$aro2, RES$kof1qm1, alternative = "greater", h = 1, power = 2)
dm.test(RES$aro2, RES$kof1qm1, alternative = "greater", h = 2, power = 2)

dm.test(RES$kof3, RES$oecd3, alternative = "greater", h = 1, power = 2)
dm.test(RES$kof3, RES$oecd3, alternative = "greater", h = 2, power = 2)
dm.test(RES$kof3, RES$oecd3, alternative = "two.sided", h = 2, power = 2)

dm.test(RES$kof2, RES$oecd2, alternative = "greater", h = 1, power = 2)
dm.test(RES$kof2, RES$oecd2, alternative = "greater", h = 2, power = 2)
dm.test(RES$kof2, RES$oecd2, alternative = "two.sided", h = 2, power = 2)

dm.test(RES$kof1, RES$oecd1, alternative = "greater", h = 1, power = 2)
dm.test(RES$kof1, RES$oecd1, alternative = "greater", h = 2, power = 2)
dm.test(RES$kof1, RES$oecd1, alternative = "two.sided", h = 2, power = 2)

dm.test(RES$kof3qm1, RES$oecd3qm1, alternative = "greater", h = 1, power = 2)
dm.test(RES$kof3qm1, RES$oecd3qm1, alternative = "greater", h = 2, power = 2)
dm.test(RES$kof3qm1, RES$oecd3qm1, alternative = "two.sided", h = 2, power = 2)
dm.test(RES$kof3qm1, RES$oecd3qm1, alternative = "less", h = 2, power = 2)
dm.test(RES$oecd3qm1, RES$kof3qm1, alternative = "greater", h = 2, power = 2)

dm.test(RES$kof2qm1, RES$oecd2qm1, alternative = "greater", h = 1, power = 2)
dm.test(RES$kof2qm1, RES$oecd2qm1, alternative = "greater", h = 2, power = 2)
dm.test(RES$kof2qm1, RES$oecd2qm1, alternative = "two.sided", h = 2, power = 2)
dm.test(RES$kof2qm1, RES$oecd2qm1, alternative = "less", h = 2, power = 2)
dm.test(RES$oecd2qm1, RES$kof2qm1, alternative = "greater", h = 2, power = 2)

dm.test(RES$kof1qm1, RES$oecd1qm1, alternative = "greater", h = 1, power = 2)
dm.test(RES$kof1qm1, RES$oecd1qm1, alternative = "greater", h = 2, power = 2)
dm.test(RES$kof1qm1, RES$oecd1qm1, alternative = "two.sided", h = 2, power = 2)


# Global BArometer leading #######
kof.dat <- kof.dat2

# Relatime forcasts, data situation last month of quarter
md.yrec <- rep(0, np)
zzm <- rep(NA, np)

for (i in 1:(np)) {
  xk <- ts(kof.dat[, (4 + (3 * i))], start = c(1991, 7), freq = 12)
  x <- window(xk, start = c(1992, 1), end = c(2018, 6), extend = T)
  xmm <- mls(x, 0:9, 3)
  xmm <- xmm[-c(1:4), ]
  df.o1 <- data.frame(
    y0 = y0[i:(37 + i)], # y1=y1,
    y2 = y2[i:(37 + i)], y3 = y3[i:(37 + i)], # y4=y4[i:(36+i)],
    x0 = xmm[i:(37 + i), c(1)],
    x1 = xmm[i:(37 + i), c(2)],
    x2 = xmm[i:(37 + i), c(3)], x3 = xmm[i:(37 + i), c(4)],
    x4 = xmm[i:(37 + i), c(5)], x5 = xmm[i:(37 + i), c(6)],
    x6 = xmm[i:(37 + i), c(7)], x7 = xmm[i:(37 + i), c(8)],
    x8 = xmm[i:(37 + i), c(9)]
  ) # ,x9=xmm[i:(36+i),c(10)])
  # x10=xmm[i:(36+i),c(11)],x11=xmm[i:(36+i),c(12)],
  # x12=xmm[i:(36+i),c(13)],x13=xmm[i:(36+i),c(14)],
  # x14=xmm[i:(36+i),c(15)])
  mdc <- regsubsets(y0 ~ ., df.o1, nbest = 1, nvmax = 19, force.in = NULL, method = "exhaustive")
  z <- which.min(summary(mdc)$bic)
  zz <- coef(mdc, z)
  zzz <- paste(names(zz)[-1], collapse = "+")
  zm <- paste("y0", zzz, sep = "~")
  zzm[i + 1] <- zm
  rec.o <- lm(as.formula(zm), df.o1)

  xk <- ts(kof.dat[, (4 + (3 * (i + 1)))], start = c(1991, 7), freq = 12)
  x <- window(xk, start = c(1992, 1), end = c(2018, 6), extend = T)
  xmm <- mls(x, 0:9, 3)
  xmm <- xmm[-c(1:4), ]
  df.p1 <- data.frame(
    y0 = y0[(38 + i)], # y1=y1,
    y2 = y2[(38 + i)], y3 = y3[(38 + i)], # y4=y4[(37+i)],
    x0 = xmm[(38 + i), c(1)],
    x1 = xmm[(38 + i), c(2)],
    x2 = xmm[(38 + i), c(3)], x3 = xmm[(38 + i), c(4)],
    x4 = xmm[(38 + i), c(5)], x5 = xmm[(38 + i), c(6)],
    x6 = xmm[(38 + i), c(7)], x7 = xmm[(38 + i), c(8)],
    x8 = xmm[(38 + i), c(9)]
  ) # ,x9=xmm[(37+i),c(10)])
  # x10=xmm[(37+i),c(11)],x11=xmm[(37+i),c(12)],
  # x12=xmm[(37+i),c(13)],x13=xmm[(37+i),c(14)],
  # x14=xmm[(37+i),c(15)])
  # acf(md.rec$residuals)
  md.yrec[i] <- predict(rec.o, newdata = df.p1)
}
yhtp <- ts(md.yrec, start = c(2002, 3), freq = 4)
YHT$kof3 <- md.yrec
RES$kof3 <- yhtp - ytp
RMSE$kof3 <- sqrt(sum((yhtp - ytp)**2) / (length(ytp)))
MAE$kof3 <- sum(abs(yhtp - ytp)) / (length(ytp))

# Relatime forcasts, data situation second month of the quarter
md.yrec <- rep(0, np)
zzm <- rep(NA, np)

for (i in 1:(np)) {
  xk <- ts(kof.dat[, (3 + (3 * i))], start = c(1991, 7), freq = 12)
  x <- window(xk, start = c(1992, 1), end = c(2018, 6), extend = T)
  xmm <- mls(x, 0:9, 3)
  xmm <- xmm[-c(1:4), ]
  df.o1 <- data.frame(
    y0 = y0[i:(37 + i)], # y1=y1,
    y2 = y2[i:(37 + i)], y3 = y3[i:(37 + i)], # y4=y4[i:(36+i)],
    # x0=xmm[,c(1)],
    x1 = xmm[i:(37 + i), c(2)],
    x2 = xmm[i:(37 + i), c(3)],
    x3 = xmm[i:(37 + i), c(4)],
    x4 = xmm[i:(37 + i), c(5)], x5 = xmm[i:(37 + i), c(6)],
    x6 = xmm[i:(37 + i), c(7)], x7 = xmm[i:(37 + i), c(8)],
    x8 = xmm[i:(37 + i), c(9)]
  ) # ,x9=xmm[i:(36+i),c(10)])
  # x10=xmm[i:(36+i),c(11)],x11=xmm[i:(36+i),c(12)],
  # x12=xmm[i:(36+i),c(13)],x13=xmm[i:(36+i),c(14)],
  # x14=xmm[i:(36+i),c(15)])
  mdc <- regsubsets(y0 ~ ., df.o1, nbest = 1, nvmax = 19, force.in = NULL, method = "exhaustive")
  z <- which.min(summary(mdc)$bic)
  zz <- coef(mdc, z)
  zzz <- paste(names(zz)[-1], collapse = "+")
  zm <- paste("y0", zzz, sep = "~")
  zzm[i + 1] <- zm
  rec.o <- lm(as.formula(zm), df.o1)

  xk <- ts(kof.dat[, (3 + (3 * (i + 1)))], start = c(1991, 7), freq = 12)
  x <- window(xk, start = c(1992, 1), end = c(2018, 6), extend = T)
  xmm <- mls(x, 0:9, 3)
  xmm <- xmm[-c(1:4), ]
  df.p1 <- data.frame(
    y0 = y0[(38 + i)], # y1=y1,
    y2 = y2[(38 + i)], y3 = y3[(38 + i)], # y4=y4[(37+i)],
    # x0=xmm[,c(1)],
    x1 = xmm[(38 + i), c(2)],
    x2 = xmm[(38 + i), c(3)],
    x3 = xmm[(38 + i), c(4)],
    x4 = xmm[(38 + i), c(5)], x5 = xmm[(38 + i), c(6)],
    x6 = xmm[(38 + i), c(7)], x7 = xmm[(38 + i), c(8)],
    x8 = xmm[(38 + i), c(9)]
  ) # ,x9=xmm[(37+i),c(10)])
  # x10=xmm[(37+i),c(11)],x11=xmm[(37+i),c(12)],
  # x12=xmm[(37+i),c(13)],x13=xmm[(37+i),c(14)],
  # x14=xmm[(37+i),c(15)])
  # acf(md.rec$residuals)
  md.yrec[i] <- predict(rec.o, newdata = df.p1)
}
yhtp <- ts(md.yrec, start = c(2002, 3), freq = 4)
YHT$kof2 <- md.yrec
RES$kof2 <- yhtp - ytp
RMSE$kof2 <- sqrt(sum((yhtp - ytp)**2) / (length(ytp)))
MAE$kof2 <- sum(abs(yhtp - ytp)) / (length(ytp))

# Relatime forcasts, data situation first month of the quarter

md.yrec <- rep(0, np)
zzm <- rep(NA, np)

for (i in 1:(np)) {
  xk <- ts(kof.dat[, (2 + (3 * i))], start = c(1991, 7), freq = 12)
  x <- window(xk, start = c(1992, 1), end = c(2018, 6), extend = T)
  xmm <- mls(x, 0:9, 3)
  xmm <- xmm[-c(1:4), ]
  df.o1 <- data.frame(
    y0 = y0[i:(37 + i)], # y1=y1,
    y2 = y2[i:(37 + i)], y3 = y3[i:(37 + i)], # y4=y4[i:(36+i)],
    # x0=xmm[,c(1)],x1=xmm[,c(2)],
    x2 = xmm[i:(37 + i), c(3)],
    x3 = xmm[i:(37 + i), c(4)],
    x4 = xmm[i:(37 + i), c(5)], x5 = xmm[i:(37 + i), c(6)],
    x6 = xmm[i:(37 + i), c(7)], x7 = xmm[i:(37 + i), c(8)],
    x8 = xmm[i:(37 + i), c(9)]
  ) # ,x9=xmm[i:(36+i),c(10)])
  # x10=xmm[i:(36+i),c(11)],x11=xmm[i:(36+i),c(12)],
  # x12=xmm[i:(36+i),c(13)],x13=xmm[i:(36+i),c(14)],
  # x14=xmm[i:(36+i),c(15)])
  mdc <- regsubsets(y0 ~ ., df.o1, nbest = 1, nvmax = 19, force.in = NULL, method = "exhaustive")
  z <- which.min(summary(mdc)$bic)
  zz <- coef(mdc, z)
  zzz <- paste(names(zz)[-1], collapse = "+")
  zm <- paste("y0", zzz, sep = "~")
  zzm[i + 1] <- zm
  rec.o <- lm(as.formula(zm), df.o1)

  xk <- ts(kof.dat[, (2 + (3 * (i + 1)))], start = c(1991, 7), freq = 12)
  x <- window(xk, start = c(1992, 1), end = c(2018, 6), extend = T)
  xmm <- mls(x, 0:9, 3)
  xmm <- xmm[-c(1:4), ]
  df.p1 <- data.frame(
    y0 = y0[(38 + i)], # y1=y1,
    y2 = y2[(38 + i)], y3 = y3[(38 + i)], # y4=y4[(37+i)],
    # x0=xmm[,c(1)],x1=xmm[,c(2)],
    x2 = xmm[(38 + i), c(3)],
    x3 = xmm[(38 + i), c(4)],
    x4 = xmm[(38 + i), c(5)], x5 = xmm[(38 + i), c(6)],
    x6 = xmm[(38 + i), c(7)], x7 = xmm[(38 + i), c(8)],
    x8 = xmm[(38 + i), c(9)]
  ) # ,x9=xmm[(37+i),c(10)])
  # x10=xmm[(37+i),c(11)],x11=xmm[(37+i),c(12)],
  # x12=xmm[(37+i),c(13)],x13=xmm[(37+i),c(14)],
  # x14=xmm[(37+i),c(15)])
  # acf(md.rec$residuals)
  md.yrec[i] <- predict(rec.o, newdata = df.p1)
}
yhtp <- ts(md.yrec, start = c(2002, 3), freq = 4)
YHT$kof1 <- md.yrec
RES$kof1 <- yhtp - ytp
RMSE$kof1 <- sqrt(sum((yhtp - ytp)**2) / (length(ytp)))
MAE$kof1 <- sum(abs(yhtp - ytp)) / (length(ytp))

# Relatime forcasts, data situaion last month of previuos quarter
md.yrec <- rep(0, np)
zzm <- rep(NA, np)

for (i in 1:(np)) {
  xk <- ts(kof.dat[, ((4 + (3 * i)) - 3)], start = c(1991, 7), freq = 12)
  x <- window(xk, start = c(1992, 1), end = c(2018, 6), extend = T)
  xmm <- mls(x, 0:9, 3)
  xmm <- xmm[-c(1:3), ]
  df.o1 <- data.frame(
    y0 = y0[i:(37 + i)], # y1=y1,
    # y2=y2[i:(36+i)],
    y3 = y3[i:(37 + i)], # y4=y4[i:(36+i)],
    x0 = xmm[i:(37 + i), c(1)],
    x1 = xmm[i:(37 + i), c(2)],
    x2 = xmm[i:(37 + i), c(3)],
    x3 = xmm[i:(37 + i), c(4)],
    x4 = xmm[i:(37 + i), c(5)],
    x5 = xmm[i:(37 + i), c(6)]
  ) # ,
  # x6=xmm[i:(35+i),c(7)],x7=xmm[i:(35+i),c(8)],
  # x8=xmm[i:(35+i),c(9)])#,x9=xmm[i:(36+i),c(10)])
  # x10=xmm[i:(36+i),c(11)],x11=xmm[i:(36+i),c(12)],
  # x12=xmm[i:(36+i),c(13)],x13=xmm[i:(36+i),c(14)],
  # x14=xmm[i:(36+i),c(15)])
  mdc <- regsubsets(y0 ~ ., df.o1, nbest = 1, nvmax = 19, force.in = NULL, method = "exhaustive")
  z <- which.min(summary(mdc)$bic)
  zz <- coef(mdc, z)
  zzz <- paste(names(zz)[-1], collapse = "+")
  zm <- paste("y0", zzz, sep = "~")
  zzm[i + 1] <- zm
  rec.o <- lm(as.formula(zm), df.o1)

  xk <- ts(kof.dat[, ((4 + (3 * (i + 1))) - 3)], start = c(1991, 7), freq = 12)
  x <- window(xk, start = c(1992, 1), end = c(2018, 6), extend = T)
  xmm <- mls(x, 0:9, 3)
  xmm <- xmm[-c(1:3), ]
  df.p1 <- data.frame(
    y0 = y0[(38 + i)], # y1=y1,
    # y2=y2[(37+i)],
    y3 = y3[(38 + i)], # y4=y4[(37+i)],
    x0 = xmm[(38 + i), c(1)],
    x1 = xmm[(38 + i), c(2)],
    x2 = xmm[(38 + i), c(3)],
    x3 = xmm[(38 + i), c(4)],
    x4 = xmm[(38 + i), c(5)],
    x5 = xmm[(38 + i), c(6)]
  ) # ,
  # x6=xmm[(36+i),c(7)],x7=xmm[(36+i),c(8)],
  # x8=xmm[(36+i),c(9)])#,x9=xmm[(37+i),c(10)])
  # x10=xmm[(37+i),c(11)],x11=xmm[(37+i),c(12)],
  # x12=xmm[(37+i),c(13)],x13=xmm[(37+i),c(14)],
  # x14=xmm[(37+i),c(15)])
  # acf(md.rec$residuals)
  md.yrec[i] <- predict(rec.o, newdata = df.p1)
}
yhtp <- ts(md.yrec, start = c(2002, 3), freq = 4)
YHT$kof3qm1 <- md.yrec
RES$kof3qm1 <- yhtp - ytp
RMSE$kof3qm1 <- sqrt(sum((yhtp - ytp)**2) / (length(ytp)))
MAE$kof3qm1 <- sum(abs(yhtp - ytp)) / (length(ytp))

# Relatime forcasts, data situation second month of previous quarter
md.yrec <- rep(0, np)
zzm <- rep(NA, np)

for (i in 1:(np)) {
  xk <- ts(kof.dat[, ((3 + (3 * i)) - 3)], start = c(1991, 7), freq = 12)
  x <- window(xk, start = c(1992, 1), end = c(2018, 6), extend = T)
  xmm <- mls(x, 0:9, 3)
  xmm <- xmm[-c(1:3), ]
  df.o1 <- data.frame(
    y0 = y0[i:(37 + i)], # y1=y1,
    # y2=y2[i:(36+i)],
    y3 = y3[i:(37 + i)], # y4=y4[i:(36+i)],
    # x0=xmm[,c(1)],
    x1 = xmm[i:(37 + i), c(2)],
    x2 = xmm[i:(37 + i), c(3)],
    x3 = xmm[i:(37 + i), c(4)],
    x4 = xmm[i:(37 + i), c(5)],
    x5 = xmm[i:(37 + i), c(6)]
  ) # ,
  # x6=xmm[i:(35+i),c(7)],x7=xmm[i:(35+i),c(8)],
  # x8=xmm[i:(35+i),c(9)])#,x9=xmm[i:(36+i),c(10)])
  # x10=xmm[i:(36+i),c(11)],x11=xmm[i:(36+i),c(12)],
  # x12=xmm[i:(36+i),c(13)],x13=xmm[i:(36+i),c(14)],
  # x14=xmm[i:(36+i),c(15)])
  mdc <- regsubsets(y0 ~ ., df.o1, nbest = 1, nvmax = 19, force.in = NULL, method = "exhaustive")
  z <- which.min(summary(mdc)$bic)
  zz <- coef(mdc, z)
  zzz <- paste(names(zz)[-1], collapse = "+")
  zm <- paste("y0", zzz, sep = "~")
  zzm[i + 1] <- zm
  rec.o <- lm(as.formula(zm), df.o1)

  xk <- ts(kof.dat[, ((3 + (3 * (i + 1))) - 3)], start = c(1991, 7), freq = 12)
  x <- window(xk, start = c(1992, 1), end = c(2018, 6), extend = T)
  xmm <- mls(x, 0:9, 3)
  xmm <- xmm[-c(1:3), ]
  df.p1 <- data.frame(
    y0 = y0[(38 + i)], # y1=y1,
    # y2=y2[(37+i)],
    y3 = y3[(38 + i)], # y4=y4[(37+i)],
    # x0=xmm[,c(1)],
    x1 = xmm[(38 + i), c(2)],
    x2 = xmm[(38 + i), c(3)],
    x3 = xmm[(38 + i), c(4)],
    x4 = xmm[(38 + i), c(5)],
    x5 = xmm[(38 + i), c(6)]
  ) # ,
  # x6=xmm[(36+i),c(7)],x7=xmm[(36+i),c(8)],
  # x8=xmm[(36+i),c(9)])#,x9=xmm[(37+i),c(10)])
  # x10=xmm[(37+i),c(11)],x11=xmm[(37+i),c(12)],
  # x12=xmm[(37+i),c(13)],x13=xmm[(37+i),c(14)],
  # x14=xmm[(37+i),c(15)])
  # acf(md.rec$residuals)
  md.yrec[i] <- predict(rec.o, newdata = df.p1)
}
yhtp <- ts(md.yrec, start = c(2002, 3), freq = 4)
YHT$kof2qm1 <- md.yrec
RES$kof2qm1 <- yhtp - ytp
RMSE$kof2qm1 <- sqrt(sum((yhtp - ytp)**2) / (length(ytp)))
MAE$kof2qm1 <- sum(abs(yhtp - ytp)) / (length(ytp))

# Relatime forcasts, data situation first month of previous quarter
md.yrec <- rep(0, np)
zzm <- rep(NA, np)

for (i in 1:(np)) {
  xk <- ts(kof.dat[, ((2 + (3 * i)) - 3)], start = c(1991, 7), freq = 12)
  x <- window(xk, start = c(1992, 1), end = c(2018, 6), extend = T)
  xmm <- mls(x, 0:9, 3)
  xmm <- xmm[-c(1:3), ]
  df.o1 <- data.frame(
    y0 = y0[i:(37 + i)], # y1=y1,
    # y2=y2[i:(36+i)],
    y3 = y3[i:(37 + i)], # y4=y4[i:(36+i)],
    # x0=xmm[,c(1)],x1=xmm[,c(2)],
    x2 = xmm[i:(37 + i), c(3)],
    x3 = xmm[i:(37 + i), c(4)],
    x4 = xmm[i:(37 + i), c(5)],
    x5 = xmm[i:(37 + i), c(6)]
  ) # ,
  # x6=xmm[i:(35+i),c(7)],
  # x7=xmm[i:(35+i),c(8)],
  # x8=xmm[i:(35+i),c(9)])#,x9=xmm[i:(36+i),c(10)])
  # x10=xmm[i:(36+i),c(11)],x11=xmm[i:(36+i),c(12)],
  # x12=xmm[i:(36+i),c(13)],x13=xmm[i:(36+i),c(14)],
  # x14=xmm[i:(36+i),c(15)])
  mdc <- regsubsets(y0 ~ ., df.o1, nbest = 1, nvmax = 19, force.in = NULL, method = "exhaustive")
  z <- which.min(summary(mdc)$bic)
  zz <- coef(mdc, z)
  zzz <- paste(names(zz)[-1], collapse = "+")
  zm <- paste("y0", zzz, sep = "~")
  zzm[i + 1] <- zm
  rec.o <- lm(as.formula(zm), df.o1)

  xk <- ts(kof.dat[, ((2 + (3 * (i + 1))) - 3)], start = c(1991, 7), freq = 12)
  x <- window(xk, start = c(1992, 1), end = c(2018, 6), extend = T)
  xmm <- mls(x, 0:9, 3)
  xmm <- xmm[-c(1:3), ]
  df.p1 <- data.frame(
    y0 = y0[(38 + i)], # y1=y1,
    # y2=y2[(37+i)],
    y3 = y3[(38 + i)], # y4=y4[(37+i)],
    # x0=xmm[,c(1)],x1=xmm[,c(2)],
    x2 = xmm[(38 + i), c(3)],
    x3 = xmm[(38 + i), c(4)],
    x4 = xmm[(38 + i), c(5)],
    x5 = xmm[(38 + i), c(6)]
  ) # ,
  # x6=xmm[(36+i),c(7)],
  # x7=xmm[(36+i),c(8)],
  # x8=xmm[(36+i),c(9)])#,x9=xmm[(37+i),c(10)])
  # x10=xmm[(37+i),c(11)],x11=xmm[(37+i),c(12)],
  #  x12=xmm[(37+i),c(13)],x13=xmm[(37+i),c(14)],
  #  x14=xmm[(37+i),c(15)])
  # acf(md.rec$residuals)
  md.yrec[i] <- predict(rec.o, newdata = df.p1)
}
yhtp <- ts(md.yrec, start = c(2002, 3), freq = 4)
YHT$kof1qm1 <- md.yrec
RES$kof1qm1 <- yhtp - ytp
RMSE$kof1qm1 <- sqrt(sum((yhtp - ytp)**2) / (length(ytp)))
MAE$kof1qm1 <- sum(abs(yhtp - ytp)) / (length(ytp))

dm.test(RES$aro1, RES$kof3, alternative = "greater", h = 1, power = 2)
dm.test(RES$aro1, RES$kof3, alternative = "greater", h = 2, power = 2)

dm.test(RES$aro1, RES$kof2, alternative = "greater", h = 1, power = 2)
dm.test(RES$aro1, RES$kof2, alternative = "greater", h = 2, power = 2)

dm.test(RES$aro1, RES$kof1, alternative = "greater", h = 1, power = 2)
dm.test(RES$aro1, RES$kof1, alternative = "greater", h = 2, power = 2)

dm.test(RES$aro2, RES$kof3qm1, alternative = "greater", h = 1, power = 2)
dm.test(RES$aro2, RES$kof3qm1, alternative = "greater", h = 2, power = 2)

dm.test(RES$aro2, RES$kof2qm1, alternative = "greater", h = 1, power = 2)
dm.test(RES$aro2, RES$kof2qm1, alternative = "greater", h = 2, power = 2)

dm.test(RES$aro2, RES$kof1qm1, alternative = "greater", h = 1, power = 2)
dm.test(RES$aro2, RES$kof1qm1, alternative = "greater", h = 2, power = 2)

dm.test(RES$kof3, RES$oecd3, alternative = "greater", h = 1, power = 2)
dm.test(RES$kof3, RES$oecd3, alternative = "greater", h = 2, power = 2)
dm.test(RES$kof3, RES$oecd3, alternative = "two.sided", h = 2, power = 2)

dm.test(RES$kof2, RES$oecd2, alternative = "greater", h = 1, power = 2)
dm.test(RES$kof2, RES$oecd2, alternative = "greater", h = 2, power = 2)
dm.test(RES$kof2, RES$oecd2, alternative = "two.sided", h = 2, power = 2)

dm.test(RES$kof1, RES$oecd1, alternative = "greater", h = 1, power = 2)
dm.test(RES$kof1, RES$oecd1, alternative = "greater", h = 2, power = 2)
dm.test(RES$kof1, RES$oecd1, alternative = "two.sided", h = 2, power = 2)

dm.test(RES$kof3qm1, RES$oecd3qm1, alternative = "greater", h = 1, power = 2)
dm.test(RES$kof3qm1, RES$oecd3qm1, alternative = "greater", h = 2, power = 2)
dm.test(RES$kof3qm1, RES$oecd3qm1, alternative = "two.sided", h = 2, power = 2)
dm.test(RES$kof3qm1, RES$oecd3qm1, alternative = "less", h = 2, power = 2)
dm.test(RES$oecd3qm1, RES$kof3qm1, alternative = "greater", h = 2, power = 2)

dm.test(RES$kof2qm1, RES$oecd2qm1, alternative = "greater", h = 1, power = 2)
dm.test(RES$kof2qm1, RES$oecd2qm1, alternative = "greater", h = 2, power = 2)
dm.test(RES$kof2qm1, RES$oecd2qm1, alternative = "two.sided", h = 2, power = 2)
dm.test(RES$kof2qm1, RES$oecd2qm1, alternative = "less", h = 2, power = 2)
dm.test(RES$oecd2qm1, RES$kof2qm1, alternative = "greater", h = 2, power = 2)


dm.test(RES$kof1qm1, RES$oecd1qm1, alternative = "greater", h = 1, power = 2)
dm.test(RES$kof1qm1, RES$oecd1qm1, alternative = "greater", h = 2, power = 2)
dm.test(RES$kof1qm1, RES$oecd1qm1, alternative = "two.sided", h = 2, power = 2)
