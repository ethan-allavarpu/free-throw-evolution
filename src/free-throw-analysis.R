library("ggplot2")
playerdata2000_2001 <- read.csv("../data/2000-2001NBA.csv", stringsAsFactors = FALSE)
playerdata2001_2002 <- read.csv("../data/2001-2002NBA.csv", stringsAsFactors = FALSE)
playerdata2002_2003 <- read.csv("../data/2002-2003NBA.csv", stringsAsFactors = FALSE)
playerdata2016_2017 <- read.csv("../data/2016-2017NBA.csv", stringsAsFactors = FALSE)
playerdata2017_2018 <- read.csv("../data/2017-2018NBA.csv", stringsAsFactors = FALSE)
playerdata2018_2019 <- read.csv("../data/2018-2019NBA.csv", stringsAsFactors = FALSE)

avg <- tapply(playerdata2000_2001$FT., playerdata2000_2001$POS, mean)
dev <- tapply(playerdata2000_2001$FT., playerdata2000_2001$POS, sd)
stats2000_2001 <- data.frame("Free Throw Percentage" = avg,"Standard Deviation" = dev)
knitr::kable(stats2000_2001)
avg <- tapply(playerdata2001_2002$FT., playerdata2001_2002$POS, mean, na.rm = TRUE)
dev <- tapply(playerdata2001_2002$FT., playerdata2001_2002$POS, sd)
stats2001_2002 <- data.frame("Free Throw Percentage" = avg,"Standard Deviation" = dev)
knitr::kable(stats2001_2002)
avg <- tapply(playerdata2002_2003$FT., playerdata2002_2003$POS, mean, na.rm = TRUE)
dev <- tapply(playerdata2002_2003$FT., playerdata2002_2003$POS, sd)
stats2002_2003 <- data.frame("Free Throw Percentage" = avg,"Standard Deviation" = dev)
knitr::kable(stats2002_2003)
avg <- tapply(playerdata2016_2017$FT., playerdata2016_2017$POS, mean)
dev <- tapply(playerdata2016_2017$FT., playerdata2016_2017$POS, sd)
stats2016_2017 <- data.frame("Free Throw Percentage" = avg,"Standard Deviation" = dev)
knitr::kable(stats2016_2017)
avg <- tapply(playerdata2017_2018$FT., playerdata2017_2018$POS, mean)
dev <- tapply(playerdata2017_2018$FT., playerdata2017_2018$POS, sd)
stats2017_2018 <- data.frame("Free Throw Percentage" = avg,"Standard Deviation" = dev)
knitr::kable(stats2017_2018)
avg <- tapply(playerdata2018_2019$FT., playerdata2018_2019$POS, mean)
dev <- tapply(playerdata2018_2019$FT., playerdata2018_2019$POS, sd)
stats2018_2019 <- data.frame("Free Throw Percentage" = avg,"Standard Deviation" = dev)
knitr::kable(stats2018_2019)
rbind(stats2000_2001, stats2001_2002, stats2002_2003)

playerdata2000_2001 <- playerdata2000_2001[!duplicated(playerdata2000_2001$PLAYER), ]
playerdata2001_2002 <- playerdata2001_2002[!duplicated(playerdata2001_2002$PLAYER), ]
playerdata2002_2003 <- playerdata2002_2003[!duplicated(playerdata2002_2003[, -10]), ]
playerdata2016_2017 <- playerdata2016_2017[!duplicated(playerdata2016_2017$PLAYER), ]
playerdata2017_2018 <- playerdata2017_2018[!duplicated(playerdata2017_2018$PLAYER), ]
playerdata2018_2019 <- playerdata2018_2019[!duplicated(playerdata2018_2019$PLAYER), ]
playerdata2002_2003 <- cbind(PLAYER = rep(c("names"), nrow(playerdata2002_2003)), playerdata2002_2003)
playerdata2000s <- rbind(playerdata2000_2001, playerdata2001_2002, playerdata2002_2003)
playerdata2010s <- rbind(playerdata2016_2017, playerdata2017_2018, playerdata2018_2019)
# write.csv(playerdata2000s, "../data/2000sNBAData.csv")
# write.csv(playerdata2010s, "../data/2010sNBAData.csv")

avg <- tapply(playerdata2000s$FT., playerdata2000s$POS, mean)
dev <- tapply(playerdata2000s$FT., playerdata2000s$POS, sd)
stats2000s <- data.frame("Free Throw Percentage" = avg,"Standard Deviation" = dev)
knitr::kable(stats2000s)
avg <- tapply(playerdata2010s$FT., playerdata2010s$POS, mean)
dev <- tapply(playerdata2010s$FT., playerdata2010s$POS, sd)
stats2010s <- data.frame("Free Throw Percentage" = avg,"Standard Deviation" = dev)
knitr::kable(stats2010s)
plot(playerdata2000s$X3P., playerdata2000s$FT.)
cor(playerdata2000s$X3P., playerdata2000s$FT.)
plot(playerdata2010s$X3P., playerdata2010s$FT.)
cor(playerdata2010s$X3P., playerdata2010s$FT.)

nonstars2000_2001 <- playerdata2000_2001[playerdata2000_2001$PTS <= 19.8 & playerdata2000_2001$REB <= 7.7 &
                                           playerdata2000_2001$AST <= 7.3, ]
nonstars2001_2002 <- playerdata2001_2002[playerdata2001_2002$PTS <= 20.4 & playerdata2001_2002$REB <= 7.4 &
                                           playerdata2001_2002$AST <= 7.5, ]
nonstars2002_2003 <- playerdata2002_2003[playerdata2002_2003$PTS <= 20 & playerdata2002_2003$REB <= 7 &
                                           playerdata2002_2003$AST <= 6.3, ]
nonstars2016_2017 <- playerdata2016_2017[playerdata2016_2017$PTS <= 22.9 & playerdata2016_2017$REB <= 8.5 &
                                           playerdata2016_2017$REB <= 6.7, ]
nonstars2017_2018 <- playerdata2017_2018[playerdata2017_2018$PTS <= 21.9 & playerdata2017_2018$REB <= 8.5 &
                                           playerdata2017_2018$REB <= 6.5, ]
nonstars2018_2019 <- playerdata2018_2019[playerdata2018_2019$PTS <= 21.4 & playerdata2018_2019$REB <= 9.7 &
                                           playerdata2018_2019$REB <= 7.3, ]

non0001 <- cor(nonstars2000_2001$X3P., nonstars2000_2001$FT.)
non0102 <- cor(nonstars2001_2002$X3P., nonstars2001_2002$FT.)
non0203 <- cor(nonstars2002_2003$X3P., nonstars2002_2003$FT.)
non1617 <- cor(nonstars2016_2017$X3P., nonstars2016_2017$FT.)
non1718 <- cor(nonstars2017_2018$X3P., nonstars2017_2018$FT.)
non1819 <- cor(nonstars2018_2019$X3P., nonstars2018_2019$FT.)
nons <- data.frame("Year" = c("00-01", "01-02", "02-03", "16-17", "17-18", "18-19"),
                   "Correlation" = c(non0001, non0102, non0203, non1617, non1718, non1819))
knitr::kable(nons)

stars2000_2001 <- playerdata2000_2001[playerdata2000_2001$PTS > 19.8 | playerdata2000_2001$REB > 7.7 |
                                        playerdata2000_2001$AST > 7.3, ]
stars2001_2002 <- playerdata2001_2002[playerdata2001_2002$PTS > 20.4 | playerdata2001_2002$REB > 7.4 |
                                        playerdata2001_2002$AST > 7.5, ]
stars2002_2003 <- playerdata2002_2003[playerdata2002_2003$PTS > 20 | playerdata2002_2003$REB > 7 |
                                        playerdata2002_2003$AST > 6.3, ]
stars2016_2017 <- playerdata2016_2017[playerdata2016_2017$PTS > 22.9 | playerdata2016_2017$REB > 8.5 |
                                        playerdata2016_2017$REB > 6.7, ]
stars2017_2018 <- playerdata2017_2018[playerdata2017_2018$PTS > 21.9 | playerdata2017_2018$REB > 8.5 |
                                        playerdata2017_2018$REB > 6.5, ]
stars2018_2019 <- playerdata2018_2019[playerdata2018_2019$PTS > 21.4 | playerdata2018_2019$REB > 9.7 |
                                        playerdata2018_2019$REB > 7.3, ]

stars00 <- cor(stars2000_2001$X3P., stars2000_2001$FT.)
stars01 <- cor(stars2001_2002$X3P., stars2001_2002$FT.)
stars02 <- cor(stars2002_2003$X3P., stars2002_2003$FT.)
stars16 <- cor(stars2016_2017$X3P., stars2016_2017$FT.)
stars17 <- cor(stars2017_2018$X3P., stars2017_2018$FT.)
stars18 <- cor(stars2018_2019$X3P., stars2018_2019$FT.)
stars <- data.frame("Year" = c("00-01", "01-02", "02-03", "16-17", "17-18", "18-19"),
                    "Correlation" = c(stars00, stars01, stars02, stars16, stars17, stars18))
knitr::kable(stars)
stars2000_data <- rbind(stars2000_2001, stars2001_2002, stars2002_2003)
stars2010_data <- rbind(stars2016_2017, stars2017_2018, stars2018_2019)
mean(stars2000_data$FT.)
sd(stars2000_data$FT.)
mean(stars2010_data$FT.)
sd(stars2010_data$FT.)
nonstars2000_data <- rbind(nonstars2000_2001, nonstars2001_2002, nonstars2002_2003)
nonstars2010_data <- rbind(nonstars2016_2017, nonstars2017_2018, nonstars2018_2019)
mean(nonstars2000_data$FT.)
sd(nonstars2000_data$FT.)
mean(nonstars2010_data$FT.)
sd(nonstars2010_data$FT.)

r <- cor(playerdata2000s$X3P., playerdata2000s$FT.)
b <- r * sd(playerdata2000s$FT.) / sd(playerdata2000s$X3P.)
n <- length(playerdata2000s$FT.)
ydevs <- numeric(length = n)
for (i in 1:n) {
  ydevs[i] <- (playerdata2000s$FT.[i] - mean(playerdata2000s$FT.))^2
}
ydevs <- sum(ydevs)
xdevs <- numeric(length = n)
for (i in 1:n) {
  xdevs[i] <- (playerdata2000s$X3P.[i] - mean(playerdata2000s$X3P.))^2
}
xdevs <- sum(xdevs)
se <- sqrt(ydevs / (n - 2)) / sqrt(xdevs)
t <- b / se
pt(t, (n - 2), lower.tail = FALSE) + pt(-t, n - 2)

r <- cor(playerdata2010s$X3P., playerdata2010s$FT.)
b <- r * sd(playerdata2010s$FT.) / sd(playerdata2010s$X3P.)
n <- length(playerdata2010s$FT.)
ydevs <- numeric(length = n)
for (i in 1:n) {
  ydevs[i] <- (playerdata2010s$FT.[i] - mean(playerdata2010s$FT.))^2
}
ydevs <- sum(ydevs)
xdevs <- numeric(length = n)
for (i in 1:n) {
  xdevs[i] <- (playerdata2010s$X3P.[i] - mean(playerdata2010s$X3P.))^2
}
xdevs <- sum(xdevs)
se <- sqrt(ydevs / (n - 2)) / sqrt(xdevs)
t <- b / se
pt(t, (n - 2), lower.tail = FALSE) + pt(-t, n - 2)
