# setwd("/home/tinea/Documents/H_et_S/Projects/github/Electoral-statistics/scripts/")

# Reading data;

spb <- read.table("../data/St.Petersburg/2020-07-01/St.Petersburg.Russia.2020-07-01.Constitution_amendments.txt", h=TRUE, sep="\t")

# Calculating variables;

spb$TURNOUT <- spb$BALLOTS.CAST / spb$VOTERS
spb$YES.sh <- spb$YES / spb$BALLOTS.CAST
spb$NO.sh <- spb$NO / spb$BALLOTS.CAST
spb$INVALID.sh <- spb$BALLOTS.INVALID / spb$BALLOTS.CAST

# Preparing for MC simulation;

# spb.YES.sh.MC <- NULL
# spb.YES.sh.MC <- as.list(spb.YES.sh.MC)
#
# for(i in 1:nrow(spb)){
# spb.YES.sh.MC[[i]] <- rbinom(1000, spb$VOTED[i], spb$YES.sh[i])/spb$VOTED[i]
# }
#
# for(i in 1:nrow(spb)){
# spb.YES.sh.MC.v <- c(spb.YES.sh.MC.v, spb.YES.sh.MC[[i]])
# }
#
# hist(spb.YES.sh.MC.v)

# Subsetting for Territorial Commissions;

spb.ls <- NULL
spb.ls <- as.list(spb.ls)

for(i in 1:30){
spb.ls[[i]] <- subset(spb, spb$TIK == i)
}

spb.pct.ls <- NULL
spb.pct.ls <- as.list(spb.pct.ls)

Breaks <- seq(-.0005, 1.0005, .001)

for(i in 1:1001){
spb.pct.ls[[i]] <- subset(spb, spb$TURNOUT > Breaks[i] & spb$TURNOUT <= Breaks[i+1])
}

spb.pct.mean.YES.sh <- NULL
spb.pct.mean.NO.sh <- NULL

for(i in 1:101){
spb.pct.mean.YES.sh <- c(spb.pct.mean.YES.sh, mean(spb.pct.ls[[i]]$YES.sh, na.rm=TRUE))
spb.pct.mean.NO.sh <- c(spb.pct.mean.NO.sh, mean(spb.pct.ls[[i]]$NO.sh, na.rm=TRUE))
}

spb.pct.mean.YES.sh.RA.3 <- (
spb.pct.mean.YES.sh[1:99] + 
spb.pct.mean.YES.sh[2:100] + 
spb.pct.mean.YES.sh[3:101])/3

spb.pct.mean.YES.sh.RA.5 <- (
spb.pct.mean.YES.sh[1:97] + 
spb.pct.mean.YES.sh[2:98] + 
spb.pct.mean.YES.sh[3:99] + 
spb.pct.mean.YES.sh[4:100] + 
spb.pct.mean.YES.sh[5:101])/5

spb.pct.mean.NO.sh.RA.3 <- (
spb.pct.mean.NO.sh[1:99] + 
spb.pct.mean.NO.sh[2:100] + 
spb.pct.mean.NO.sh[3:101])/3

spb.pct.mean.NO.sh.RA.5 <- (
spb.pct.mean.NO.sh[1:97] + 
spb.pct.mean.NO.sh[2:98] + 
spb.pct.mean.NO.sh[3:99] + 
spb.pct.mean.NO.sh[4:100] + 
spb.pct.mean.NO.sh[5:101])/5

# Control plots;

# Turnout hist;

png("../plots/St.Petersburg/2020-07-01/00.hist.turnout.png", height=750, width=750, res=120, pointsize=10)
hist.spb.TURNOUT <- hist(spb$TURNOUT, breaks=seq(-.005,1.005,.01), col=1, main="St. Petersburg, Russia, 2020-07-01 \nConstitution amendments vote", xlab="Voters' turnout, 1% centered bins")
dev.off()

png("../plots/St.Petersburg/2020-07-01/00.hist.turnout.01.png", height=750, width=750, res=120, pointsize=10)
hist.spb.TURNOUT.01 <- hist(spb$TURNOUT, breaks=seq(-.0005,1.0005,.001), col=1, main="St. Petersburg, Russia, 2020-07-01 \nConstitution amendments vote", xlab="Voters' turnout, 0.1% centered bins")
dev.off()

# Leader's share hist;

png("../plots/St.Petersburg/2020-07-01/00.hist.leader.png", height=750, width=750, res=120, pointsize=10)
hist(spb$YES.sh, breaks=seq(-.005,1.005,.01), col=1, 
main="St. Petersburg, Russia, 2020-07-01 \nConstitution amendments vote", 
xlab="YES votes share, 1% centered bins")
dev.off()

png("../plots/St.Petersburg/2020-07-01/00.hist.leader.01.png", height=750, width=750, res=120, pointsize=10)
hist.spb.YES.sh.01 <- hist(spb$YES.sh, breaks=seq(-.0005,1.0005,.001), col=1, 
main="St. Petersburg, Russia, 2020-07-01 \nConstitution amendments vote", 
xlab="YES votes share, 0.1% centered bins")
dev.off()

# Leader ~ turnout scatter;

png("../plots/St.Petersburg/2020-07-01/00.scatter.leader~turnout.png", height=750, width=750, res=120, pointsize=10)
plot(spb$TURNOUT, spb$YES.sh, 
xlim=c(0,1), ylim=c(0,1),
pch=20, cex=sqrt(spb$BALLOTS.CAST)/25, col=rgb(0,0,0,.2), 
main="St. Petersburg, Russia, 2020-07-01 \nConstitution amendments vote", 
xlab="Voters' turnout",
ylab="YES votes share")
grid()
dev.off()

# By territorial commissions;

for(i in 1:30){
png(file=paste("../plots/St.Petersburg/2020-07-01/01.scatter.leader~turnout.TEC.",i,".png", sep=""), height=750, width=750, res=120, pointsize=10)

plot(spb.ls[[i]]$TURNOUT, spb.ls[[i]]$YES.sh, 
xlim=c(0,1), ylim=c(0,1), type="n", 
main=paste("St. Petersburg, Russia, 2020-07-01, TEC no.",i,"\nConstitution amendments vote"), 
xlab="Voters' turnout",
ylab="YES votes share")

# if(i == 3){
  points(spb$TURNOUT, spb$YES.sh, pch=20, cex=sqrt(spb$BALLOTS.CAST)/25, col="lightgrey")
  for(k in 1:nrow(spb.ls[[i]])){
    if(sqrt(spb.ls[[i]]$BALLOTS.CAST)[k]/25 < .5){
      points(spb.ls[[i]]$TURNOUT[k], spb.ls[[i]]$YES.sh[k], 
      pch=21, cex=sqrt(spb.ls[[i]]$BALLOTS.CAST)[k]/9, col="white", bg="black") 
    }else{
      points(spb.ls[[i]]$TURNOUT[k], spb.ls[[i]]$YES.sh[k], 
      pch=21, cex=sqrt(spb.ls[[i]]$BALLOTS.CAST)[k]/25, col="white", bg="black") 
      }
    }
# }else{
# points(spb$TURNOUT, spb$YES.sh, pch=20, cex=sqrt(spb$BALLOTS.CAST)/25, col="lightgrey")
# points(spb.ls[[i]]$TURNOUT, spb.ls[[i]]$YES.sh, 
# pch=21, cex=sqrt(spb.ls[[i]]$BALLOTS.CAST)/25, col="white", bg="black") 
# }

grid()
dev.off()
}

png("../plots/St.Petersburg/2020-07-01/00.hist.leader.3.special.png", height=750, width=750, res=120, pointsize=10)

hist(subset(spb.ls[[3]], spb.ls[[3]]$TURNOUT > .9)$YES.sh, 
breaks=seq(-.025,1.025,.05), 
ylim=c(0,10),
col=1,
main="St. Petersburg, Russia, 2020-07-01, TEC no. 3, special stations\nConstitution amendments vote",
xlab="YES votes share, 5% centered bins"
)
abline(v=mean(subset(spb.ls[[3]], spb.ls[[3]]$TURNOUT > .9)$YES.sh), col=2)
abline(v=median(subset(spb.ls[[3]], spb.ls[[3]]$TURNOUT > .9)$YES.sh), col=3)
legend("topright", lty=1, col=2:3, legend=c(
paste("median share (", round(mean(subset(spb.ls[[3]], spb.ls[[3]]$TURNOUT > .9)$YES.sh), 4), ")", sep=""),
paste("mean share (", round(median(subset(spb.ls[[3]], spb.ls[[3]]$TURNOUT > .9)$YES.sh), 4), ")", sep="")),
bty="n"
)

dev.off()

png("../plots/St.Petersburg/2020-07-01/02.qqnorm.NO.share.png", height=750, width=750, res=120, pointsize=10)
qqnorm(spb$NO.sh, pch=20, col=rgb(.5,0,0,.1),
main="Normal Q-Q plot for the share of 'NO' votes")
dev.off()

png("../plots/St.Petersburg/2020-07-01/02.sliding.YES~TURNOUT.bins.png", height=750, width=750, res=120, pointsize=10)

plot(spb.pct.mean.YES.sh, type="l", 
ylim=c(.5,.9),
main="St. Petersburg, Russia, 2020-07-01 \nConstitution amendments vote", 
xlab="Voters' turnout, 1% bin",
ylab="Mean YES votes share in 1% turnout bin")
lines(spb.pct.mean.YES.sh.RA.3, col=2)
lines(spb.pct.mean.YES.sh.RA.5, col="darkgreen")
abline(v=0:100, lwd=.5, col=8)
abline(v=seq(0,100,5), lwd=1, col=8)
abline(v=seq(0,100,10), lty=3, col=1)

leg <- legend("topleft", lty=1, col=c("black", "red", "darkgreen"), 
legend=c("natural", "3-point rolling average", "5-point rolling average"), bg="white")

dev.off()

png("../plots/St.Petersburg/2020-07-01/02.sliding.NO~TURNOUT.bins.png", height=750, width=750, res=120, pointsize=10)

plot(spb.pct.mean.NO.sh, type="l", 
ylim=c(0,.5),
main="St. Petersburg, Russia, 2020-07-01 \nConstitution amendments vote", 
xlab="Voters' turnout, 1% bin",
ylab="Mean NO votes share in 1% turnout bin")
lines(spb.pct.mean.NO.sh.RA.3, col=2)
lines(spb.pct.mean.NO.sh.RA.5, col="darkgreen")
abline(v=0:100, lwd=.5, col=8)
abline(v=seq(0,100,5), lwd=1, col=8)
abline(v=seq(0,100,10), lty=3, col=1)

leg <- legend("topleft", lty=1, col=c("black", "red", "darkgreen"), 
legend=c("natural", "3-point rolling average", "5-point rolling average"), bg="white")

dev.off()

# Special section / requires preceding elections dataset;

spb.2018 <- read.table("/home/tinea/Documents/H_et_S/Projects/elections/2018/spb.q.20180319.txt", h=TRUE, sep="\t")

spb.2018$VOTED <- spb.2018$BALL.VALID + spb.2018$BALL.INVALID
spb.2018$TURNOUT <- spb.2018$VOTED/spb.2018$VOTERS

spb.2018$BABURIN.sh <- spb.2018$BABURIN/spb.2018$VOTED
spb.2018$GRUDININ.sh <- spb.2018$GRUDININ/spb.2018$VOTED
spb.2018$ZHIRINOVSKI.sh <- spb.2018$ZHIRINOVSKI/spb.2018$VOTED
spb.2018$PUTIN.sh <- spb.2018$PUTIN/spb.2018$VOTED
spb.2018$SOBCHAK.sh <- spb.2018$SOBCHAK/spb.2018$VOTED
spb.2018$SURAIKIN.sh <- spb.2018$SURAIKIN/spb.2018$VOTED
spb.2018$TITOV.sh <- spb.2018$TITOV/spb.2018$VOTED
spb.2018$YAVLINSKI.sh <- spb.2018$YAVLINSKI/spb.2018$VOTED

spb.2019.gvr <- read.table("/home/tinea/Documents/H_et_S/Projects/elections/2019/scripts/data/2019/gvr/spb.20190908.gvr.txt", h=TRUE, sep="\t")

spb.2019.gvr.raw <- spb.2019.gvr

spb.2019.gvr <- subset(spb.2019.gvr, spb.2019.gvr$BULL.RECEIVED >= 0)

# Calculating turnout variables

spb.2019.gvr$VOTED <- spb.2019.gvr$BALL.INVALID + spb.2019.gvr$BALL.VALID
spb.2019.gvr$TURNOUT <- spb.2019.gvr$VOTED/spb.2019.gvr$VOTERS
spb.2019.gvr$AMOSOV.sh <- spb.2019.gvr$AMOSOV/spb.2019.gvr$VOTED
spb.2019.gvr$BEGLOV.sh <- spb.2019.gvr$BEGLOV/spb.2019.gvr$VOTED
spb.2019.gvr$TIKHONOVA.sh <- spb.2019.gvr$TIKHONOVA/spb.2019.gvr$VOTED

################################################################

is.18.20 <- intersect(spb$UIK, spb.2018$UIK)
is.18.19.20 <- intersect(is.18.20, spb.2019.gvr$UIK)

spb.18.19.20.ls <- NULL
spb.18.19.20.ls <- as.list(spb.18.19.20.ls)

spb.18.TURN <- NULL
spb.19.TURN <- NULL
spb.20.TURN <- NULL

for(i in 1:length(is.18.19.20)){
spb.18.19.20.ls[[i]] <- list(
  subset(spb.2018, spb.2018$UIK==is.18.19.20[i]),
  subset(spb.2019.gvr, spb.2019.gvr$UIK==is.18.19.20[i]),
  subset(spb, spb$UIK==is.18.19.20[i])
)
spb.18.TURN <- c(spb.18.TURN, subset(spb.2018, spb.2018$UIK==is.18.19.20[i])$TURNOUT)
spb.19.TURN <- c(spb.19.TURN, subset(spb.2019.gvr, spb.2019.gvr$UIK==is.18.19.20[i])$TURNOUT)
spb.20.TURN <- c(spb.20.TURN, subset(spb, spb$UIK==is.18.19.20[i])$TURNOUT)
}

spb.18.19.20.TURN <- cbind.data.frame(
is.18.19.20,
spb.18.TURN,
spb.19.TURN,
spb.20.TURN
)

colnames(spb.18.19.20.TURN) <- c("UIK","T18","T19","T20")

spb.pct.18.19.20.TURN.ls <- NULL
spb.pct.18.19.20.TURN.ls <- as.list(spb.pct.18.19.20.TURN.ls)

Breaks <- seq(-.005, 1.005, .01)

for(i in 1:101){
spb.pct.18.19.20.TURN.ls[[i]] <- subset(
spb.18.19.20.TURN, 
spb.18.19.20.TURN$T18 > Breaks[i] & 
spb.18.19.20.TURN$T18 <= Breaks[i+1])
}

spb.pct.mean.T19.sh <- NULL
spb.pct.mean.T20.sh <- NULL

for(i in 1:101){
spb.pct.mean.T19.sh <- c(spb.pct.mean.T19.sh, mean(spb.pct.18.19.20.TURN.ls[[i]]$T19, na.rm=TRUE))
spb.pct.mean.T20.sh <- c(spb.pct.mean.T20.sh, mean(spb.pct.18.19.20.TURN.ls[[i]]$T20, na.rm=TRUE))
}

plot(0:1, 0:1, type="n")
points(spb.18.19.20.TURN$T19, spb.18.19.20.TURN$T20, pch=20, col=rgb(0,0,0,.2))

spb.pct.mean.YES.sh.RA.3 <- (
spb.pct.mean.YES.sh[1:99] + 
spb.pct.mean.YES.sh[2:100] + 
spb.pct.mean.YES.sh[3:101])/3

spb.pct.mean.YES.sh.RA.5 <- (
spb.pct.mean.YES.sh[1:97] + 
spb.pct.mean.YES.sh[2:98] + 
spb.pct.mean.YES.sh[3:99] + 
spb.pct.mean.YES.sh[4:100] + 
spb.pct.mean.YES.sh[5:101])/5

###############################################################################
# Declaring objects for the loop
###############################################################################
# MK.repeats <- 100 # For preliminary testing;
# MK.repeats <- 1000 # For preliminary testing;
# DO NOT try the following line on larger datasets. 10,000 iterations on
# the dataset for the whole Russia (97+K records) and for 0.1% bins
# may take days to complete.

MK.repeats <- 10000 # Working repeats number;

spb.ksp.rbinom.ls <- NULL # ls of simulated polling stations;
spb.ksp.rbinom.ls <- as.list(spb.ksp.rbinom.ls)
spb.ksp.rbinom.YES.sh <- NULL # complete vector of simulated turnouts;

# df for simulated histograms' counts:

spb.ksp.rbinom.YES.sh.hist.counts.df <- NULL
spb.ksp.rbinom.YES.sh.hist.counts.df <- as.data.frame(spb.ksp.rbinom.YES.sh.hist.counts.df)

j <- NULL
i <- NULL

###############################################################################
# Main simulation loop begins
###############################################################################

for (k in 1:MK.repeats) {
spb.ksp.rbinom.ls <- NULL # ls of simulated polling stations;
spb.ksp.rbinom.ls <- as.list(spb.ksp.rbinom.ls)
spb.ksp.rbinom.YES.sh <- NULL # complete vector of simulated turnouts;

# Simulating polling stations' YES.sh
j <- 1
while(j <= length(spb$BALLOTS.CAST)){
spb.ksp.rbinom.ls[[j]] <- rbinom(n=1, size=spb$BALLOTS.CAST[j], prob=spb$YES.sh[j])
j <- j + 1
}

# Gathering simulated YES.sh in a vector

i <- 1
while(i <= length(spb$BALLOTS.CAST)){
spb.ksp.rbinom.YES.sh <- c(spb.ksp.rbinom.YES.sh, spb.ksp.rbinom.ls[[i]]/spb$BALLOTS.CAST[i])
i <- i + 1
}

# Calculating hist for simulated turnouts, bin 1% (hist plots appear
# in the graphics console)

spb.ksp.rbinom.YES.sh.hist <- hist(spb.ksp.rbinom.YES.sh, breaks=seq(-.0005, 1.0005, .001))

# Extracting simulated hist counts

spb.ksp.rbinom.YES.sh.hist.counts.df <- rbind.data.frame(
spb.ksp.rbinom.YES.sh.hist.counts.df,
spb.ksp.rbinom.YES.sh.hist$counts
)
gc()
}

###############################################################################
# Main simulation loop ends
###############################################################################

# Extracting summary stats for counts from all MK.repeats simulated hists into
# a data frame

spb.ksp.rbinom.YES.sh.hist.counts.MIN <- NULL
spb.ksp.rbinom.YES.sh.hist.counts.MAX <- NULL
spb.ksp.rbinom.YES.sh.hist.counts.MEAN <- NULL
spb.ksp.rbinom.YES.sh.hist.counts.SD <- NULL
spb.ksp.rbinom.YES.sh.hist.counts.MEDIAN <- NULL
spb.ksp.rbinom.YES.sh.hist.counts.Q1 <- NULL
spb.ksp.rbinom.YES.sh.hist.counts.Q3 <- NULL
spb.ksp.rbinom.YES.sh.hist.counts.IQR <- NULL

for (i in 1:1001){
spb.ksp.rbinom.YES.sh.hist.counts.MEAN <- c(
spb.ksp.rbinom.YES.sh.hist.counts.MEAN,
mean(spb.ksp.rbinom.YES.sh.hist.counts.df[,i])
)

spb.ksp.rbinom.YES.sh.hist.counts.MIN <- c(
spb.ksp.rbinom.YES.sh.hist.counts.MIN,
min(spb.ksp.rbinom.YES.sh.hist.counts.df[,i]
)
)

spb.ksp.rbinom.YES.sh.hist.counts.MAX <- c(
spb.ksp.rbinom.YES.sh.hist.counts.MAX,
max(spb.ksp.rbinom.YES.sh.hist.counts.df[,i]
)
)

spb.ksp.rbinom.YES.sh.hist.counts.SD <- c(
spb.ksp.rbinom.YES.sh.hist.counts.SD,
sd(spb.ksp.rbinom.YES.sh.hist.counts.df[,i])
)

spb.ksp.rbinom.YES.sh.hist.counts.MEDIAN <- c(
spb.ksp.rbinom.YES.sh.hist.counts.MEDIAN,
median(spb.ksp.rbinom.YES.sh.hist.counts.df[,i])
)

spb.ksp.rbinom.YES.sh.hist.counts.Q1 <- c(
spb.ksp.rbinom.YES.sh.hist.counts.Q1,
summary(spb.ksp.rbinom.YES.sh.hist.counts.df[,i])[2]
)

spb.ksp.rbinom.YES.sh.hist.counts.Q3 <- c(
spb.ksp.rbinom.YES.sh.hist.counts.Q3,
summary(spb.ksp.rbinom.YES.sh.hist.counts.df[,i])[5]
)

spb.ksp.rbinom.YES.sh.hist.counts.IQR <- c(
spb.ksp.rbinom.YES.sh.hist.counts.IQR,
IQR(spb.ksp.rbinom.YES.sh.hist.counts.df[,i]))
}

spb.hist.ksp.simulated.stats.1pct <- data.frame(
seq(0, 1, .001),
spb.ksp.rbinom.YES.sh.hist.counts.MEAN,
spb.ksp.rbinom.YES.sh.hist.counts.SD,
spb.ksp.rbinom.YES.sh.hist.counts.MEDIAN,
spb.ksp.rbinom.YES.sh.hist.counts.Q1,
spb.ksp.rbinom.YES.sh.hist.counts.Q3,
spb.ksp.rbinom.YES.sh.hist.counts.IQR,
spb.ksp.rbinom.YES.sh.hist.counts.MIN,
spb.ksp.rbinom.YES.sh.hist.counts.MAX
)

colnames(spb.hist.ksp.simulated.stats.1pct) <-
c("PCT","MEAN","SD","MEDIAN","Q1","Q3","IQR","MIN","MAX")

# Control plots

png("../plots/St.Petersburg/2020-07-01/03.hist.YES.sh.spb.MEAN-SD.simul.png", height=750, width=750, res=120, pointsize=10)

hist(spb$YES.sh, breaks=seq(-.0005, 1.0005, .001),
col=rgb(0,0,1,.3), border=rgb(0,0,.2,.5),
main="Constitution amendments vote in Russia, 2020-07-01\nSt. Petersburg",
xlab="YES shares, 0.1% bin",
ylab="Frequency")

polygon(c(spb.hist.ksp.simulated.stats.1pct$PCT,
spb.hist.ksp.simulated.stats.1pct$PCT[1001:1]),
c(spb.hist.ksp.simulated.stats.1pct$MEAN +
3.09*spb.hist.ksp.simulated.stats.1pct$SD,
(spb.hist.ksp.simulated.stats.1pct$MEAN -
3.09*spb.hist.ksp.simulated.stats.1pct$SD)[1001:1]),
col=rgb(0,0,0,.1), border=rgb(1,0,0,.1))

polygon(c(spb.hist.ksp.simulated.stats.1pct$PCT,
spb.hist.ksp.simulated.stats.1pct$PCT[1001:1]),
c(spb.hist.ksp.simulated.stats.1pct$MEAN +
2.58*spb.hist.ksp.simulated.stats.1pct$SD,
(spb.hist.ksp.simulated.stats.1pct$MEAN -
2.58*spb.hist.ksp.simulated.stats.1pct$SD)[1001:1]),
col=rgb(0,0,0,.1), border=rgb(1,0,0,.1))

polygon(c(spb.hist.ksp.simulated.stats.1pct$PCT,
spb.hist.ksp.simulated.stats.1pct$PCT[1001:1]),
c(spb.hist.ksp.simulated.stats.1pct$MEAN +
1.96*spb.hist.ksp.simulated.stats.1pct$SD,
(spb.hist.ksp.simulated.stats.1pct$MEAN -
1.96*spb.hist.ksp.simulated.stats.1pct$SD)[1001:1]),
col=rgb(0,0,0,.1), border=rgb(1,0,0,.1))

# polygon(c(spb.hist.ksp.simulated.stats.1pct$PCT,
# spb.hist.ksp.simulated.stats.1pct$PCT[1001:1]),
# c(spb.hist.ksp.simulated.stats.1pct$MEDIAN +
# 1.5*spb.hist.ksp.simulated.stats.1pct$IQR,
# (spb.hist.ksp.simulated.stats.1pct$MEDIAN -
# 1.5*spb.hist.ksp.simulated.stats.1pct$IQR)[1001:1]),
# col=rgb(0,0,0,.3), border=rgb(1,0,0,.3))

lines(spb.hist.ksp.simulated.stats.1pct$PCT,
spb.hist.ksp.simulated.stats.1pct$MEAN, col=2, lwd=1)

legend("topleft", lwd=c(2,.5), col=c(2,2),
legend=c("MC-simulated mean", "Mean +/- 1.96, 2.58, and 3.09 SD"), bty="n")

axis(1, at=seq(0,1,.1), labels=FALSE, lwd=1.5)

dev.off()

png("hist.YES.sh.spb.MEAN-SD.simul.png", height=750, width=750)
par(cex=1.5, lwd=1.5)

hist(spb$YES.sh, breaks=seq(-.005, 1.005, .01),
ylim=c(0,190), col=rgb(0,0,1,.3), border=rgb(0,0,1,.3),
main="Presidential elections in Russia, 2018-03-18\nSt. Petersburg",
xlab="YES votes share, 1% bin",
ylab="Frequency")

polygon(c(spb.hist.ksp.simulated.stats.1pct$PCT,
spb.hist.ksp.simulated.stats.1pct$PCT[101:1]),
c(spb.hist.ksp.simulated.stats.1pct$MEAN +
3*spb.hist.ksp.simulated.stats.1pct$SD,
(spb.hist.ksp.simulated.stats.1pct$MEAN -
3*spb.hist.ksp.simulated.stats.1pct$SD)[101:1]),
col=rgb(0,0,0,.3), border=rgb(1,0,0,.3))

points(spb.hist.ksp.simulated.stats.1pct$PCT,
spb.hist.ksp.simulated.stats.1pct$MEAN, type="l", col=2, lwd=3)

legend("topleft", lwd=c(3,1), col=c(2,2),
legend=c("MC-simulated mean", "MC-simulated mean +/- 3 SD"), bty="n")

axis(1, at=seq(0,1,.1), labels=FALSE, lwd=1.5)
axis(2, at=seq(0,190,10), tcl=-.25, labels=FALSE, lwd=1.5)

dev.off()

# Участок 776
# Избирателей в списке : 2295
# Выдано бюллетеней : 1780
# Обнаружено бюллетеней : 1777
# Недействительных : 986
# ДА : 459
# НЕТ : 332

# > subset(spb, spb$UIK==776)
#                    REGION TIK UIK                                  TIK.NAME
# 565 Город Санкт-Петербург   7 776 Территориальная избирательная комиссия №7
#     UIK.NAME VOTERS BALLOTS.ISSUED BALLOTS.CAST BALLOTS.INVALID YES YES.pct  NO
# 565 УИК №776   2295           1780         1777             986 459  25.83% 332
#     NO.pct   TURNOUT    YES.sh     NO.sh INVALID.sh
# 565 18.68% 0.7742919 0.2583005 0.1868317  0.5548678

U776.YES.desired <- (986+459)/1777
U776.TURNOUT.desired <- 1777/2295
U776.YES.20200701 <- 459/(459+332)
U776.TURNOUT.20200701 <- (459+332)/2295
U776.YES <- 459/1777
U776.TURNOUT <- 1777/2295

# Участок 775
# Избирателей в списке : 2391
# Выдано бюллетеней : 1017
# Обнаружено бюллетеней : 1017
# Недействительных : 300
# ДА : 464
# НЕТ : 253

# > subset(spb, spb$UIK==775)
#                    REGION TIK UIK                                  TIK.NAME
# 564 Город Санкт-Петербург   7 775 Территориальная избирательная комиссия №7
#     UIK.NAME VOTERS BALLOTS.ISSUED BALLOTS.CAST BALLOTS.INVALID YES YES.pct  NO
# 564 УИК №775   2391           1017         1017             300 464  45.62% 253
#     NO.pct  TURNOUT    YES.sh     NO.sh INVALID.sh
# 564 24.88% 0.425345 0.4562439 0.2487709  0.2949853

U775.YES.desired <- (300+464)/1017
U775.TURNOUT.desired <- 1017/2391
U775.YES.20200701 <- 464/(300+464)
U775.TURNOUT.20200701 <- (300+464)/2391
U775.YES <- 464/1017
U775.TURNOUT <- 1017/2391

png(file=paste("../plots/St.Petersburg/2020-07-01/01.scatter.leader~turnout.TEC.7.PS.776.png", sep=""), height=750, width=750, res=120, pointsize=10)

plot(spb.ls[[7]]$TURNOUT, spb.ls[[7]]$YES.sh, 
xlim=c(0,1), ylim=c(0,1), type="n", 
main=paste("St. Petersburg, Russia, TEC no. 7, Station no. 776\nConstitution amendments vote, 2020-07-01"), 
xlab="Voters' turnout",
ylab="YES votes share")

# if(i == 3){
  points(spb$TURNOUT, spb$YES.sh, pch=20, cex=sqrt(spb$BALLOTS.CAST)/25, col="lightgrey")
  for(k in 1:nrow(spb.ls[[7]])){
    if(sqrt(spb.ls[[7]]$BALLOTS.CAST)[k]/25 < .5){
      points(spb.ls[[7]]$TURNOUT[k], spb.ls[[7]]$YES.sh[k], 
      pch=21, cex=sqrt(spb.ls[[7]]$BALLOTS.CAST)[k]/9, col="white", bg="black") 
    }else{
      points(spb.ls[[7]]$TURNOUT[k], spb.ls[[7]]$YES.sh[k], 
      pch=21, cex=sqrt(spb.ls[[7]]$BALLOTS.CAST)[k]/25, col="white", bg="black") 
      }
    }

grid()

points(
x=c(U776.TURNOUT.desired, U776.TURNOUT, U776.TURNOUT.20200701), 
y=c(U776.YES.desired, U776.YES, U776.YES.20200701), 
pch=21, cex=sqrt(c(1777, 1777, (459+332)))/25, col="white", bg="red")

arrows(
x0=c(U776.TURNOUT.desired, U776.TURNOUT -.01),
x1=c(U776.TURNOUT, U776.TURNOUT.20200701 + .01),
y0=c(U776.YES.desired - .02, U776.YES + .01),
y1=c(U776.YES + .02, U776.YES.20200701 - .01),
col="red",
angle=10
)

text(
x=c(U776.TURNOUT.desired, U776.TURNOUT, U776.TURNOUT.20200701), 
y=c(U776.YES.desired, U776.YES, U776.YES.20200701), 
labels=1:3,
pos=c(4,4,2))

dev.off()


png(file=paste("../plots/St.Petersburg/2020-07-01/01.scatter.leader~turnout.TEC.7.PS.775.png", sep=""), height=750, width=750, res=120, pointsize=10)

plot(spb.ls[[7]]$TURNOUT, spb.ls[[7]]$YES.sh, 
xlim=c(0,1), ylim=c(0,1), type="n", 
main=paste("St. Petersburg, Russia, TEC no. 7, Station no. 775\nConstitution amendments vote, 2020-07-01"), 
xlab="Voters' turnout",
ylab="YES votes share")

# if(i == 3){
  points(spb$TURNOUT, spb$YES.sh, pch=20, cex=sqrt(spb$BALLOTS.CAST)/25, col="lightgrey")
  for(k in 1:nrow(spb.ls[[7]])){
    if(sqrt(spb.ls[[7]]$BALLOTS.CAST)[k]/25 < .5){
      points(spb.ls[[7]]$TURNOUT[k], spb.ls[[7]]$YES.sh[k], 
      pch=21, cex=sqrt(spb.ls[[7]]$BALLOTS.CAST)[k]/9, col="white", bg="black") 
    }else{
      points(spb.ls[[7]]$TURNOUT[k], spb.ls[[7]]$YES.sh[k], 
      pch=21, cex=sqrt(spb.ls[[7]]$BALLOTS.CAST)[k]/25, col="white", bg="black") 
      }
    }

grid()

points(
x=c(U775.TURNOUT.desired, U775.TURNOUT, U775.TURNOUT.20200701), 
y=c(U775.YES.desired, U775.YES, U775.YES.20200701), 
pch=21, cex=sqrt(c(1777, 1777, (459+332)))/25, col="white", bg="red")

arrows(
x0=c(U775.TURNOUT.desired, U775.TURNOUT -.01),
x1=c(U775.TURNOUT, U775.TURNOUT.20200701 + .01),
y0=c(U775.YES.desired - .02, U775.YES + .01),
y1=c(U775.YES + .02, U775.YES.20200701 - .01),
col="red",
angle=10
)

text(
x=c(U775.TURNOUT.desired, U775.TURNOUT, U775.TURNOUT.20200701), 
y=c(U775.YES.desired, U775.YES, U775.YES.20200701), 
labels=1:3,
pos=c(4,4,2))

dev.off()

# Same in Russian

png(file=paste("../plots/St.Petersburg/2020-07-01/01.scatter.leader~turnout.TEC.7.PS.776.ru.png", sep=""), height=750, width=750, res=120, pointsize=10)

plot(spb.ls[[7]]$TURNOUT, spb.ls[[7]]$YES.sh, 
xlim=c(0,1), ylim=c(0,1), type="n", 
main=paste("С.-Петербург, Россия, ТИК № 7, Участок № 776\nГолосование по поправкам в Конституции, 2020-07-01"), 
xlab="Явка на участке",
ylab="Доля голосов ЗА")

# if(i == 3){
  points(spb$TURNOUT, spb$YES.sh, pch=20, cex=sqrt(spb$BALLOTS.CAST)/25, col="lightgrey")
  for(k in 1:nrow(spb.ls[[7]])){
    if(sqrt(spb.ls[[7]]$BALLOTS.CAST)[k]/25 < .5){
      points(spb.ls[[7]]$TURNOUT[k], spb.ls[[7]]$YES.sh[k], 
      pch=21, cex=sqrt(spb.ls[[7]]$BALLOTS.CAST)[k]/9, col="white", bg="black") 
    }else{
      points(spb.ls[[7]]$TURNOUT[k], spb.ls[[7]]$YES.sh[k], 
      pch=21, cex=sqrt(spb.ls[[7]]$BALLOTS.CAST)[k]/25, col="white", bg="black") 
      }
    }

grid()

points(
x=c(U776.TURNOUT.desired, U776.TURNOUT, U776.TURNOUT.20200701), 
y=c(U776.YES.desired, U776.YES, U776.YES.20200701), 
pch=21, cex=sqrt(c(1777, 1777, (459+332)))/25, col="white", bg="red")

arrows(
x0=c(U776.TURNOUT.desired, U776.TURNOUT -.01),
x1=c(U776.TURNOUT, U776.TURNOUT.20200701 + .01),
y0=c(U776.YES.desired - .02, U776.YES + .01),
y1=c(U776.YES + .02, U776.YES.20200701 - .01),
col="red",
angle=10
)

text(
x=c(U776.TURNOUT.desired, U776.TURNOUT, U776.TURNOUT.20200701), 
y=c(U776.YES.desired, U776.YES, U776.YES.20200701), 
labels=1:3,
pos=c(4,4,2))

dev.off()


png(file=paste("../plots/St.Petersburg/2020-07-01/01.scatter.leader~turnout.TEC.7.PS.775.ru.png", sep=""), height=750, width=750, res=120, pointsize=10)

plot(spb.ls[[7]]$TURNOUT, spb.ls[[7]]$YES.sh, 
xlim=c(0,1), ylim=c(0,1), type="n", 
main=paste("С.-Петербург, Россия, ТИК № 7, Участок № 775\nГолосование по поправкам в Конституции, 2020-07-01"), 
xlab="Явка на участке",
ylab="Доля голосов ЗА")

# if(i == 3){
  points(spb$TURNOUT, spb$YES.sh, pch=20, cex=sqrt(spb$BALLOTS.CAST)/25, col="lightgrey")
  for(k in 1:nrow(spb.ls[[7]])){
    if(sqrt(spb.ls[[7]]$BALLOTS.CAST)[k]/25 < .5){
      points(spb.ls[[7]]$TURNOUT[k], spb.ls[[7]]$YES.sh[k], 
      pch=21, cex=sqrt(spb.ls[[7]]$BALLOTS.CAST)[k]/9, col="white", bg="black") 
    }else{
      points(spb.ls[[7]]$TURNOUT[k], spb.ls[[7]]$YES.sh[k], 
      pch=21, cex=sqrt(spb.ls[[7]]$BALLOTS.CAST)[k]/25, col="white", bg="black") 
      }
    }

grid()

points(
x=c(U775.TURNOUT.desired, U775.TURNOUT, U775.TURNOUT.20200701), 
y=c(U775.YES.desired, U775.YES, U775.YES.20200701), 
pch=21, cex=sqrt(c(1777, 1777, (459+332)))/25, col="white", bg="red")

arrows(
x0=c(U775.TURNOUT.desired, U775.TURNOUT -.01),
x1=c(U775.TURNOUT, U775.TURNOUT.20200701 + .01),
y0=c(U775.YES.desired - .02, U775.YES + .01),
y1=c(U775.YES + .02, U775.YES.20200701 - .01),
col="red",
angle=10
)

text(
x=c(U775.TURNOUT.desired, U775.TURNOUT, U775.TURNOUT.20200701), 
y=c(U775.YES.desired, U775.YES, U775.YES.20200701), 
labels=1:3,
pos=c(4,4,2))

dev.off()
