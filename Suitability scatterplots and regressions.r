library(biomod2)
library(ggplot2)
library(gridExtra)
require(SDMTools)
require(raster)
require(rasterVis)
require(lattice)
require(fBasics)
load("Eske_Americas.RData")

colX <- seqPalette(length(unique(ls(pattern = "Amer_forecast_suit_"))) + 6, "Oranges")
colX2 <- colX[c(7:(length(unique(ls(pattern = "Amer_forecast_suit_"))) + 6))]
colX2 <- adjustcolor(colX2, alpha.f = 0.6)

colY <- seqPalette(length(unique(ls(pattern = "Amer_forecast_suit_"))) + 6, "Greens")
colY2 <- paste(colY[c(7:(length(unique(ls(pattern = "Amer_forecast_suit_"))) + 6))], sep="")
colY2 <- adjustcolor(colY2, alpha.f = 0.6)

bins <- c(seq(12,21,1), seq(22,46,2))
dav <- as.data.frame(matrix(NA, nrow=length(bins), ncol=4))
names(dav) <- c("Time", "Average", "Median", "Maximum")
dav$Time <- bins
#st <- stack()
for (m in seq_along(ls(pattern="Amer_forecast_suit_")))
{
ma <- get(ls(pattern="Amer_forecast_suit_")[m])
#names(ma) <- paste0("Amer_", bins[m], "ka", sep="")
ma2 <- na.exclude(as.data.frame(ma, xy=TRUE))
av <- mean(ma2[,3])
med <- median(ma2[,3])
maxim <- max(ma2[,3])
dav[m,2] <- as.numeric(av)
dav[m,3] <- as.numeric(med)
dav[m,4] <- as.numeric(maxim)
}
trellis.device(device="pdf", file="Americas_plots.pdf", width=15, height=12)
par(mfrow=c(3,2), mar= c(3,3,3,3), oma=c(2,2,2,2))
plot(x=dav$Time, y=dav$Average, type="p", col=colY2[11], xlim=c(11,48), ylim=c(0,0.25), bg= colY2[11], xaxs="i", pch=21, cex=1.6, yaxs="i", axes=F, xlab=NA, ylab=NA)
mod2 <- data.frame(Time=dav$Time, Av=dav$Average)
time_lm2 <- mod2[!is.na(mod2$Av),"Time"]
hyp <- mod2[!is.na(mod2$Av),"Av"]
fit3 <- lm(hyp~time_lm2)
abline(fit3, col=colX2[11], lwd=1.8, lty=2)
ad2 <- round(summary(fit3)$adj.r.squared,3)
p.v2 <- round(summary(fit3)$coef[2,4],3)  
axis(side=2, col= "grey60", at=seq(0,0.25,0.025), lwd=1, col.ticks="grey60", cex.axis=1)
axis(side=1, col="grey60", at=c(seq(11,21,1), seq(22,48,2)), lwd=1, col.ticks="grey60", cex.axis=0.9)
text(x=13.7, y = 0.23, cex=0.8, bquote(italic(p.val)~ "=" ~. (paste0(p.v2, ifelse(p.v2 <0.05, "*", "")))), col="grey60")
#mtext(side = 1, line = 3, cex=1.0, "Time(ka)")
mtext(side = 2, line = 3, cex=1.0, "Average Suitability", col="grey60")

plot.new()

plot(x=dav$Time, y=dav$Median, type="p", col=adjustcolor("steelblue2", alpha.f = 0.6), xlim=c(11,48), ylim=c(0,0.25), bg= adjustcolor("steelblue2", alpha.f = 0.6)
, xaxs="i", pch=21, cex=1.6, yaxs="i", axes=F, xlab=NA, ylab=NA)
mod2 <- data.frame(Time=dav$Time, Me=dav$Median)
time_lm2 <- mod2[!is.na(mod2$Me),"Time"]
hyp <- mod2[!is.na(mod2$Me),"Me"]
fit3 <- lm(hyp~time_lm2)
abline(fit3, col=colX2[11], lwd=1.8, lty=2)
ad2 <- round(summary(fit3)$adj.r.squared,3)
p.v2 <- round(summary(fit3)$coef[2,4],3)  
axis(side=2, col= "grey60", at=seq(0,0.25,0.025), lwd=1, col.ticks="grey60", cex.axis=1)
axis(side=1, col="grey60", at=c(seq(11,21,1), seq(22,48,2)), lwd=1, col.ticks="grey60", cex.axis=0.9)
text(x=13.7, y = 0.23, cex=0.8, bquote(italic(p.val)~ "=" ~. (paste0(p.v2, ifelse(p.v2 <0.05, "*", "")))), col="grey60")
#mtext(side = 1, line = 3, cex=1.0, "Time(ka)")
mtext(side = 2, line = 3, cex=1.0, "Median Suitability", col="grey60")
plot.new()

plot(x=dav$Time, y=dav$Maximum, type="p", col=adjustcolor("firebrick3", alpha.f = 0.6), xlim=c(11,48), ylim=c(0,1), bg= adjustcolor("firebrick3", alpha.f = 0.6)
, xaxs="i", pch=21, cex=1.6, yaxs="i", axes=F, xlab=NA, ylab=NA)
mod2 <- data.frame(Time=dav$Time, Max=dav$Maximum)
time_lm2 <- mod2[!is.na(mod2$Max),"Time"]
hyp <- mod2[!is.na(mod2$Max),"Max"]
fit3 <- lm(hyp~time_lm2)
abline(fit3, col=colX2[11], lwd=1.8, lty=2)
ad2 <- round(summary(fit3)$adj.r.squared,3)
p.v2 <- round(summary(fit3)$coef[2,4],3)  
axis(side=2, col= "grey60", at=seq(0,1,0.1), lwd=1, col.ticks="grey60", cex.axis=1)
axis(side=1, col="grey60", at=c(seq(11,21,1), seq(22,48,2)), lwd=1, col.ticks="grey60", cex.axis=0.9)
text(x=13.7, y = 0.9, cex=0.8, bquote(italic(p.val)~ "=" ~. (paste0(p.v2, ifelse(p.v2 <0.05, "*", "")))), col="grey60")
mtext(side = 1, line = 3, cex=1.0, "Time(ka)")
mtext(side = 2, line = 3, cex=1.0, "Maximum Suitability", col="grey60")
dev.off()