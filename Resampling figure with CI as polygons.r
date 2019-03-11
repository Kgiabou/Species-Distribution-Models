polylims = function(xdata, ydata, ydata2) {
  # A function to find the beginings and endings of each run of real numbers
  # in a set of data, in order to create polygons for plotting. The assumption
  # is that ydata, ydata2, and xdata vectors are the same length, and that 
  # the ydata and ydata2 vectors contain NA values for missing data. The 
  # same values in ydata and ydata2 must be missing. The output will be 
  # a list of data frames of x and y values suitable for plotting polygons.
  
  # Use rle function to find contiguous real numbers
  rl = rle(is.na(ydata))
  starts = vector()
  ends = vector()
  indx = 1
  for (i in 1:length(rl$lengths)){
    if (rl$values[i]){
      # Value was NA, advance index without saving the numeric values
      indx = indx + rl$lengths[i]
    } else {
      # Value was a real number, extract and save those values
      starts = c(starts,indx)
      ends = c(ends, (indx + rl$lengths[i] - 1))
      indx = indx + rl$lengths[i]
    }	
  }
  
  # At this point the lengths of the vectors 'starts' and 'ends' should be
  # equal, and each pair of values represents the starting and ending indices
  # of a continuous set of data in the ydata vector.
  
  # Next separate out each set of continuous ydata, and the associated xdata,
  # and format them for plotting as a polygon.
  polylist = list()
  for (i in 1:length(starts)){
    temp = data.frame(x = c(xdata[starts[i]],xdata[starts[i]:ends[i]],
                            rev(xdata[starts[i]:ends[i]])),
                      y = c(ydata[starts[i]],ydata2[starts[i]:ends[i]],
                            rev(ydata[starts[i]:ends[i]])))
    polylist[[i]] = temp	
  }
  polylist
  # You can iterate through the items in polylist and plot them as 
  # polygons on your plot. Use code similar to the following:
  #	for (i in 1:length(polylist)){
  #			polygon(polylist[[i]]$x, polylist[[i]]$y, 
  #				col = rgb(0.5,0.5,0.5,0.5), border = NA)
  #	}
}

require(lattice)
library(fBasics)
library(grDevices)
trellis.device(device="pdf", file="Fig2.pdf", width=12, height=12)

cols <- adjustcolor("grey90", alpha.f = 0.7)
sams <- c(6,10,15,20)
bins=c(seq(8,21,1), seq(22,48,2))

nam <- as.vector(NULL)
for(ss in seq_along(dir(pattern = "6_occurrences_volumes.txt")[-c(7,11,14)]))
{
  n <- strsplit(dir(pattern = "6_occurrences_volumes.txt")[-c(7,11,14)][ss], split="_6")[[1]][1]
  nam <- c(nam, n)
}

species_list <- nam 
par(mfrow=c(4,4), mar=c(4,4,4,4), oma=c(3,3,3,3))
for (sp in seq_along(species_list))
{
  if(sp == 8)
  {
    bins <- c(seq(8,21,1), seq(22,48,2))
  }
  else
  {
    bins <-c(seq(13,21,1), seq(22,48,2))
  }
  for(j in seq_along(sams))
  {
    setwd("C:/Users/kgiab/Desktop/Project 2 PhD/2nd Chpapter ANALYSES/Human localities again/BENS SUGGESTION HYPERVOLUME/Resampling")
    spec_vol <- read.delim(paste(species_list[sp],"_", sams[j],"_occurrences_volumes.txt", sep=""), h=T, sep="\t")
    timeX <- as.vector(NULL)
    for(tt in seq_along(1:ncol(spec_vol)))
    {
      timeX <- c(timeX, as.numeric(strsplit(strsplit(colnames(spec_vol), split="_")[[tt]][3], split="k")))
    }
    colnames(spec_vol) <- timeX
    spec_vol2 <- data.frame(matrix(NA, nrow=100, ncol=length(bins)))
    colnames(spec_vol2) <-as.numeric(bins)
    vec <- data.frame(matrix(NA, nrow=1, ncol=length(bins)))
    colnames(vec) <-as.numeric(bins)
    vec2 <- data.frame(matrix(NA, nrow=1, ncol=length(bins)))
    colnames(vec2) <-as.numeric(bins)
    setwd("C:/Users/kgiab/Desktop/Project 2 PhD/2nd Chpapter ANALYSES/Human localities again/BENS SUGGESTION HYPERVOLUME")
    origin <- read.delim(paste(species_list[sp], "_volumes.txt", sep=""), h=T, sep="\t")
    Occs <- read.delim(dir(pattern=paste(species_list[sp], "_Occurrences.txt", sep="")), h=T, sep="\t")
    spec_met <- data.frame(Time=bins)
    for(co in seq_along(colnames(spec_vol)))
    {
      for (bi in seq_along(bins))
      {
        if(bins[bi] == as.numeric(colnames(spec_vol)[co]))
        {
          spec_vol2[,bi] <- spec_vol[,co]
          med <- median(spec_vol[,co])
          vec[,bi] <- med 
          mea <- mean(spec_vol[, co])
          vec2[,bi] <- mea 
          spec_met[bi,2] <- Occs[co,2]
          spec_met[bi,3] <- origin[co,1]
          qu <- quantile(spec_vol[,co], na.rm=TRUE, names=TRUE)
          spec_met[bi,4] <- qu[2]
          spec_met[bi,5] <- qu[3]
          spec_met[bi,6] <- qu[4]
        }
      }
    }
    colnames(spec_met) <- c("Time", "Occurrences", "Volume", "lower", "med", "upper")
    assign(paste0("spec_met_", sams[j], sep=""), spec_met)
  }
  min_vol <- subset(spec_met, Volume==min(na.omit(spec_met$Volume)))
  max_vol <- subset(spec_met, Volume==max(na.omit(spec_met$Volume)))
  row_min <- as.numeric(rownames(subset(spec_met, Volume==min(na.omit(spec_met$Volume)))))
  row_max <- as.numeric(rownames(subset(spec_met, Volume==max(na.omit(spec_met$Volume)))))
  bb <- barplot(spec_met_6$Occurrences, plot=FALSE,  space=0, border="grey45", axes=F, ylab=NA, xlab=NA, xaxs="i", yaxs="i")
  barplot(spec_met_6$Occurrences, col=adjustcolor("dark red", alpha.f = 0.5),  space=0, border="grey45", xlim=range(bb), ylim=c(0,700), axes=F, ylab=NA, xlab=NA, xaxs="i", yaxs="i")
  axis(1, at=bb, labels=as.numeric(c(spec_met_6$Time)), cex.axis = 0.9)
  if(sp == 4 || sp == 8 || sp == 12)	
  {
    axis(side=4, col="dark red", at=seq(0,700,50), labels=TRUE, lwd=1,col.ticks="dark red", cex.axis=0.9)
  }
  abline(h=sams[j],  col="black", lwd=1.3, lty=2)
  par(new=T)
  plot(bb, spec_met_6$Volume, type="n", pch=21, col="black", xlim=range(bb), ylim = c(0,70),xlab = NA, xaxs="i", yaxs="i", ylab = NA, las = 1, axes=F, cex.main = 0.7)
  polys_6 <- polylims(bb, spec_met_6$upper, spec_met_6$lower)
  for(i in 1:length(polys_6)){
    polygon(polys_6[[i]]$x, polys_6[[i]]$y, col = cols, border = NA)
  }
  polys_10 <- polylims(bb, spec_met_10$upper, spec_met_10$lower)
  for(i in 1:length(polys_10)){
  polygon(polys_10[[i]]$x, polys_10[[i]]$y, col = "grey75", border = NA)
  }
  polys_15<- polylims(bb, spec_met_15$upper, spec_met_15$lower)
  for(i in 1:length(polys_15)){
  polygon(polys_15[[i]]$x, polys_15[[i]]$y, col = "grey65", border = NA)
  }
  polys_20<- polylims(bb, spec_met_20$upper, spec_met_20$lower)
  for(i in 1:length(polys_20)){
  polygon(polys_20[[i]]$x, polys_20[[i]]$y, col = "grey50", border = NA)
  }
  if(sp == 1 || sp == 5 || sp == 9 || sp == 13)	
  {
    axis(2, at=seq(0,70,7), col="black", col.ticks="black", labels=c(seq(0,70, 7)), cex.axis = 0.9)
  }
  par(new=T)
  plot(bb, spec_met_6$Volume, type="b", col="black",pch=21, bg="black", xlim=range(bb), ylim = c(0,70),xlab = NA, xaxs="i", yaxs="i", ylab = NA, las = 1, axes=F, main=species_list[sp], cex.main = 0.7)
  legend(x= bb[3], y = 50, legend=c("original_values", "6_occ", "10_occ", "15_occ", "20_occ"), col=c("purple", "cornsilk3","grey48", "goldenrod", "coral"), lwd=1.3, lty=1, fill=NULL, cex=0.6, bty="n", xjust=0)
  abline(v = bb[17], col="steelblue", lwd=1.3, lty=2)
  abline(v = bb[row_min], col="steelblue", lwd=1.2, lty=2)
  abline(v = bb[row_max], col="red", lwd=1.2, lty=2)
  mod <- data.frame(plot_x=bb, vol=spec_met$Volume)
  time_lm2 <- mod[!is.na(mod$vol),"plot_x"]
  vol2 <- mod[!is.na(mod$vol),"vol"]
  fit2 <- lm(vol2~time_lm2)
  abline(fit2, col="grey40", lwd=1.2)
  ad <- round(summary(fit2)$adj.r.squared,3)
  p.v <- round(summary(fit2)$coef[2,4],3)
  text(x=min(bb)+ 6, y = 56, cex=0.85, bquote(italic(r.sq)~ "=" ~ .(ad)), col="grey30")
  text(x=min(bb)+ 6, y = 50, cex=0.85, bquote(italic(p.val)~ "=" ~. (paste0(p.v, ifelse(p.v <0.05, "*", "")))), col="grey30")
  if(sp == 13)	
  {
    mtext(side = 1, line = 2.6, cex=0.9, "Time(ka)")
  }
  if(sp == 5)
  {
    mtext(side = 2, line = 2.6, cex=0.9, "Niche Hypervolume(Std³)")
  }
  if(sp == 8)
  {
    mtext(side = 4, line = 2.6, cex=0.9, "No Occurrences")
  }
}
