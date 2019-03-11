library(ade4)
library(factoextra)

### PCA to assess the climatic variables correlation ###

humans_final3 <- read.delim("Humans_sdm_cor.txt", h=T,sep = "\t", stringsAsFactors = FALSE, quote = "") 
points_humans <- humans_final3[,c("cell_Longitude", "cell_Latitude", "Interval")]
bins3 <- as.vector(unique(humans_final3$Interval))
for (i in seq_along(bins3)) ### Prec_Au, Temp_Su, Temp_Au ###
{
 points_h <- subset(points_humans, Interval==bins3[i])
 points_h <- points_h[,c(1,2)]
 cl_ras <- get(ls(pattern = paste("clim_", bins3[i], "k", sep="")))
 hum_id <- cellFromXY(subset(cl_ras,1), points_h)
 cl_df <- na.omit(as.data.frame(cl_ras))
 pca_cl <- dudi.pca(cl_df, scannf = F, nf = 5) ### pca on all variables for contirbution
 fviz_pca_var(pca_cl, col.var = "contrib", gradient.cols=c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE) ### plotting variable loadings
 res.var <- get_pca_var(pca_cl) ### results of pca
 s.class(pca_cl$li[, 1:2], fac= factor(rownames(cl_df)%in% hum_id, levels = c("FALSE", "TRUE" ),labels = c("background", "Homo_Sapiens")), col=c("red", "blue"),
 csta = 0, cellipse = 2, cpoint = .6, pch = 19, sub=paste0("Asia_", bins3[i], "ka", sep=""))
}