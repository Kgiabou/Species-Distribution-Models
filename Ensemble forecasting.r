library(biomod2)
library(ggplot2)
library(gridExtra)
require(SDMTools)
require(raster)
require(rasterVis)

## Subset Asian climates to Intervals ###

bins <-c(seq(11,22,1), seq(24,46,2))
par(mfrow=c(3,1), mar=c(2,2,2,2), oma=c(2,2,2,2))
for(i in seq_along(bins))
{
  Climate <- read.delim(dir(pattern = paste("clim_", bins[i], "kyr", sep = "")),h=T,sep="\t", stringsAsFactors = FALSE, quote="")
  colnames(Climate) <- c("Latitude", "Longitude", "Prec_Sp", "Prec_Su", "Prec_Au", "Prec_Wi", "Temp_Sp", "Temp_Su", "Temp_Au", "Temp_Wi" )
  Interval<- bins[i]
  Climate2 <- Climate[which(Climate$Longitude < -168 | Climate$Longitude >= 60),]
  ### Convert climatic dataframe to raster #####
  Climate3 <- na.exclude(Climate2)
  Climatexyz <- Climate3[,c(2,1,3)]; names(Climatexyz) <- names(Climatexyz[,c(1,2,3)])
  clim_rasts <- rasterFromXYZ(Climatexyz)
  plot(clim_rasts, main=paste0(bins[i],"kyr_",names(clim_rasts)[[1]], sep=""), cex.main = 0.9, xlim=c(-180,180), ylim=c(30,90))
  for(r in c(4:10))
  {
    clim_xyz <- Climate3[,c(2,1,r)]
    clim_nrast <- rasterFromXYZ(clim_xyz)
    clim_rasts <- stack(clim_rasts, clim_nrast)
    crs(clim_rasts) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" ## Set coordinate system
  }
  assign(paste("clim_", Interval,"k", sep=""), clim_rasts)
}

## Change map projection ##
crs (ras_med) <- "+proj=aeqd +lat_0=90 +lon_0=0+x_0=0+y_0=0"

n_var <- 3 ### specify numebr of variables
vec <- as.vector(NULL)
vec2 <- as.vector(NULL)
	for (bi in seq_along(bins3))
	{ 
	  occ_per_bin <- subset(humans_final3, Interval==bins3[bi])	 ## create a subset data.frame for each time bin ###
	  vec <- c(vec, nrow(occ_per_bin)) ### Construct a vector with the number of Occurrences for each time interval
	  vec2 <- c(vec2, nrow(unique(occ_per_bin[,c("cell_Longitude", "cell_Latitude")])))
	}
human_occurences <- data.frame("Time"= paste(bins3,"k", sep=""), "All"=vec, "Unique"=vec2)
list_len <- length(vec[vec >= 2*n_var]) 
sdm_bins <- bins3[vec >= 2*n_var]
used_occ <- vec[vec >= 2*n_var]

### Subset climates ###
require(biomod2)
myBiomodOption <- Print_Default_ModelingOptions()
myBiomodOption@MAXENT.Phillips$path_to_maxent.jar = paste("C:/Users/kgiab/Desktop/SDM");
myBiomodOption@MAXENT.Phillips$memory_allocated = 1024; #Allocates 2048 MB/2 GB of memory to modeling
myBiomodOption@MAXENT.Phillips$maximumiterations = 1000


for (cl in seq_along(sdm_bins))
{
## Get climatic variables per time bin
clim <- get(ls(pattern= paste0("clim_", bins3[cl], "k", sep=""))) 
## Make a raster stack witht the relevant variables##
clim_sub <- stack(subset(clim, c("Prec_Au", "Temp_Su", "Temp_Au")))
## Subset occurrecnes per time interval ##
Homosap_occ <- subset(humans_final3, Interval==sdm_bins[cl])
### Format data, create presence-absence dataset ##
Hsap_data <- BIOMOD_FormatingData(resp.var = rep(1, nrow(Homosap_occ)), expl.var = clim_sub,
resp.xy = Homosap_occ[, c("cell_Longitude", "cell_Latitude")], resp.name = "Homo_Sapiens", PA.nb.rep = 5, PA.nb.absences = 300, PA.strategy = "random")
assign(paste0("H_sapiens_biomod_data_", sdm_bins[cl], "k", sep=""), Hsap_data)
plot(Hsap_data)
## Choose Modelling Algorithms, Training dataset ##
Hsap_models <- BIOMOD_Modeling(data = Hsap_data, models = c("MAXENT.Phillips", "GLM", "GAM", "GBM", "ANN", "MARS", "RF"), models.options = myBiomodOption, 
NbRunEval = 5, DataSplit = 80, VarImport = 3, models.eval.meth = c("TSS", "ROC"), do.full.models = T, modeling.id = paste0("Hsap_", sdm_bins[cl], "k", sep=""))

assign(paste0("H_sapiens_biomod_models_", sdm_bins[cl], "k", sep=""), Hsap_models)

Hsap_models_scores <- get_evaluations(Hsap_models, as.data.frame=T)

models_scores_graph(Hsap_models, by = "models" , metrics = c("ROC","TSS"), xlim = c(0,1), ylim = c(0.5,1), main=paste0("Model_scores_", sdm_bins[cl], "k", sep=""))

models_scores_graph(Hsap_models, by="cv_run" , metrics = c("ROC","TSS"), xlim = c(0,1), ylim = c(0.5,1), main=paste0("CV_run_", sdm_bins[cl], "k", sep=""))

models_scores_graph(Hsap_models, by="data_set" , metrics = c("ROC","TSS"), xlim = c(0,1), ylim = c(0.5,1), main=paste0("PA_Dataset_", sdm_bins[cl], "k", sep=""))

Hsap_models_var_import <- get_variables_importance(Hsap_models,as.data.frame=T)
var.imp <-apply(Hsap_models_var_import, c(1,2), mean)

assign(paste0("H_sapiens_var_import_", sdm_bins[cl], "k", sep=""), var.imp)
par(mfrow=c(1,1))
## Plot variable importance ##
barplot(var.imp, beside=TRUE, legend.text=TRUE, col=c("grey90", "yellow4", "green4"), ylim=c(0,1), main=paste0("Variable Importance ", sdm_bins[cl], "k", sep=""))
## Ensemble forecasting of best model by TSS ##
Hsap_ensemble_models <- BIOMOD_EnsembleModeling(modeling.output = Hsap_models, em.by = "all", eval.metric = "TSS", eval.metric.quality.threshold = 0.8,
models.eval.meth = c("TSS", "ROC"), prob.mean = FALSE, prob.cv = TRUE, committee.averaging = FALSE, prob.mean.weight = TRUE, VarImport = 0) ### Need to redo that based on ROC scores

assign(paste0("H_sapiens_biomod_ensemble_", sdm_bins[cl], "k", sep=""), Hsap_ensemble_models)

(Hsap_ensemble_models_scores <- get_evaluations(Hsap_ensemble_models))

Hsap_models_proj <- BIOMOD_Projection(modeling.output = Hsap_moalgorithmtechnique projections 

Hsap_ensemble_models_proj <- BIOMOD_EnsembleForecasting(EM.output = Hsap_ensemble_models, projection.output = Hsap_models_proj,
binary.meth = "TSS", output.format = ".img", do.stack = FALSE)

Hsap_ensemble_forecast <- get_predictions(Hsap_ensemble_models_proj)
assign(paste0("H_sapiens_final_forecast_", sdm_bins[cl], "k", sep=""), Hsap_ensemble_models)
Hsap_WMean_ensemble_forecast <- subset(Hsap_ensemble_forecast, grep("EMwmean", names(Hsap_ensemble_forecast)))
names(Hsap_WMean_ensemble_forecast) <- c("H.sapiens_WMean")
Hsap_WMean_ensemble_forecast_suit <- Hsap_WMean_ensemble_forecast/1000 
assign(paste0("H_sapiens_forecast_suit_", sdm_bins[cl], "k", sep=""), Hsap_WMean_ensemble_forecast_suit)
}

