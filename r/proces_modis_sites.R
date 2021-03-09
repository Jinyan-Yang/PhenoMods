gcc.met.modis.df <- readRDS('cache/modis/modis.ndvi.met.rds')
limits.vec <- as.vector(quantile(gcc.met.modis.df$GCC,na.rm=T,probs = c(.01,.99)))
gcc.met.modis.df <- gcc.met.modis.df[gcc.met.modis.df$Species != 'tussock<1100',]

gcc.met.modis.df$Species <- gsub('<','_',gcc.met.modis.df$Species)
modis.sites.vec <- unique(gcc.met.modis.df$Species)
modis.sites.vec <- modis.sites.vec[c(4:9,14:19)]