# read int he gcc and met file
gcc.met.modis.df <- readRDS('cache/modis/modis.ndvi.met.rds')
# gcc.met.modis.df <- gcc.met.modis.df[gcc.met.modis.df$Species != 'tussock<1100',]

# make specie name as rainfall and vege type
gcc.met.modis.df$Species <- gsub('<','_',gcc.met.modis.df$Species)
modis.sites.vec <- unique(gcc.met.modis.df$Species)

# get the bare ground and full cover  from ndvi
# max is the 99% of all data
max.ndvi <- unname(quantile(gcc.met.modis.df$GCC,na.rm=T,probs = c(.99)))
# make a list of all site
limit.ls <- list()
for (i in seq_along(modis.sites.vec)){
  
  tmp.df <- gcc.met.modis.df[gcc.met.modis.df$Species == modis.sites.vec[i],]
  # get the min as 1% of each site;
  # this is because soil sypte may change ndvi
  # following Choler 2010
  limit.ls[[i]] <- c(unname(quantile(tmp.df$GCC,na.rm=T,probs = c(.01))),max.ndvi)
}
