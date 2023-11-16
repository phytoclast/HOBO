library(terra)
mons <- c('01','02','03','04','05','06','07','08','09','10','11','12')
i=1
path = 'C:/a/Ecological_Sites/GIS/Climate/PRISM2010'
for(i in 1:12){
  t0 <- rast(paste0(path, '/T/t',mons[i],'/w001001.adf'))
  p0 <- rast(paste0(path, '/P/p',mons[i],'/w001001.adf'))
  names(t0) <- paste0('t',mons[i])
  assign(paste0('t',mons[i]), t0)
  names(p0) <- paste0('p',mons[i])
  assign(paste0('p',mons[i]), p0)
  if(i==1){
    t00 = t0
    p0112 = p0
  }else{
    t00=t00+t0
    p0112=p0112+p0
  }
}
t0112 <- t00/12
names(p0112)<-'p0112'
names(t0112)<-'t0112'
writeRaster(p0112, 'C:/workspace/HOBO/data/PRISM/p0112.tif', overwrite=T)
writeRaster(t0112, 'C:/workspace/HOBO/data/PRISM/t0112.tif', overwrite=T)

plot(t07)