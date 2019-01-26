library(raster)
x <- hps <- nepal::pyuthan_health_facilities
y <- hps[hps@data$HF_TYPE %in%  c('Sub Health Post', 'Health Post') |
           (hps@data$VDC_NAME1 == 'Bijubar' &
              hps@data$HF_TYPE == 'Hospital') | 
           hps@data$VDC_NAME1 =='Bhingri PHC' ,]
y$hospital <- (y@data$VDC_NAME1 == 'Bijubar' &
                 y@data$HF_TYPE == 'Hospital') | 
  y@data$VDC_NAME1 =='Bhingri PHC'
y$health_post <- y$HF_TYPE == 'Health Post'
y$sub_health_post <- y$HF_TYPE == 'Sub Health Post'
y$hub <- y$hospital | y$VDC_NAME1 == 'Khawang'
y$type <- ifelse(y$hub, 'Hub',
                 ifelse(y$health_post, 'Health post',
                        ifelse(y$sub_health_post, 'Sub health post', NA)))
hf <- y