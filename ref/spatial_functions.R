## Fuctions writen by Nishant Kishore 
impute_hh_gps <- function(households){
  #impute GPS
  x <- households[which(is.na(households$lat)),]
  
  for(i in 1:nrow(x)){
    
    a <- households %>%
      subset(as.Date(completed_time) == as.Date(x$completed_time[i])) %>%
      subset(!is.na(lat)) %>%
      subset(username == x$username[i]) %>%
      {.[c("long","lat")]}
    
    if(nrow(a) > 0){
      households[which(households$formid == x$formid[i]),c("long","lat")] <-
        a %>% sample_n(1)
    }
    households[which(households$formid == x$formid[i]),c("long","lat")] <-
      households[which(as.Date(households$completed_time) == as.Date(x$completed_time[i]) &
                         !is.na(households$lat)),c("long","lat")] %>% sample_n(1)
  }
  return(households)
}

over_near <- function(households, sel){
  require("rgdal")
  require("sp")
  require("raster")
  require("geosphere")
  
  hh_sp <-SpatialPointsDataFrame(households[,c("long","lat")], 
                                 households, 
                                 proj4string = crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0 ")) %>% 
    spTransform(crs(sel))
  
  hh_sp2 <- over(hh_sp, sel)
  hh_sp@data$id <- hh_sp2$id
  hh_sp@data$strata <- hh_sp2$strata
  
  #clean points that are just a little outside polygons
  
  no_fit <- hh_sp[is.na(hh_sp$strata),]
  
  for(i in 1:nrow(no_fit)){
    
    a <- hh_sp[which(hh_sp$username == no_fit[i,]$username & 
                       as.Date(hh_sp$completed_time)==as.Date(no_fit[i,]$completed_time)),] %>% 
      as.data.frame() %>%
      {unique(as.numeric(as.character(.[!is.na(.$id),"id"])))}
    
    b <- lapply(a, function(x) dist2Line(spTransform(no_fit[i,], 
                                                     CRS("+init=epsg:4326")), 
                                         spTransform(barrio_frame[which(barrio_frame$id == x),], 
                                                     CRS("+init=epsg:4326")))) %>%
      unlist() %>% matrix(ncol = 4, byrow = T) %>% as.data.frame() %>% 
      set_names(c("dist", "long", "lat", "id")) %>% subset(dist == min(dist)) %>% 
      row.names() %>% as.numeric()
    
    hh_sp[which(hh_sp$hh_id == no_fit[i,]$hh_id),"id"] <- a[b]
  }
  
  return(hh_sp)
  
}

create_weights <- function(barrio_frame, households){
  
  households <- households %>% subset(consent == 1)
  
  strata_weights <- barrio_frame %>%
    aggregate(.$pop17~strata, data = ., FUN = sum) %>% 
    set_names(c("strata","strata_pop17")) %>%
    mutate(strata_w  = sum(strata_pop17)/strata_pop17)
  
  barrio_weights <- barrio_frame %>%
    aggregate(.$pop17~id, data = ., FUN = sum) %>% set_names(c("id", "pop17")) %>%
    merge(barrio_frame[,c("id","strata")], by = "id") %>%
    merge(strata_weights, by = "strata") %>%
    mutate(barrio_w = strata_pop17 / pop17)
  
  hh_weights <- table(households$id) %>% as.data.frame() %>% 
    set_names(c("id", "samp_hh")) %>%
    merge(barrio_weights, by = "id", all = T) %>%
    mutate(hh_w = (pop17/3)/samp_hh)
  
  return(hh_weights)
  
}
