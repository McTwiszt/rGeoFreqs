#' @export
importMapData <- function(countries = c("UKR", "PL", "SK", "HU", "RO", "CZ"), levels = 2, localPath = "C:/Users/Admin/Documents/country_codes"){
  mapList <- c()
  countryList <- c()
  nameList <- c()
  for(c in countries){
    for(i in 0:levels){
      if(i == 0){}
      country_shp <- geodata::gadm(country = c, level = i, localPath)
      nam <- paste(c, "shp", i, sep = "_")
      #assign(nam, country_shp)
      mapList <- c(mapList, country_shp)
      nameList <- c(nameList, nam)
      names(mapList) <- nameList
    }
  }
  countryList<- c(countryList, mapList)
  return(countryList)
}

subsetMaps <- function(countryList){
  require(raster)
  mapLevelZero <- c(countryList[grepl( "0" , names(countryList))])
  for(i in mapLevelZero){
    x <- as(i, "Spatial")
    mapLevel0 <- x %>% rbind(x)
  }
  mapLevelOne<- c(countryList[grepl( "1" , names(countryList))])
  for(i in mapLevelOne){
    x <- as(i, "Spatial")
    mapLevel1 <- x %>% rbind(x)
  }
  mapLevelTwo <- c(countryList[grepl( "2" , names(countryList))])
  for(i in mapLevelTwo){
    x <- as(i, "Spatial")
    mapLevel2 <- x %>% rbind(x)
  }
  mapLevel0 <<-mapLevel0
  mapLevel1 <<-mapLevel1
  mapLevel2 <<-mapLevel2
  
}

#' @export
getMaps <- function(countries = c("UKR", "PL", "SK", "HU", "RO", "CZ"), levels = 2, localPath = "C:/Users/Admin/Documents/country_codes"){
  print("This may take a while ...")
  countryList <<- countryList <- importMapData(countries,levels, localPath)
  subsetMaps(countryList)
}

#' @export
getRegions <- function(df_rich, token, regionMap, stats = median, scale = F){
  e <- terra::extract(regionMap, df_rich[, c('long','lat')])
  df_rich$GID_2 <- e$GID_2 #  geographical ID level 2 = Region
  xx <- data.frame(df_rich$GID_2, df_rich[,token])
  
  xx <- xx %>%
    group_by(df_rich.GID_2) %>%
    summarise_at(vars(df_rich...token.), list(name = stats))
  if(scale == T){
    token_scaled <- gsub(" ","", paste(token, "_scaled"))
    xx3 <- data.frame(df_rich$GID_2, df_rich[,token_scaled])
    xx4 <- xx3 %>%
      group_by(df_rich.GID_2) %>%
      summarise_at(vars(df_rich...token_scaled.), list(name = stats))
    xx5<- cbind(xx,xx4$name)
    df_rich2 <- data.frame(xx5)
    colnames(df_rich2)[1] <- "GID_2"
    colnames(df_rich2)[2] <- "token_mean"
    colnames(df_rich2)[3] <- "z_score"
  }
  else{
    df_rich2 <- data.frame(xx)
    colnames(df_rich2)[1] <- "GID_2"
    colnames(df_rich2)[2] <- "token_mean"
  }
  return(df_rich2)
}

getOblast <- function(df_rich, token, regionMap, stats = median, scale = F){
  e <- terra::extract(regionMap, df_rich[, c('long','lat')])
  df_rich$GID_1 <- e$GID_1 #  geographical ID level 2 = Region
  xx <- data.frame(df_rich$GID_1, df_rich[,token])
  xx2 <- xx %>%
    group_by(df_rich.GID_1) %>%
    summarise_at(vars(df_rich...token.), list(name = stats))
  if(scale == T){
    token_scaled <- gsub(" ","", paste(token, "_scaled"))
    xx3 <- data.frame(df_rich$GID_1, df_rich[,token_scaled])
    xx4 <- xx3 %>%
      group_by(df_rich.GID_1) %>%
      summarise_at(vars(df_rich...token_scaled.), list(name = stats))
    xx5<- cbind(xx2,xx4$name)
    df_rich2 <- data.frame(xx5)
    colnames(df_rich2)[1] <- "GID_1"
    colnames(df_rich2)[2] <- "token_mean"
    colnames(df_rich2)[3] <- "z_score"
  }
  else{
    df_rich2 <- data.frame(xx2)
    colnames(df_rich2)[1] <- "GID_1"
    colnames(df_rich2)[2] <- "token_mean"
  }
  test <<- df_rich2
  return(df_rich2)
}


mergeRegionMap <- function(map, df){
  map_merged <-merge(map, df, by.x = "GID_2", by.y = "GID_2")
  map_merged <- map_merged[-1,] # optional! first row is weird in this case
  return(map_merged)
}

mergeOblastMap <- function(map, df){
  map_merged <-merge(map, df, by.x = "GID_1", by.y = "GID_1")
  map_merged <- map_merged[-1,] # optional! first row is weird in this case
  return(map_merged)
}


#' @export
plotFreqMap <- function(type = "c", size = 1, styloFreqList, token = "а", stats = median, scale = F, metadata = metadata_df, map_level = 2,  title = "Title", fill = "Mean relative frequency: token", xlim = c(18, 25), ylim = c(47, 51), regex = F, tokenInWords = ""){
  if(map_level == 1){
    map = mapLevel1
    if(regex == T){
      stylo_freqs <<- df <- getTokenFreqsRegex(type, size, token, tokenInWords, scale)
      df_rich <<- x <- addMeta(df, metadata, 1)
      df_rich2 <- prepareMetaData(x)
      df_rich3 <- df_rich2[!is.na(df_rich2$Age),]
      df_rich3 <- df_rich3[!is.na(df_rich3$Gender),]
      df_rich4 <- transformGeoCoords(df_rich3)
      if(tokenInWords == ""){
        df_rich5 <- getOblast(df_rich4, token, map, stats, scale)
      }
      else{
        df_rich5 <- getOblast(df_rich4, tokenInWords, map, stats, scale)
      }
      map_df_merged <- mergeOblastMap(map, df_rich5)
    }
    else{
      stylo_freqs <<- df <- getTokenFreqs(type, size, token, scale)
      df_rich <<- x <- addMeta(df, metadata, 1)
      df_rich2 <- prepareMetaData(x)
      df_rich3 <- df_rich2[!is.na(df_rich2$Age),]
      df_rich3 <- df_rich3[!is.na(df_rich3$Gender),]
      df_rich4 <- transformGeoCoords(df_rich3)
      df_rich5 <- getOblast(df_rich4, token, map, stats, scale)
      map_df_merged <- mergeOblastMap(map, df_rich5)
    }
    
  }
  else{
    map = mapLevel2
    if(regex == T){
      stylo_freqs <<- df <- getTokenFreqsRegex(type, size, token, tokenInWords, scale)
      df_rich <<- x <- addMeta(df, metadata, 1)
      df_rich2 <- prepareMetaData(x)
      df_rich3 <- df_rich2[!is.na(df_rich2$Age),]
      df_rich3 <- df_rich3[!is.na(df_rich3$Gender),]
      df_rich4 <- transformGeoCoords(df_rich3)
      if(tokenInWords == ""){
        df_rich5 <- getOblast(df_rich4, token, map, stats, scale)
      }
      else{
        df_rich5 <- getOblast(df_rich4, tokenInWords, map, stats, scale)
      }
      map_df_merged <- mergeRegionMap(map, df_rich5)
    }
    else{
      stylo_freqs <<- df <- getTokenFreqs(type, size, token, scale)
      df_rich <<- x <- addMeta(df, metadata, 1)
      df_rich2 <- prepareMetaData(x)
      df_rich3 <- df_rich2[!is.na(df_rich2$Age),]
      df_rich3 <- df_rich3[!is.na(df_rich3$Gender),]
      df_rich4 <- transformGeoCoords(df_rich3)
      df_rich5 <- getRegions(df_rich4, token, map, stats, scale)
      map_df_merged <- mergeRegionMap(map, df_rich5)
    }
  }
  map_df_merged@data$id <- rownames(map_df_merged@data)
  map_df_merged_fortified <- fortify(map_df_merged, region = "id")
  map_data_combined <- merge(map_df_merged_fortified, map_df_merged@data,
                             by = "id")
  if(scale == F){
    token_map_borders <- ggplot2::ggplot()+ geom_polygon(data = map_data_combined, ggplot2::aes(x = long, y = lat, group = group , fill = token_mean), color = "black", size = 0.25) + 
      ggplot2::coord_map(xlim = xlim, ylim = ylim) + ggplot2::borders(mapLevel0, colour = "red", size = 0.5) + #+ borders(eu0_shp, colour = "blue")
      labs(title = title,
           x = "Latitude",
           y = "Longitude",
           fill = fill)
  }
  else{
    token_map_borders <- ggplot2::ggplot()+ geom_polygon(data = map_data_combined, ggplot2::aes(x = long, y = lat, group = group , fill = z_score), color = "black", size = 0.25) + 
      ggplot2::coord_map(xlim = xlim, ylim = ylim) + ggplot2::borders(mapLevel0, colour = "red", size = 0.5) + #+ borders(eu0_shp, colour = "blue")
      labs(title = title,
           x = "Latitude",
           y = "Longitude",
           fill = fill)
    
  }
  
  print(token_map_borders)
  
}
