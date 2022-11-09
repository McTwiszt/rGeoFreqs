#' @export
importMetaXml<- function(metapath){
  metadata_df <<- metadata_df <- XML::xmlToDataFrame(metapath)
print("Info: Metadata imported as metadata_df.")
print("Info:  Please check whether the metadata file contains the columns ´speaker´,´longitude´ and ´latitude´.")
return(metadata_df)
}

#' @export
addMeta <- function(stylodf, metadata_df, idIndex = 1) {
  id_list <- c()
  colNum <- as.numeric(ncol(stylodf))
  metaNameVector <-  colnames(metadata_df[-idIndex])
  stylodf[,metaNameVector] <- NA
  for(i in 1:length(metadata_df$ID)){
    if(grepl(toString(metadata_df$ID[i]),  toString(stylodf$Speaker)) && toString(metadata_df$ID[i]) != "NN"){
      mr <- as.numeric(mapply(regexpr, toString(metadata_df$ID[i]), stylodf$Speaker))
      names(mr) <- NULL
      matchRow <- which(mr > 0)
      stylodf[matchRow,] <- cbind(stylodf[0:colNum][matchRow,], c(metadata_df[metaNameVector][i,]))
      id_list <- c(id_list, matchRow)
      i <- i +1
    }
    else 
      i <- i + 1
  }
  return(stylodf)
}

dms2dec <- function(dms, separators = c("º", "°", "\'", "’", "’’", "\"", "\'\'", "\\?")) {
  dms <- as.character(dms)
  dms <- gsub(pattern = " ", replacement = "", x = dms)
  for (s in separators) dms <- gsub(pattern = s, replacement = "_splitHere_", x = dms)
  
  splits <- strsplit(dms, split = "_splitHere_")
  n <- length(dms)
  deg <- min <- sec <- hem <- vector("character", n)
  
  for (i in 1:n) {
    deg[i] <- splits[[i]][1]
    min[i] <- splits[[i]][2]
    
    if (length(splits[[i]]) < 4) {
      hem[i] <- splits[[i]][3]
    } else {
      sec[i] <- splits[[i]][3]
      hem[i] <- splits[[i]][4]
    }
  }
  
  dec <- colSums(rbind(as.numeric(deg), (as.numeric(min) / 60), (as.numeric(sec) / 3600)), na.rm = TRUE)
  sign <- ifelse (hem %in% c("N", "E"), 1, -1)
  hem_miss <- which(is.na(hem))
  if (length(hem_miss) > 0) {
    warning("Hemisphere not specified at position(s) ", hem_miss, ", so the sign of the resulting coordinates may be wrong.")
  }
  dec <- sign * dec
  return(dec)
}

transformGeoCoords <- function(df){
  df = df[!df$Latitude == "NN",]
  df = df[!is.na(df$Latitude),]
  df$lat<- dms2dec(df$Latitude) %>% abs()
  df$long<- dms2dec(df$Longitude) %>% abs()
  #df$Latitude <- NULL
  #df$Longitude<- NULL
  return(df)
}

#' @export
prepareMetaData <- function(df_rich, geo = T){
  df_rich[df_rich == "NN"] <- NA
  df_rich$Variety<- factor(df_rich$var)
  df_rich$Gender<- factor(df_rich$Gender)
  df_rich$Year_birth <- df_rich$"Year-of-birth"
  df_rich$Age <- 2019-as.numeric(df_rich$Year_birth)
  df_rich$Livingplace <- factor(df_rich$`Living-place`)
  if(geo ==T){ 
    df_rich2 <- transformGeoCoords(df_rich)
  }
  else{
    df_rich2 <- df_rich
  }
  return(df_rich2)
}