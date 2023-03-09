#' Get Stylo Frequency List Function
#' This function performs a stylometric analysis of your corpus and creates a file called "table_with_frequencies"
#' in your working directory. The file is imported as dataframe. Please name the output according to specified settings
#' e.g. styloFreqList_c_1. 
#' @param type Do you want to create word ("w") or chacacter ("c") n-grams?
#' @param size Indicate the size of the n-grams here.
#' @param path Indicate the path of your folder containing the corpus folder.
#' @param size Indicate the name of the corpus folder. Default is "corpus". 
#' @export
#' @examples
#' getFreqTable()
#' 
getFreqTable <- function(type = "w", size = 1, path, corpusfoldername = "corpus",  mfw.min = 100, mfw.max = 500, mfw.incr = 100, culling.min = 20, culling.max = 70, culling.incr = 20, mfw.list.cutoff = 5000, distance.measure = "eder",  output = freq_list){
  # This function creates a stylo frequency list based on predefined settings (type = word or character, size = n-gram size, most frequent words, culling and distance measure.
  # The created frequencylist will automatically be 
  setwd(path)
  corpuspath <-gsub(" ","", paste(path, "/", corpusfoldername))
  stylo::stylo(corpus.dir = {{corpuspath}},
        corpus.format = "plain",
        corpus.lang = "Other",
        analyzed.features = {{type}}, # words = w, characters = c
        ngram.size = {{size}}, # size 
        preserve.case = FALSE,
        encoding = "UTF-8",
        mfw.min = {{mfw.min}},
        mfw.max =  {{mfw.max}},
        mfw.incr = {{mfw.incr}},
        start.at = 1,
        culling.min = {{culling.min}},
        culling.max = {{culling.max}},
        culling.incr = {{culling.incr}},
        mfw.list.cutoff = {{mfw.list.cutoff}},
        delete.pronouns = FALSE,
        use.existing.freq.tables = FALSE,
        use.existing.wordlist = FALSE,
        use.custom.list.of.files = FALSE,
        analysis.type = "CA",
        consensus.strength = 0.5,
        distance.measure = {{distance.measure}},
        sampling = "no.sampling",
        sample.size = 10000,
        number.of.samples = 1,
        display.on.screen = FALSE,
        write.pdf.file = TRUE,
        write.jpg.file = FALSE,
        write.svg.file = TRUE,
        write.png.file = FALSE,
        plot.custom.height = 7,
        plot.custom.width = 7,
        plot.font.size = 1,
        plot.line.thickness = 1,
        text.id.on.graphs = "labels",
        colors.on.graphs = "colors",
        titles.on.graphs = TRUE,
        label.offset = 0,
        add.to.margins = 2,
        dendrogram.layout.horizontal = TRUE,
        pca.visual.flavour = "classic",
        save.distance.tables = FALSE,
        save.analyzed.features = FALSE,
        save.analyzed.freqs = TRUE,
        dump.samples = FALSE,
        gui = FALSE
  )
  freqs<- read.table(gsub(" ","", paste(path, "/table_with_frequencies.txt")), encoding = "UTF-8")
  set_freqs <- freqs  %>% t() %>% as.data.frame() # transform DF
  set_freqs   <- tibble::rownames_to_column(set_freqs  , "Speaker") # rename column with texts
  return(set_freqs)
}

#' @export
getNscAccuracyPlot <- function (type = "w", size = 1, mfw_from = 10, mfw_to = 1000, 
                                 mfw_by = 10, culling = 0) {
  
  name <- gsub(" ", "", paste("styloFreqList", "_", type, "_", 
                              size))
  freqlist2 <- eval(as.symbol(name))
  freqlist <- freqlist2[, -1]
  rownames(freqlist) <- freqlist2[, 1]
  dataset_nFreqs = as.matrix(freqlist)
  mfw_to_test = seq(from = mfw_from, to = mfw_to, by = mfw_by)
  classifier = "nsc"
  f1_all = c()
  acc_all = c()
  
  x <- for (mfw in mfw_to_test) {
    data_subset = dataset_nFreqs[, 1:mfw]
    current_dataset = stylo::perform.culling(data_subset, culling)
    
    current_results = stylo::crossv(training.set = current_dataset, 
                                    cv.mode = "leaveoneout", classification.method = classifier)
    get_performance = stylo::performance.measures(current_results)
    get_f1 = get_performance$avg.f
    acc = get_performance$accuracy
    f1_all = c(f1_all, get_f1)
    acc_all = c(acc_all, acc)
  }
  
  if (type == "w") {
    type_name <- "Word"
  }
  else {
    type_name <- "Character"
  }
  
  main <- gsub(" "," ", paste("Performance Measures:",type_name , size, "- Grams"))
  x <- gsub(" "," ", paste("Most Frequent",type_name, size, "- Grams"))
  plot(acc_all~ mfw_to_test,
       main = main,
       ylab = "Accuracy", 
       xlab = x, 
       ylim = c(0.4, 1), 
       col = c("red"), pch = 17, yaxt="n",
       sub = paste("Culled@ ", culling))
  axis(2, at=pretty(acc_all), lab=paste0(pretty(acc_all) * 100, " %"), las=TRUE)
  par(new=TRUE)
  
  #### layers: adding a new layer to the existing plot ----
  
  points(f1_all ~ mfw_to_test, col = "blue",pch = 16)
  lines(acc_all ~ mfw_to_test, col = "red")
  lines(f1_all ~ mfw_to_test, col = "blue", lty=2)
  
  
  #### legend ----
  legend("bottomright", 
         legend = c("Accuracy", "F1 Score "),
         col = c("red","blue"),
         text.col = c("red","blue"),
         pch = c(17,16), 
         bty = "n",
         lty=c(1, 2))
  
}

#' @export
getNscTable <- function(type = "w", size = 1, mfw_min = 10, mfw_max = 150, mfw_incr =10, culling_min = 20, culling_max = 70, culling_incr = 10,
                        cv_folds = 10, export_table = F){
  x <- stylo::classify(corpus.lang = "Other",
                       analyzed.features = type,
                       ngram.size = size,
                       mfw.min = mfw_min, mfw.max = mfw_max, mfw.incr = mfw_incr,
                       culling.min = culling_min,
                       culling.max = culling_max,
                       culling.incr = culling_incr,
                       mfw.list.cutoff = 5000,
                       classification.method = "nsc", 
                       cv.folds = cv_folds,
                       distance.measure = "eder",
                       show.features = T,
                       gui = F)
  
  
  if(export_table == T){
    stargazer::stargazer(x$distinctive.features, type = "html", summary= F) # print table as html or latex
    cat("<p>Settings:</p><p>Mfw Max. - ", mfw_max, "</p>", "<p>Culling Max. - ", culling_max, "</p>")
    cat("\nPlease copy paste code from console to external editor and save as HTML-file.")
  }
  print(x$overall.success.rate)
  return(x)
}


#' @export
plotFeatureFreqs <- function(df, x = var, y = value, fill = var, measure.var = measure.var, title = "plot", x_title ="", y_title = "Frequency", fill_title = "Variety", significance = F, test = "wilcox.test", test_args = "two.sided", comparisons = list(c("Slovak", "Transcarpathian"), c("Lemko","Transcarpathian"), c("Lemko", "Slovak"), 3, simplify = F)){
  df_melt <- reshape::melt(df, measure.vars  = measure.var)
  melt_plot <- ggplot2::ggplot(df_melt, ggplot2::aes(x = var, y = value, fill = var), na.rm = T) +
    ggplot2::geom_boxplot() +
    ggplot2::labs(title= title,
         x = x_title,
         y = y_title,
         fill = fill_title)
  
  if(significance == T){
    print(melt_plot + ggsignif::geom_signif(data= df_melt, test="wilcox.test", test.args = test_args,
                                            comparisons = comparisons, 
                                            map_signif_level=T) + ggplot2::theme(aspect.ratio = 1))
  }
  else{
    suppressWarnings(print(melt_plot))
  }
}


#' @export
signifTests <- function(df, var = "var", depvar = "depvar", path = "", test.exact = FALSE, test.alternative = "two.sided") {
  var_list <- unique(df[[var]])
  filename <- capture.output(cat(depvar, "tests.txt", sep = "_"))
  file <- capture.output(cat(path,"/",filename, sep = ""))
  cat("Tests Output for feature: ", depvar,  "\n", file = file)
  cat("----------------------------------------------------","\n\n",file = file, append = TRUE)
  rFromWilcox <-function(wilcoxModel, N){
    z<- qnorm(wilcoxModel$p.value/2)
    r<- z/ sqrt(N)
    cat(wilcoxModel$data.name, "Effect Size, r = ", r)
  }
  for (i in var_list) {
    df_sub <- subset(df, var != i)
    col_name <- rlang::enquo(depvar)
    levene_test <- suppressWarnings(car::leveneTest(df_sub[[depvar]], df_sub[[var]]))
    wilcox_test <- suppressWarnings(wilcox.test(value ~ var, reshape2::melt(df_sub), exact = test.exact, alternative = test.alternative))
    # export Levene test output
    cat("Levene Test for:\n", file = file, append = TRUE)
    cat(unique(df_sub$var), file = file, append = TRUE)
    # add 2 newlines
    cat("\n\n", file = file, append = TRUE)
    capture.output(levene_test, file = file, append = TRUE)
    cat("\n", file = file, append = TRUE)
    cat(">", file = file, append = TRUE)
    # add 2 newlines
    cat("\n\n", file = file, append = TRUE)
    # export Wilcoxon test output
    cat("Wilcoxon-Test for:\n", file = file, append = TRUE)
    # add 2 newlines
    cat(unique(df_sub$var), file = file, append = TRUE)
    capture.output(wilcox_test, file = file, append = TRUE)
    cat("\n", file = file, append = TRUE)
    cat(">", file = file, append = TRUE)
    # add 2 newlines
    cat("\n", file = file, append = TRUE)
    # export Effectsize
    cat("Effect Size for Wilcoxon-Test for:\n", file = file, append = TRUE)
    # add 2 newlines
    cat(unique(df_sub$var), file = file, append = TRUE)
    # add 2 newlines
    cat("\n", file = file, append = TRUE)
    capture.output(rFromWilcox(wilcox_test, nrow(reshape2::melt(df_sub))) , file = file, append = TRUE)
    # add 2 newlines
    cat("\n\n", file = file, append = TRUE)
    # add line
    cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~", file = file, append = TRUE)
    # add 2 newlines
    cat("\n\n", file = file, append = TRUE)
  }
}




#' @export
getFeatureFreqs <- function(type = "w", size = 1, token = "у", scale = F){
  name <- gsub(" ","", paste("styloFreqList","_", type, "_", size))
  freqlist1 <- eval(as.symbol(name))
  if(scale == T){
    subset0 <- as.data.frame(freqlist1)
    subset0 <- data.frame("Speaker" = subset0$Speaker, Token = subset0[, token], check.names = F)
    results <<- subset0 <- subset(subset0, Token >= 0)
    freqlist_scaled <- as.data.frame(scale(subset0[,2:ncol(subset0)]))
    dataframe <- cbind(subset0[,1], freqlist_scaled)
    colnames(dataframe)[1] <- "Speaker"
    colnames(dataframe)[2] <- "Token_Scaled"
    dataframe2 <-dataframe[order(dataframe$Token_Scaled, decreasing = T),]
    dataframe2$var <- dplyr::case_when(grepl("LEM", dataframe2$Speaker) ~ "Lemko", 
                                       grepl("TRA", dataframe2$Speaker) ~ "Transcarpathian",
                                       grepl("SLO", dataframe2$Speaker) ~ "Slovak")
    
    subset2 <- as.data.frame(freqlist1)
    dataframe22 <- data.frame("Speaker" = subset2$Speaker, Token = subset2[, token], check.names = F)
    dataframe22 <- subset(dataframe22, Token >=0)
    dataframe222 <-dataframe22[order(dataframe22$Token, decreasing = T),] 
    
    dataframe3 <- cbind(dataframe2, dataframe222$Token)
    names(dataframe3)[names(dataframe3) == "dataframe222$Token"] <- "Token"
  }
  else{
    subset <- as.data.frame(freqlist1)
    dataframe <- data.frame("Speaker" = subset$Speaker, Token = subset[, token], check.names = F)
    results <<- dataframe <- subset(dataframe, Token >= 0)
    dataframe2 <-dataframe[order(dataframe$Token, decreasing = T),] 
    dataframe2$var <- dplyr::case_when(grepl("LEM", dataframe2$Speaker) ~ "Lemko", 
                                       grepl("TRA", dataframe2$Speaker) ~ "Transcarpathian",
                                       grepl("SLO", dataframe2$Speaker) ~ "Slovak")
    dataframe3 <- dataframe2
  }
  names(dataframe3)[names(dataframe3) == "Token"] <- token
  names(dataframe3)[names(dataframe3) == "Token_Scaled"] <- gsub(" ","", paste(token, "_scaled"))
  return(dataframe3)
}


getFeatureFreqsRegex <- function(type = "w", size = 2, token = "^\\bу\\b.*", tokenInWords = "", scale = F, perl = F){
  name <- gsub(" ","", paste("styloFreqList","_", type, "_", size))
  freqlist1 <- eval(as.symbol(name))

#normalized frequencies
  if(scale == T){
    freqlist0 <- as.data.frame(freqlist1)
    #adds names of columns automatically, if there are more features than one
    regexResult0 <- as.data.frame(freqlist0[ , grepl( token , names( freqlist0 ), perl = perl) ])
    if(ncol(regexResult0) == 0){
      stop("No Match!")
    }
    #name of column will be added, if there is only one feature
    if(ncol(regexResult0) < 2){
      colnames(regexResult0)[1] <- grep(token, names(freqlist0), value = TRUE)
    }
    #adds speaker column 
    regexResult0 <- cbind(freqlist0[,1],regexResult0)
    colnames(regexResult0)[1] <- "Speaker"
    
    #Scaling of the frequencies, cfeates new DF 
    freqlist_scaled <- as.data.frame(scale(regexResult0[,2:ncol(regexResult0)]))
    #sums up freqs if there is more than one feature contained
    if(ncol(freqlist_scaled) >= 2 ){
      freqlist_scaled$Sum <- as.numeric(apply(freqlist_scaled[,1:ncol(freqlist_scaled)], 1, sum))
      freqlist <- subset(freqlist_scaled, Sum >= 0)
    }
    #skips the summing up process
    else{
      freqlist_scaled$Sum <- freqlist_scaled[,1]
      colnames(freqlist_scaled)[1] <- colnames(regexResult0)[2]
    }
    #Adds speaker column and creates new DF- 
    freqlist <- cbind(regexResult0[,1], freqlist_scaled)
    colnames(freqlist)[1] <- "Speaker"
  #Now we can export the freqlist containing all hits and the scaled sum. 
    results <<- freqlist
    #We have to process the data further so we can add metadata and sum up all the freqs
    
    regexResult <- as.data.frame(freqlist[ , grepl( token , names( freqlist ), perl = perl ) ])
    if(ncol(regexResult) < 2){
      colnames(regexResult)[1] <- grep(token, names(freqlist0), value = TRUE)
    }
    
    #save that for later
    regexResult2 <- regexResult
  
    #results <<-  as.data.frame(regexResult, freqlist$Speaker) %>% tibble::rownames_to_column(., "Speaker")
    if(size > 1){
      if(ncol(regexResult) > 1){
        regexResult$Sum <- as.numeric(apply(regexResult[,1:ncol(regexResult)], 1, sum))
      }
      else{
        regexResult$Sum <- regexResult[,1]
      }
      subset <- cbind(freqlist[,1], regexResult)
      dataframe <- data.frame("Speaker" = subset[,1], "Token_Scaled" = subset$Sum, check.names = F)
    }
    else{
      subset <- cbind(freqlist[,1], regexResult)
      dataframe <- data.frame("Speaker" = subset[,1], "Token_Scaled" = subset[,2], check.names = F)
    }
    dataframe2 <-dataframe[order(dataframe$Token, decreasing = T),]
    dataframe2$var <- dplyr::case_when(grepl("LEM", dataframe2$Speaker) ~ "Lemko", 
                                       grepl("TRA", dataframe2$Speaker) ~ "Transcarpathian",
                                       grepl("SLO", dataframe2$Speaker) ~ "Slovak")
    #regexResult2 <- as.data.frame(freqlist1[ , grepl( token , names( freqlist1 ), perl = perl ) ])
    
    if(size > 1){
      if(ncol(regexResult2) > 1){
      regexResult2$Sum <- as.numeric(apply(regexResult2[,1:ncol(regexResult2)], 1, sum))
      }
      else{
        regexResult2$Sum <- regexResult2[,1]
      }
      subset2 <- cbind(freqlist1[,1], regexResult2)
      names(subset2[,2]) <- token
      subset2 <- subset(subset2, subset2$Sum >= 0)
      dataframe22 <- data.frame("Speaker" = subset2[,1], "Token" = subset2$Sum, check.names = F)
      dataframe22 <-dataframe22[order(dataframe22$Token, decreasing = T),]
      dataframe22$var <- dplyr::case_when(grepl("LEM", dataframe22$Speaker) ~ "Lemko", 
                                          grepl("TRA", dataframe22$Speaker) ~ "Transcarpathian",
                                          grepl("SLO", dataframe22$Speaker) ~ "Slovak")
    }
    else{
      subset2 <- cbind(freqlist1[,1], regexResult2)
      names(subset2[,2]) <- token
      subset2 <- subset(subset2, subset2[,2] >= 0)
      dataframe22 <- data.frame("Speaker" = subset[,1], "Token" = subset[,2], check.names = F)
      dataframe22 <-dataframe[order(dataframe$Token, decreasing = T),]
      dataframe2$var <- dplyr::case_when(grepl("LEM", dataframe22$Speaker) ~ "Lemko", 
                                         grepl("TRA", dataframe22$Speaker) ~ "Transcarpathian",
                                         grepl("SLO", dataframe22$Speaker) ~ "Slovak")
    }
    #freqlist$Sum
    #dataframe2 <- cbind(dataframe2, dataframe22$Token)
    freqlist_order <-freqlist[order(freqlist$Sum, decreasing = T),]
    dataframe2 <- cbind(dataframe2, freqlist_order$Sum)
    names(dataframe2)[names(dataframe2) == "freqlist$Sum"] <- "Token"
  }
  
  else{
    regexResult <- as.data.frame(freqlist1[ , grepl( token , names( freqlist1 ), perl = perl )])
    if(ncol(regexResult) == 0){
      stop("No Match!")
    }
    if(ncol(regexResult) < 2){
      colnames(regexResult)[1] <- grep(token, names(freqlist1), value = TRUE)
    }
    results <<-  as.data.frame(regexResult, freqlist1$Speaker) %>% tibble::rownames_to_column(., "Speaker")
    if(size >1){
      if(ncol(regexResult) > 1){
        regexResult$Sum <- as.numeric(apply(regexResult[,1:ncol(regexResult)], 1, sum))
      }
      else{
        regexResult$Sum <- regexResult[,1]
      }
      #regexResult$Sum <- as.numeric(apply(regexResult[,1:ncol(regexResult)], 1, sum))
      subset <- cbind(freqlist1[,1], regexResult)
      names(subset[,2]) <- token
      subset <- subset(subset, subset$Sum >= 0)
      dataframe <- data.frame("Speaker" = subset[,1], "Token" = subset$Sum, check.names = F)
      dataframe2 <-dataframe[order(dataframe$Token, decreasing = T),]
      dataframe2$var <- dplyr::case_when(grepl("LEM", dataframe2$Speaker) ~ "Lemko", 
                                         grepl("TRA", dataframe2$Speaker) ~ "Transcarpathian",
                                         grepl("SLO", dataframe2$Speaker) ~ "Slovak")
    }
    else{
      subset <- cbind(freqlist1[,1], regexResult)
      names(subset[,2]) <- token
      subset <- subset(subset, subset[,2] >= 0)
      dataframe <- data.frame("Speaker" = subset[,1], "Token" = subset[,2], check.names = F)
      dataframe2 <-dataframe[order(dataframe$Token, decreasing = T),]
      dataframe2$var <- dplyr::case_when(grepl("LEM", dataframe2$Speaker) ~ "Lemko", 
                                         grepl("TRA", dataframe2$Speaker) ~ "Transcarpathian",
                                         grepl("SLO", dataframe2$Speaker) ~ "Slovak")
    }
  }
  if(scale ==T){
    if(tokenInWords ==""){
      names(dataframe2)[names(dataframe2) == "Token_Scaled"] <- gsub(" ","", paste(token, "_scaled"))
      
    }
    names(dataframe2)[names(dataframe2) == "Token_Scaled"] <- gsub(" ","", paste(tokenInWords, "_scaled"))
    if(tokenInWords ==""){
      names(dataframe2)[names(dataframe2) == "Token"] <- token
      
    }
    names(dataframe2)[names(dataframe2) == "Token"] <- tokenInWords
  }
  else{
    if(tokenInWords ==""){
      names(dataframe2)[names(dataframe2) == "Token"] <- token
      
    }
    names(dataframe2)[names(dataframe2) == "Token"] <- tokenInWords
  }
  return(dataframe2)
}







