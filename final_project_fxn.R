#LC takes IRGA data and removes all non-data rows. It also creates a Comment column that stores data taken after a Remark
LC<-function(dat){
  #Renames the columns
  names(dat)<- as.character(unlist(dat[min(which(dat[,1] == "Obs")),]))
  
  #creates a comments column
  dat$comment<-NA
  dat<-dat[c(ncol(dat),1:(ncol(dat)-1))]
  
  #strips out all Remarks that are blank
  dat<-dat[-(which(dat[,2] == "Remark=" & dat[,3] == "")),]
  
  #This is to put comments in comment column
  for (i in 2:nrow(dat)){
    ifelse(dat[i,2] == 'Remark=',
           ifelse(grepl(pattern = "\"",dat[i,3]) == FALSE,
                  dat[i,1] <- dat[i,3],
                  ifelse(grepl(pattern = "=",dat[i,3]),
                         dat[i,1] <- dat[i-1,1],
                         ifelse(grepl(pattern = "Launched",dat[i,3]),
                                dat[i,1]<-dat[i-1,1], dat[i,1]<-paste(stringr::str_split(stringr::str_split(dat[i,3],":", simplify = TRUE)[3], " ", simplify= TRUE)[-1], collapse = " "))))
           ,dat[i,1] <- dat[i-1,1])
    
  }
  #####rows to exclude#####
  #makes all numbers numeric and non numbers NAs
  dat[,2]<-as.numeric(as.character(unlist(dat[,2])))
  #removes all NA rows
  dat<- dat[-c(which(is.na(dat[,2]))),]
 
  
  return(dat)
}

#Get.LC.Files extracts all .csv files in a folder and puts them into one continuous data frame
Get.LC.Files<- function(wd = NULL, filetag = NULL, type = "csv"){
  #this sets the wd to whichever file you want to extract files from
  if (wd != FALSE){
    setwd(wd)}
  if (type != "csv"){
    stop('files need to be .csv files')
  }
  #gets all files with the given file type
  allFiles<-dir(pattern = cat(paste0('\'','.',type,"$",'\'')))
  
  #filetag is the text shared among all files you wish to import
  if (filetag != FALSE){
    r<- which(str_dectect(allFiles,pattern = cat(paste0('\'',filetag,'\'')) ))
    allFiles<-allFiles[r]
  }
  outDF<-data.frame()
  for (i in 1:length(allFiles)){
    #for the current file read it in
    f<- read.delim(allFiles[i],sep = ",")
    #append to the output
    outDF<- dplyr::bind_rows(outDF,f)
    
  }
  return(outDF)
}

dat<-Get.LC.Files()
dat<-LC(dat)
