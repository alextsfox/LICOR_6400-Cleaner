require(tidyr)

###import data

dat<-read.delim("C:/Users/bhuhn/Desktop/Data_MNGMT/csvs/120123 rd2 down_.csv", sep = ",")

getwd()

LC<-function(dat){
  #This is to extract comments
  rem<- dat[(which(dat == "Remark=")),]
  rem2<-rem[-(grep(pattern = "\"", rem$X)),]
  cname<-as.character(rem2[1,2])
  
  #Renames the columns
  names(dat)<- as.character(unlist(dat[min(which(dat == "Obs"),)]))
  dat<- dat[-(which(dat == "Remark=")),]
  #creats a column with the comments put into the LICOR-6400
  dat$Rep <- cname
  dat<-dat[c(ncol(dat),1:(ncol(dat)-1))]
  ## get rid of everything up to data
  d<-as.numeric(which(dat$Photo == "out"))
  dat<-dat[-c(1:d),]
  
  return(dat)
  
}
Get.LC.Files<- function(wd = FALSE, filetag = FALSE, type = "csv"){
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



dat<-LC(dat)

dat[9,3]
