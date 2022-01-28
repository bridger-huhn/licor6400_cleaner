library(tidyverse)
#this sets the wd to whichever file you want to extract files from
setwd("/Users/Lina/Desktop/ML/IRGA_TC Fall 2015/Weinig  1/CSV_files")

#gets all files with the given file type
allFiles<-dir(pattern = cat(paste0('\'','.',"csv","$",'\'')))


outDF<-data.frame() #makes one giant data frame from all the files in that folder
for (i in 1:length(allFiles)){
  #for the current file read it in
  f<- read.delim(allFiles[i],sep = ",")
  #append to the output
  outDF<- dplyr::bind_rows(outDF,f)
  
}
rm(f)

#creates the function
LC<-function(dat){
  #Renames the columns
  names(dat)<- as.character(unlist(dat[min(which(dat[,1] == "Obs")),]))
  
  #creates a comments column
  dat$comment<-NA
  dat<-dat[c(ncol(dat),1:(ncol(dat)-1))]
  
 
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
  #strips out all Remarks that are blank
  dat<-dat[-(which(dat[,2] == "Remark=" )),]
  #####rows to exclude#####
  #makes all numbers numeric and non numbers NAs
  dat[,2]<-as.numeric(as.character(unlist(dat[,2])))
  #removes all NA rows
  dat<- dat[-c(which(is.na(dat[,2]))),]
 
  
  return(dat)
}

#cleans up the data
data <- LC(dat=outDF)

