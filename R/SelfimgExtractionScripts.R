library(stringr)

MyImgLibrary<-function(){
htmlCode = readLines("S:/Gastroenterology/Seb/R/Data/SelfAudit/Images Captured with Proc Data Audit.html")
mergedhtml<-paste(htmlCode, sep="", collapse="")
df<-strsplit(mergedhtml, "Procedure Performed: ", fixed = FALSE, perl = FALSE, useBytes = FALSE)
df<-as.data.frame(df)
colnames(df)<-c("df")


df$Endo_Endoscopist<-str_extract(df$df,"Endoscopist.*?<")
df$Endo_Endoscopist<-gsub("Endoscopist: ","",df$Endo_Endoscopist)
df$Endo_Endoscopist<-gsub("<","",df$Endo_Endoscopist)




df$Endo_ResultEntered<-str_extract(df$df,"Date of Procedure.*?<")
df$Endo_ResultEntered<-gsub("Date of Procedure: ","",df$Endo_ResultEntered)
df$Endo_ResultEntered<-gsub("<","",df$Endo_ResultEntered)
df$Endo_ResultEntered<-as.Date(df$Endo_ResultEntered,format="%d/%m/%Y")


df$PatientID<-str_extract(df$df,"Patient Record.*?<")
df$PatientID<-gsub("Patient Record Number : ","",df$PatientID)
df$PatientID<-gsub("<","",df$PatientID)


df$img<-str_extract(df$df,"img src.*?jpg")
df$img<-gsub("img src=\"Images Captured with Proc Data Audit.files/","",df$img)
df$df<-NULL


#Now collapse the table so all image files for a procedure in one row only:
library(data.table)
mergeddf<-as.data.frame(as.data.table(df)[, toString(img), by = list(Endo_Endoscopist, Endo_ResultEntered,PatientID)])

#Split the comma separated img list into a list within the data frame so you should then be able to iterate over it:
mergeddf$V1<-gsub(" ","",mergeddf$V1)
mergeddf$V1<-as.list(strsplit(mergeddf$V1,","))

mergeddf$url<-lapply(mergeddf$V1,function(x) paste0('S:\\Gastroenterology\\Seb\\R\\Data\\SelfAudit\\Images Captured with Proc Data Audit.files\\\\',x))
library(pander)
mergeddf$url<-sapply(mergeddf$url,pandoc.image.return)
return(mergeddf)
#pandoc html
}
