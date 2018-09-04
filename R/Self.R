library(stringr)
library(ggplot2)
library(lattice)
library(compare)
library(XLConnect)
library(grid)
library(reshape2)
library(gtools)
library(gplots)

library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library(splitstackshape)
library(knitr)
library(xtable)
library(pander)
library(data.table)
library(gridExtra)
library(pheatmap)
library(tidyr)
library(EndoMineR)
# local({
#   hook_plot = knit_hooks$get('plot')
#   knit_hooks$set(plot = function(x, options) {
#     x = paste(x, collapse = '.')
#     if (!grepl('\\.svg', x)) return(hook_plot(x, options))
#     # read the content of the svg image and write it out without <?xml ... ?>
#     paste(readLines(x)[-1], collapse = '\n')
#     #paste("<figure><img src=\"", opts_knit$get("base.url"), paste(x, collapse = "."), 
#     #      "\"><figcaption>", options$fig.cap, "</figcaption></figure>", sep = "")
#   })
# })

#For the imag extraction- need to get this off EndoSoft. Choose the query called Images Captured with Proc Data Audit for the month beginning 1 to the end of the month
#Then export HTML to folder called "S:\Gastroenterology\Seb\R\Data\SelfAudit" with styles and Fixed width and JPEG (default settings selected) and it will create a folder called Images Captured with Proc Data Audit.files. The html will be one level up

Self = loadWorkbook("S:\\Gastroenterology\\Seb\\R\\Data\\SelfAudit\\Gastroscopy_TCR2232HistoReport_run%2004-01-2017(Apr18).xlsx")
Self = XLConnect::readWorksheet(Self, sheet="Data",header=TRUE)


#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  ############ DATA CLEANING ##################  #############################################  #############################################
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  


names(Self)<-c("PatientID",  "NHSno", "PatientName", "birthdaynum","birthmonthnum" ,"birthyearnum" ,"Endo_ResultName", "Endo_ResultPerformed", "Endo_ResultEntered", "Endo_ResultText", "Histo_ResultName", "Histo_ResultPerformed", "Histo_ResultEntered", "Histo_ResultText")
#Remove the first row if necessary
Self<-Self[2:nrow(Self),]

#Get rid of everything without an endoscopy report (because pre 2007 there won't be one, just histopath)
Self<-subset(Self,!(is.na(Self["Endo_ResultName"])))
#Split the spreadsheet up into new columns and then get rid of the original ones


#For the Endoscopy text
source("S:\\Gastroenterology\\Seb\\R\\Scripts\\Generics\\CleanUp.R")
#Self<-EndoscChopper(Self)

Self$Endo_ResultText<-gsub("2nd Endoscopist:","Second endoscopist:",Self$Endo_ResultText)
EndoscTree<-list("Patient Name:","Date of Birth:","General Practicioner:","Hospital Number:",
                 "Date of Procedure:","Endoscopist:","Second endoscopist:","Trainee:","Referring Physician:","Nurses:","Medications",
                 "Instrument","Extent of Exam:","INDICATIONS FOR EXAMINATION","PROCEDURE PERFORMED","FINDINGS",
                 "ENDOSCOPIC DIAGNOSIS","RECOMMENDATIONS","COMMENTS","FOLLOW UP","")
for(i in 1:(length(EndoscTree)-1)) {
  Self<-Extractor(Self,"Endo_ResultText",as.character(EndoscTree[i]),
                  as.character(EndoscTree[i+1]),as.character(EndoscTree[i]))
}


#Extra tidying...DATES ARE A PAIN IN THE ASS....
Self$PROCEDUREPERFORMED<-gsub("(Withdrawal|Quality|ENDOSCOPIC).*","",Self$PROCEDUREPERFORMED)
Self$PROCEDUREPERFORMED<-gsub("\\s+[(]OGD[)]","",Self$PROCEDUREPERFORMED)
#Convert date time to date in R
Self$Endo_ResultEntered<-as.Date(as.POSIXct(Self$Endo_ResultEntered,format="%d/%m/%Y"))
#And sometimes this is the one to use...
#Self$Endo_ResultEntered<-as.Date(format(Self$Endo_ResultEntered), "%d/%m/%Y")


#Change the names of the columns as EPR keep on changing things around:

names(Self)<-c("PatientID","NHSno","PatientName","birthdaynum","birthmonthnum","birthyearnum","Endo_ResultName","Endo_ResultPerformed","Endo_ResultEntered","Endo_ResultText","Histo_ResultName","Histo_ResultPerformed","Histo_ResultEntered","Histo_ResultText","DateofBirth","GeneralPracticioner","HospitalNumber","DateofProcedure","Endo_Endoscopist","SecondEndo_Endoscopist","Trainee","ReferringPhysician","Nurses","Medications","Instrument","ExtentofExam","IndicationsFroExamination","ProcPerformed","Findings","EndoscopDiagn","Recommend","COMMENTS","FollowUp")


Self<-HistolChopper(Self)

Self$FindingsAlgoNegs<-str_extract_all(Self$Findings,".*[Nn]egative.*|.*[Nn]either.*|[Nn]o .*?[\\.\n]|.*[Nn]ormal.*?[\\.\n]|There (?:is|was|are) no.*?\\.|.*None.*?[\\.\\n]|.*unremarkable?\\.|^Negative.*?[\\.\\n]|[Oo]therwise (?:normal|unremarkable).*?[\\.\\n]")
Self$EndoscopDiagnAlgoNegs<-str_extract_all(Self$EndoscopDiagn,".*[Nn]egative.*|.*[Nn]either.*|[Nn]o .*?[\\.\\n]|.*[Nn]ormal.*?[\\.\\n]|There (?:is|was|are) no.*?\\.|.*None.*?[\\.\\n]|.*unremarkable?\\.|^Negative.*?[\\.\\n]|[Oo]therwise (?:normal|unremarkable).*?[\\.\\n]")
#Get histopath into a proper format for line by line extraction:
Self$Histo_ResultTextForFindings<-gsub("\\.\\.","\\.",Self$Histo_ResultTextForFindings)
Self$Histo_ResultTextForFindings<-gsub("\\.","\\.\n",Self$Histo_ResultTextForFindings)
Self$Histo_ResultTextForFindings<-gsub(":","",Self$Histo_ResultTextForFindings)
Self$Histo_ResultTextForFindingsAlgoNegs<-str_extract_all(Self$Histo_ResultTextForFindings,".*[Nn]egative.*|.*[Nn]either.*|[Nn]o .*?[\\.\\n]|.*[Nn]ormal.*?[\\.\\n]|There (?:is|was|are) no.*?[\\.\\n]|\\..*None.*?[\\.\\n]|\\..*unremarkable.*?[\\.\\n]|^Negative.*?[\\.\\n]|[Oo]therwise (?:normal|unremarkable).*?[\\.\\n]")
SelfWithNegs<-data.frame(Self["PatientID"],Self["Findings"],Self["FindingsAlgoNegs"],Self["EndoscopDiagn"],Self["EndoscopDiagnAlgoNegs"],Self["Histo_ResultTextForFindings"],Self["Histo_ResultTextForFindingsAlgoNegs"])


#Import the image file here from html report from Endosoft:
#You need to go to Endosoft, then pick the query under the GRS tab, under GSTT, called Images Captured with Proc Data Audit.files
#Then run for the month you are interested in
#Then export as HTML into the folder called "S:\Gastroenterology\Seb\R\Data\SelfAudit" It will name the folder itself as Images Captured with Proc Data Audit.files and the html file will be in the SelfAudit level
#Then run the script
########################################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################
############################### ############################### ############################### ############################### 
############################### Drop in the image table here############################### ############################### 
source("S:\\Gastroenterology\\Seb\\R\\Scripts\\SelfAudit\\SelfimgExtractionScripts.R")  
images<-MyImgLibrary()
############################### ############################### ############################### ############################### 
############################### ############################### ############################### ############################### 
source("S:\\Gastroenterology\\Seb\\R\\Scripts\\Generics\\CleanUp.R")

Self<-merge(images,Self,by=c("Endo_ResultEntered","PatientID"),all.y=T)

#Tidy the whole thing up:
Self$Endo_Endoscopist.x<-NULL
names(Self)[names(Self) == 'Endo_Endoscopist.y'] <-"Endo_Endoscopist"

#############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  
#############################################  #################### SANDBOXING   #################  #############################################  
#Alltbl<-Self[!Self$url=="NULL",]
#Alltbl$url<-lapply(Alltbl$url,"[[",1)
#Alltbl$url<-unlist(Alltbl$url)
#Alltbl<-Alltbl[1:5]
Self$V1.x<-NULL
Self$V1<-NULL
#Self<-Self[1:5]

#setwd("S:\\Gastroenterology\\Seb\\R\\Scripts")
rmarkdown::render("S:\\Gastroenterology\\Seb\\R\\Scripts\\SelfAudit\\Self2SANDBOX.Rmd",output_file=paste("MonthlyEndoscopyUnitReport2",Sys.Date(),'.docx'))
#############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  



#############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  
#############################################  ######## DATA CLEANING ######################  #############################################  
#############################################  #############################################  #############################################  
#############################################  #############################################  #############################################


#Count the number of sentences in a column:
#Get rid of the starting period


Self$Findings<-gsub(".\n.\n",".\n",Self$Findings)
Self$TotalLines<-str_count(Self$Findings, ".\n")
Self$TP<-str_count(Self$FindingsAlgoNegs, "\\.")
Self$TN<-Self$TotalLines-Self$TP
Self$FP<-0
Self$FN<-0


#Self$Histo_ResultTextForFindings2<-gsub("\\.",".\n",Self$Histo_ResultTextForFindings)
Self$TotalLinesHistoResultTextForFindings<-str_count(Self$Histo_ResultTextForFindings, ".\n")
Self$TPHistoResultTextForFindings<-str_count(Self$Histo_ResultTextForFindingsAlgoNegs, "\\.")
Self$TNHistoResultTextForFindings<-Self$TotalLinesHistoResultTextForFindings-Self$TPHistoResultTextForFindings
Self$FPHistoResultTextForFindings<-0
Self$FNHistoResultTextForFindings<-0
SelfForSheet<-data.frame(Self["Histo_ResultTextForFindings"],Self["Histo_ResultTextForFindingsAlgoNegs"],Self["TotalLinesHistoResultTextForFindings"],Self["TPHistoResultTextForFindings"],Self["FPHistoResultTextForFindings"],Self["TNHistoResultTextForFindings"],Self["FNHistoResultTextForFindings"],Self["Findings"],Self["FindingsAlgoNegs"],Self["TP"],Self["FP"],Self["TN"],Self["FN"])

########################### ########################### ########################### ########################### 
########################### ########################### ########################### ########################### 
########################### ########################### ########################### ########################### 
########################### Sandbox stuff ########################### ########################### ########################### 
########################### ########################### ########################### ########################### 
########################### ########################### ########################### ########################### 
########################### ########################### ########################### ########################### 
library(koRpus) 
SelfForSheet<-SelfForSheet[!is.na(SelfForSheet$Histo_ResultTextForFindings),]

#mashup<-paste(unlist(SelfForSheet$Histo_ResultTextForFindings), collapse =" ")

#token1<-tokenize(mashup, format = "obj", fileEncoding ="UTF-8", split = "[[:space:]]",ign.comp = "-", heuristics = "abbr,suf,pre", heur.fix = list(pre = c("'","'"), suf = c("'", "'")), abbrev = NULL, tag = TRUE, lang = "en",sentc.end = c(".", "!", "?", ";", ":"), detect = c(parag = FALSE, hline = FALSE), clean.raw = NULL, perl = FALSE, stopwords = tm::stopwords("en"),stemmer = SnowballC::wordStem)

#describe(token1)

#readability(token1, hyphen = NULL,
#             index = c("ARI", "Bormuth", "Coleman", "Coleman.Liau", "Dale.Chall",
#                       "Danielson.Bryan", "Dickes.Steiwer", "DRP", "ELF", "Farr.Jenkins.Paterson",
#                       "Flesch", "Flesch.Kincaid", "FOG", "FORCAST", "Fucks", "Harris.Jacobson",
#                       "Linsear.Write", "LIX", "nWS", "RIX", "SMOG", "Spache", "Strain",
#                       "Traenkle.Bailer", "TRI", "Tuldava", "Wheeler.Smith"), parameters = list(),
#             word.lists = list(Bormuth = NULL, Dale.Chall = NULL, Harris.Jacobson = NULL,
#                               Spache = NULL), fileEncoding = "UTF-8", tagger = "kRp.env",
#             force.lang = NULL, sentc.tag = "sentc", nonword.class = "nonpunct",
#             nonword.tag = c(), quiet = FALSE)

########################### ########################### ########################### ########################### 
########################### ########################### ########################### ########################### 
########################### ########################### ########################### ########################### 
########################### ########################### ########################### ########################### 

#Split the dataset up according to the Endoscopist

Dxlst<-split(Self,Self$Endo_Endoscopist)
names(Dxlst)<-paste0('df',gsub("\\s+","",names(Dxlst)))

#Gives the basic output of the number for each indication
numDx<-data.frame(t(lapply(Dxlst,nrow))) 
########REFER TO INDIVIDUAL dataframe from the list
#x<-Dxlst[["dfSebastianZeki"]]

############################################################################################################
############################################################################################################
################## Basic demographics for indications: #####################################################
############################################################################################################
############################################################################################################
Self$IndicationsFroExamination<-gsub("PROCEDURE PERFORMED","",Self$IndicationsFroExamination)
#Include this next line if you want to include the endoscopies where there are multiple indications. The default is now only to include 
#endoscopies where there are single indications so that the data is purified

s<-Self[!grepl("\\..*[A-Z]",Self$IndicationsFroExamination),]
s<-strsplit(as.character(Self[["IndicationsFroExamination"]]),'\\.')


IndicVsBx<-data.frame(Reason=unlist(s),AB=rep(Self[["NumbOfBxs"]],sapply(s,FUN=length)))
IndicVsBx$Reason<-as.character(IndicVsBx$Reason)

IndicVsBx<-IndicVsBx[!grepl("\n$",IndicVsBx$Reason),]
IndicVsBx$Reason<-gsub("\n","",IndicVsBx$Reason)
IndicVsBx<-IndicVsBx[!grepl("^$",IndicVsBx$Reason),]

cc<-IndicVsBx %>%
  group_by(Reason) %>%
  dplyr::summarise(mean=mean(AB,na.rm=T),count=n(),ToTalNumBx=sum(AB,na.rm=T))%>%
  filter(count>1,mean>0,!is.na(Reason))

cc<-cc[!is.na(cc$mean),]


# cc<-cc[order(cc$ToTalNumBx),]
# 
# cc$Reason <- factor(cc$Reason, levels = cc$Reason[order(cc$ToTalNumBx)])
# #Define the corpus of indications you are interested in:
# 
myIndications<-c("Abdominal Pain","Abdominal Pain/Bloating","Altered bowel habit- alternating/constipation","Anaemia/Low Iron or Vitamins","Blood PR","CHRONIC DIARRHOEA","Dyspepsia","Dysphagia/Odynophagia","Known IBD- Assessment of Activity","Nausea and/or Vomiting","Reflux-like Symptoms/Atypical Chest Pain","Surveillance- Family History of CRC/Polyps","Surveillance- Known IBD","Surveillance- Previous Colorectal Cancer","Surveillance- Previous Polyps","Surveillance- Previous Polyps/Metaplasia etc","Weight Loss")
# 
# cc<-cc[grep(paste(myIndications, collapse='|'), cc$Reason,perl=TRUE),]

detach(package:plyr)
source("S:\\Gastroenterology\\Seb\\R\\Scripts\\Generics\\Analytics.R")

########################### Number of biopsies taken by person by Indication############################################################## 

IndicVsBxByPerson<-data.frame(Reason=unlist(s),AB=rep(Self[["Endo_Endoscopist"]],sapply(s,FUN=length)),AC=rep(Self[["NumbOfBxs"]],sapply(s,FUN=length)))

IndicVsBxByPerson$Reason<-as.character(IndicVsBxByPerson$Reason)

IndicVsBxByPerson<-IndicVsBxByPerson[!grepl("\n$",IndicVsBxByPerson$Reason),]
IndicVsBxByPerson$Reason<-gsub("\n","",IndicVsBxByPerson$Reason)
IndicVsBxByPerson<-IndicVsBxByPerson[!grepl("^$",IndicVsBxByPerson$Reason),]

ccByPerson<-IndicVsBxByPerson %>%
  group_by(Reason,AB) %>%
  dplyr::summarise(mean=mean(AC,na.rm=T),count=n())%>%
  filter(count>1,mean>0,!is.na(Reason))%>%
  mutate(ToTalNumBx =count*mean) 

ccByPerson<-data.frame(ccByPerson)
ccByPerson$AB<-as.character(ccByPerson$AB)
ccByPerson<-ccByPerson[grepl(paste(myIndications, collapse='|'), ccByPerson$Reason,perl=TRUE),]

BxByIndicationByPesron<-dcast(ccByPerson,formula=Reason~AB,value.var="mean")

#Get the mean number of biopsies for each indication for the unit so can compare to individual's number of biopsies in the individual function
row.names(BxByIndicationByPesron)<-BxByIndicationByPesron$Reason
#BxByIndicationByPesron<-BxByIndicationByPesron[-grepl("\n",row.names(BxByIndicationByPesron)),]
BxByIndicationByPesron$Reason<-NULL
MeanNumBx<-data.frame(apply(BxByIndicationByPesron,1,function(x){mean(x,na.rm=T)}))
#row.names(MeanNumBx)<-gsub("\n","",row.names(MeanNumBx))
names(MeanNumBx)<-c("Result")
MeanNumBx$Reason<-row.names(MeanNumBx)

MeanNumBx<-MeanNumBx[grepl(paste(myIndications, collapse='|'), MeanNumBx$Reason,perl=TRUE),]


########################### Number of endoscopies including subtype for this month############################################################## 
Self$ProcPerformed<-as.character(Self$ProcPerformed)
ProcedurePlot<-Self %>% 
  group_by(ProcPerformed) %>%
  summarise(freq=n()) 

#Getting heatmap of endoscopy types done by person
matProcPerf<-as.matrix(table(Self$ProcPerformed,Self$Endo_Endoscopist))
myEndoMatrix<-as.data.frame(table(Self$ProcPerformed,Self$Endo_Endoscopist))
myEndoMatrix<-xtabs(Freq~Var1+Var2,data=myEndoMatrix) 

#Order the bar chart
ProcedurePlot$ProcPerformed <- factor(ProcedurePlot$ProcPerformed, levels = ProcedurePlot$ProcPerformed[order(ProcedurePlot$freq)])
#Do the chart for the biopsies using only the ones that are across all the endoscopist (from MeanNumBx)

ccByPerson$Reason<-gsub("\n","",ccByPerson$Reason)
ccByPerson<-data.frame(ccByPerson[which(ccByPerson$Reason %in% MeanNumBx$Reason),])


matPoss<-xtabs(mean~Reason+AB,data=ccByPerson) 


############################# Unit monthly GRS  ########################################################## 
#MyColonData<-Self[grep("Colonoscopy",Self$Endo_ResultName),]
#Make it diagnostic only:
#MyColonData<-MyColonData[!grepl(".*Therapeutic.*",MyColonData$ProcPerformed),]
#FinalTable<-GRS_Type_Assess_By_Unit(MyColonData)

#Unit_GRS_Means<-colMeans(FinalTable[,2:ncol(FinalTable)],na.rm=T)

#Mean for the whole unit by indication (to compare with individuals in their individual graphs)



############################# Faceted case mix ########################################################## 


ProcedurePlot2<-Self %>% 
  group_by(ProcPerformed,Endo_Endoscopist) %>%
  summarise(freq=n()) 
p=ggplot(data=ProcedurePlot2,aes(x=ProcPerformed,freq,fill=Endo_Endoscopist))+
  geom_bar(stat="identity")+
  #coord_polar(theta="y")+
  theme(legend.position="none")+
  theme(axis.text.x=element_text(angle=-90))

# 
# #Seperate out the diagnostics
# Gastroscopy<-ProcedurePlot2[grepl("Gastroscopy \\(OGD\\)$",ProcedurePlot2$ProcPerformed),]
# Gastroscopy<-ProcedurePlot2[grepl("Gastroscopy",ProcedurePlot2$ProcPerformed),]
# GastroscoptTherap<-Gastroscopy[!grepl("Gastroscopy \\(OGD\\)$",Gastroscopy$ProcPerformed),]
# ERCP<-ProcedurePlot2[grepl("ERCP",ProcedurePlot2$ProcPerformed),]
# LowerOscopy<-ProcedurePlot2[grepl("Colonoscopy",ProcedurePlot2$ProcPerformed)|grepl("Flexi",ProcedurePlot2$ProcPerformed),]

rmarkdown::render("S:\\Gastroenterology\\Seb\\R\\Scripts\\SelfAudit\\Self_Unit.Rmd",output_file=paste("MonthlyEndoscopyUnitReport",Sys.Date(),'.docx'))





#Do a facet plot of all the endoscopies done by each person by day for direct comparison

######################################Whole Unit path diagnosis rate for use in mydf and final report######################################################
DxlstUnit<-as.list(split(Self,Self$Endo_ResultName))

ProcDiagnosisUnit<-function(n,DxlstUnit) {
  #Note you pass the names of the dataframes in the list to the function and then you extract the dataframe from the list. Each dataframe should be a procedure
  this_is_a_name <- n  
  this_is_my_data <- DxlstUnit[[n]]
  
  #The corpus to cover both path and endoscopy
  
  ifelse(grepl("Flexible Sigmoidoscopy$",this_is_a_name,perl=TRUE),myNotableWords<-c("[Cc]olitis","[Cc]rohn","[Hh]yperplastic","[Ii]nflammation","[Ii]schaemic","[Mm]icroscopic"),
         ifelse(grepl("Gastroscopy \\(OGD\\)$",this_is_a_name,perl=TRUE),myNotableWords<-c("[Bb]arrett","[Cc]andida","[Cc]oeliac","[Ee]osinophilic","[Pp]eptic"),
                ifelse(grepl("Colonoscopy$",this_is_a_name,perl=TRUE),myNotableWords<-c("[Cc]olitis","[Cc]rohn","[Hh]yperplastic","[Ii]nflammation","[Ii]schaemic","[Mm]icroscopic"),
                       #ifelse(grepl("[Ee]RCP",this_is_a_name,perl=TRUE),myNotableWords<-c("[Ss]tone","[Ss]tricture","[Cc]arcin","[Ii]nflam"),
                       #ifelse(grepl("EUS",this_is_a_name,perl=TRUE),myNotableWords<-c("[Ss]tone","[Ss]tricture","[Cc]arcin","[Ii]nflam"),
                       #ifelse(grepl("PEG",this_is_a_name,perl=TRUE),myNotableWords<-c("[Ss]tone","[Ss]tricture","[Cc]arcin","[Ii]nflam"),
                       myNotableWords<-c("Nothing"))))
  
  if(myNotableWords!="Nothing"){
    DirectedDocumentationNoGraph(this_is_a_name,this_is_my_data,"Dx",myNotableWords)
    
  }
}


mydf<-lapply(names(DxlstUnit), ProcDiagnosisUnit,DxlstUnit=DxlstUnit) 
names(mydf)<-c("Colonoscopy","ERCP","nil","Flexible Sigmoidoscopy","Gastroscopy (OGD)")


#Need to change this so get outputs for everyone
#x <- "dfMsHarrietWatson"
#This function extracts all the metrics for the report. Firstly it extracts the presenting complaints, the indications vs average number of biopsies taken and the paper clinic otuput




ReportOP<-function(x,Dxlst,mydf){
  #This is to get the name of the dataframe 
  this_is_a_name <- x; 
  print(this_is_a_name)
  #This is to make sure that I get the data from the dataframe as I am passing the names into the function
  x <- Dxlst[[x]] 
  
  
  #Number of endoscopies done over time
  x<-x %>%
    mutate(date=as.Date(format(x[["Endo_ResultEntered"]], "%Y/%m/%d")))
  
  x$ProcPerformed<-as.character(x[["ProcPerformed"]])
  counts<-t(table(x[["date"]],x[["ProcPerformed"]]))
  counts_df<-melt(counts,var.id=counts)
  NumDone<-barplot(counts,xlab="Date",main="Procedures performed in the month",ylab="Number procedures",legend=rownames(counts))
  tbl<-table(x[["ProcPerformed"]])
  
  x[["IndicationsFroExamination"]]<-gsub("\\.","\n",x[["IndicationsFroExamination"]])
  
  #BDR<-lapply(names(PClst), BDR_ProcDiagnosis,PClst=PClst,)
  
  Gastroscop<-x[x[["ProcPerformed"]]=="Gastroscopy (OGD)",]
  GastroForBDR<-x[grepl("Gastroscopy (OGD)",x[["ProcPerformed"]],fixed=T),]
  GastroForBDR<-data.frame(GastroForBDR)
  
  try(BDR<-BDR_DirectedDocumentation(GastroForBDR,"Dx"))
  BDR<-as.numeric(BDR)
  
  #BDR<-ifelse(grepl("Gastroscopy (OGD)",x[["IndicationsFroExamination"]],fixed=T),BDR_ProcDiagnosis(names(PClst),PClst),"")
  
  
  ################################################################################################################################ 
  ################################################################################################################################ 
  ################################################################################################################################ 
  ######################## Type of presenting complaint   ######################################################################## 
  ################################################################################################################################
  ################################################################################################################################ 
  #Do graph here of presenting complaints
  
  
  ################################################################################################################################ 
  ################################################################################################################################ 
  ################################################################################################################################ 
  ######################## Adenoma detection rate   ############################################################################## 
  ################################################################################################################################
  ################################################################################################################################ 
  
  Colons<-x[grep("Colonoscopy.*",x[["ProcPerformed"]]),]
  ColonsAdr<-Colons[grep(".*[Aa]denom.*",Colons$Dx),]
  ADR<-(nrow(ColonsAdr)/nrow(Colons))*100
  
  ################################################################################################################################ 
  ################################################################################################################################ 
  ################################################################################################################################ 
  ######################## Diagnoses made by endoscopy type   #################################################################### 
  ################################################################################################################################
  ################################################################################################################################ 
  
  #This is how to split into the different endoscopies
  PClst<-split(x,x$ProcPerformed)
  #names(PClst)<-paste0('df',gsub("\\s+","",names(PClst)))
  #mydf
  z<-lapply(names(PClst), ProcDiagnosis,PClst=PClst,mydf=mydf)
  
  #Get rid of null lists which mess up grid.arrange
  z<-z[lapply(z,length)>0]   
  
  ################################################################################################################################ 
  ################################################################################################################################ 
  ################################################################################################################################ 
  #########################Number of biopsies vs indication  ##################################################################### 
  ################################################################################################################################
  ################################################################################################################################ 
  
  
  x[["IndicationsFroExamination"]]<-gsub("PROCEDURE PERFORMED","",x[["IndicationsFroExamination"]])
  numbx<-data.frame(x[["IndicationsFroExamination"]],x[["NumbOfBxs"]])
  names(numbx)<-c("IndicationsFroExamination","NumbOfBxs")
  grp <- numbx %>% group_by(IndicationsFroExamination) %>% summarise(mean = mean(NumbOfBxs))
  
  
  
  ########FILTER FOR PURE INDICATIONS
  #Only chronic diarrhoea on its own
  #Only dysphagia on its own
  IndicVsBx<-data.frame(cSplit(x,"IndicationsFroExamination","\n","long")[, .(Avg=mean(NumbOfBxs)),.(IndicationsFroExamination)])
  
  
  s<-strsplit(as.character(x[["IndicationsFroExamination"]]),'\n')
  IndicVsBx<-data.frame(Reason=unlist(s),AB=rep(x[["NumbOfBxs"]],sapply(s,FUN=length)))
  IndicVsBx$Reason<-as.character(IndicVsBx$Reason)
  #View(IndicVsBx)
  
  IndicVsBx2<-IndicVsBx %>%
    group_by(Reason) %>%
    dplyr::summarise(mean=mean(AB,na.rm=T))
  
  
  IndicVsBx2<-IndicVsBx2[!is.na(IndicVsBx2$mean),]
  #merge with the relevant fromt eh unit averages from
  #View(IndicVsBx2)
  
  IndicVsBx2<-IndicVsBx2[!grepl("\n$",IndicVsBx2$Reason),]
  IndicVsBx2$Reason<-gsub("\n ","",IndicVsBx2$Reason)
  IndicVsBx2$Reason<-gsub("\n","",IndicVsBx2$Reason)
  IndicVsBx3<-inner_join(MeanNumBx,IndicVsBx2,by=c("Reason"))
  IndicVsBx3<-IndicVsBx3[!is.na(IndicVsBx3$mean),]
  IndicVsBx3<-IndicVsBx3[IndicVsBx3$Reason!="",]
  ################################################################################################################################ 
  ################################################################################################################################ 
  ################################################################################################################################ 
  ####################A##### Separate out the diagnoses and create a table for each one ########################################### 
  ################################################################################################################################
  ################################################################################################################################  
  
  #Build a heatmap of each Indication and diagnosis on biopsy:# Ditch this for now in the final report but maybe use later
  
  
  #Splits the frame so each indication has its own row
  try(dfByIndicPerRow<-data.frame(cSplit(x,"IndicationsFroExamination","\n","long")))
  #PClst<-split(dfByIndicPerRow,dfByIndicPerRow$Endo_ResultName)
  try(PClst<-split(dfByIndicPerRow,dfByIndicPerRow$IndicationsFroExamination))
  try(zz<-lapply(names(PClst), ProcDiagnosis2,PClst=PClst))  
  try(names(zz)<-names(PClst))
  try(zz<-data.frame(Filter(Negate(function(x) is.null(unlist(x))),zz)))
  try(rownames(zz)<-zz[,1])
  try(zzz<-zz[grep("Prop",colnames(zz))])
  try(colnames(zzz)<-gsub("\\.Prop","",colnames(zzz)))
  try(colnames(zzz)<-gsub("\\."," ",colnames(zzz)))
  try(zzz$Dx<-rownames(zzz))
  try(zzzLong<-melt(zzz,id=c("Dx")))
  try(myzzzMatrix<-xtabs(value~Dx+variable,data=zzzLong))
  try(print(myzzzMatrix))
  
  
  Cancertbl<-data.frame(x[!is.na(x[["Cancer"]]),])
  Cancertbl<-data.frame(Cancertbl["PatientID"],Cancertbl["PatientName"],Cancertbl["Findings"],Cancertbl["DxRaw"])
  Dysplasiatbl<-x[!is.na(x[["Dysplasia"]]),]
  Dysplasiatbl<-data.frame(Dysplasiatbl["PatientID"],Dysplasiatbl["PatientName"],Dysplasiatbl["Findings"],Dysplasiatbl["DxRaw"])
  
  
  #######This is where to merge the AllTbl with the image table so you can see the images themselves  #######  #######  #######
  #######  #######  #######  #######  #######  #######  #######  #######  #######  #######  #######  #######  #######  #######
  
  
  
  x<-x[!is.na(x$Histol),]
  row.names(x) <- NULL
  Alltbl<-data.frame(x["PatientID"],x["PatientName"],x["Findings"],x["DxRaw"],x["url"])
  #Alltbl<-x
  Alltbl$ID<-paste(apply( Alltbl[1:2]  , 1 , paste , collapse = " ",sep="\n" ))
  Alltbl$ID<-gsub(",",",\n",Alltbl$ID)
  Alltbl<-data.frame(Alltbl["ID"],Alltbl["Findings"],Alltbl["DxRaw"],Alltbl["url"])
  try(FinalTable2<-GRS_Type_Assess_By_Unit(x))
  
  
  rmarkdown::render("S:\\Gastroenterology\\Seb\\R\\Scripts\\SelfAudit\\Self.Rmd",params=list(this_is_a_name),output_file=paste(this_is_a_name,"report",Sys.Date(),'.docx'))
  print("done")
}

#This gets the unit's figures as a whole
#To get the units pathological diagnosis rate by each type of endoscopy:
lapply(names(Dxlst),ReportOP,Dxlst=Dxlst,mydf=mydf)

#To do:
#Conclusion