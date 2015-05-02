require(rJava)
require(googleVis)
library(reshape)
library(shiny)
library(reldist)

mydata=read.table("data.csv",header=TRUE,sep=",")
mydata$M_A_DATE=as.Date(mydata$M_A_DATE,origin="1899-12-31")
mydata$EDC_DATE=as.Date(mydata$EDC_DATE,origin="1899-12-31")
today=as.Date("2013/11/07","%Y/%m/%d")
year_start=as.Date("2013/01/01","%Y/%m/%d")

################################Setting up data for first chart#########################################
tab1=mydata[,c("REGION","CATEGORY","TYPE","M_A_DATE","MARCHANT","NO_EDC","EDC_DATE")]

################################Setting up data for Second and third chart chart#########################################
M_Month=format(mydata$M_A_DATE,"%B")
E_Month=format(mydata$EDC_DATE,"%B")
tab2=data.frame(mydata[,c("REGION","CATEGORY","TYPE","M_A_DATE","MARCHANT","NO_EDC","EDC_DATE")],M_Month,E_Month)

################################Setting up data for fourth and fifth chart#########################################
tab3=data.frame(mydata[,c("REGION","CATEGORY","TYPE","M_A_DATE","MARCHANT","NO_EDC","EDC_DATE","MANAGER")],M_Month,E_Month)

################################Setting up data for intensification first chart#########################################
tab4=data.frame(mydata[,],M_Month,E_Month)

################################Setting up data for intensification second and third chart#########################################
tab5=read.table("data1.csv",header=TRUE,sep=",")
tab5=data.frame(tab5)

################################Setting up data for intensification fourth chart#########################################
tab6=read.table("data2.csv",header=TRUE,sep=",")
tab6$Date=as.Date(tab6$Date,origin="1899-12-31")
tab6=data.frame(tab6)

################################Setting up data for actionable first chart#########################################
tab7=read.table("data3.csv",header=TRUE,sep=",")
tab7=data.frame(tab7)

################################Setting up data for actionable second chart#########################################
tab8=read.table("data4.csv",header=TRUE,sep=",")
tab8=data.frame(tab8)