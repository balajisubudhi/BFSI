library(shiny)
library(datasets)
library(fmsb)
library(plyr)

# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {
###############################################################################################################

#RENDERING Extensification TAB1

###############################################################################################################
requiredata = reactive({
	a=subset(tab1, CATEGORY %in% input$category & TYPE %in% input$type)
	a <- droplevels(a)
	return(a)
	})
output$Ex1<- renderGvis({
	rdata1=subset(requiredata(),M_A_DATE>=year_start,c(M_A_DATE,MARCHANT,NO_EDC,EDC_DATE,REGION))
	month=unique(format(rdata1$M_A_DATE,"%B"))
	MONTH=c("January","February","March","April","May","June","July","August","September","October","November","December")
	MONTH=subset(MONTH,MONTH %in% month)
	Merchant=0
	EDC=0
	i=1
	while(i<=length(MONTH))
	{
		Merchant[i]=length(unique(subset(rdata1,format(rdata1$M_A_DATE,"%B")==MONTH[i],MARCHANT))[,1])
		EDC[i]=sum(subset(rdata1,format(rdata1$EDC_DATE,"%B")==MONTH[i],NO_EDC)[,1])
		i=i+1
	}
	data1=data.frame(MONTH,Merchant,EDC)
	E1 = gvisBarChart(data1,xvar="MONTH", yvar=c("Merchant", "EDC"), options=list(width=750, height=250),chartid="E1")
	data1=array(NA,c(23,length(MONTH)+3))
	data1[1,1]="Region"
	data1[2,1]="Region1"
	data1[4,1]="Region2"
	data1[6,1]="Region3"
	data1[8,1]="Region4"
	data1[10,1]="Region5"
	data1[12,1]="Region6"
	data1[14,1]="Region7"
	data1[16,1]="Region8"
	data1[18,1]="Region9"
	data1[20,1]="Region10"
	data1[22,1]="TOTAL"
	data1[1,2]="VALUE"
	data1[1,3]="YTD"
	data1[1,4:14]=MONTH
	data1[2:23,2]=c("Merchant Acquired","Target Achieved",
					"Merchant Acquired","Target Achieved",
					"Merchant Acquired","Target Achieved",
					"Merchant Acquired","Target Achieved",
					"Merchant Acquired","Target Achieved",
					"Merchant Acquired","Target Achieved",
					"Merchant Acquired","Target Achieved",
					"Merchant Acquired","Target Achieved",
					"Merchant Acquired","Target Achieved",
					"Merchant Acquired","Target Achieved",
					"Merchant Acquired","Target Achieved")
	i=2
	while(i<=21)
	{
		j=3
		while(j<=length(data1))
		{
			if(j==3)
			{
				if(i%%2==0)
				{
					data1[i,j]=length(unique(subset(rdata1,REGION==data1[i,1],MARCHANT))[,1])
				}
				else
				{
					data1[i,j]=paste(round(as.numeric(data1[i-1,j])/60*100),"%",sep="")
				}
			}
			else if(j>=4 && j<=14)
			{
				if(i%%2==0)
				{
					data1[i,j]=length(unique(subset(rdata1,REGION==data1[i,1] & format(rdata1$M_A_DATE,"%B")==data1[1,j],MARCHANT))[,1])
				}
				else
				{
					data1[i,j]=paste(round(as.numeric(data1[i-1,j])/5*100),"%",sep="")
				}
			}
			j=j+1
		}
		i=i+1
	}
	while(i<=23)
	{
		j=3
		while(j<=length(data1))
		{
			if(j==3)
			{
				if(i%%2==0)
				{
					data1[i,j]=sum(as.numeric(data1[2,j]),as.numeric(data1[4,j]),as.numeric(data1[6,j]),as.numeric(data1[8,j]),as.numeric(data1[10,j]),
									as.numeric(data1[12,j]),as.numeric(data1[14,j]),as.numeric(data1[16,j]),as.numeric(data1[18,j]),as.numeric(data1[20,j]))
				}
				else
				{
					data1[i,j]=paste(round(as.numeric(data1[i-1,j])/600*100),"%",sep="")
				}
			}
			else if(j>=4 && j<=14)
			{
				if(i%%2==0)
				{
					data1[i,j]=sum(as.numeric(data1[2,j]),as.numeric(data1[4,j]),as.numeric(data1[6,j]),as.numeric(data1[8,j]),as.numeric(data1[10,j]),
									as.numeric(data1[12,j]),as.numeric(data1[14,j]),as.numeric(data1[16,j]),as.numeric(data1[18,j]),as.numeric(data1[20,j]))
				}
				else
				{
					data1[i,j]=paste(round(as.numeric(data1[i-1,j])/50*100),"%",sep="")
				}
			}
			j=j+1
		}
		i=i+1
	}
	
	data1=data.frame(data1)
	data2=data.frame(data1[2:23,])
	name=c("REGION","VALUE","YTD",MONTH)
	colnames(data2)=name
	E2 = gvisTable(data2,options=list(height=200,width=900),chartid="E2")
	
	data1=array(NA,c(23,length(MONTH)+3))
	data1[1,1]="Region"
	data1[2,1]="Region1"
	data1[4,1]="Region2"
	data1[6,1]="Region3"
	data1[8,1]="Region4"
	data1[10,1]="Region5"
	data1[12,1]="Region6"
	data1[14,1]="Region7"
	data1[16,1]="Region8"
	data1[18,1]="Region9"
	data1[20,1]="Region10"
	data1[22,1]="TOTAL"
	data1[1,2]="VALUE"
	data1[1,3]="YTD"
	data1[1,4:14]=MONTH
	data1[2:23,2]=c("EDC Acquired","Target Achieved",
					"EDC Acquired","Target Achieved",
					"EDC Acquired","Target Achieved",
					"EDC Acquired","Target Achieved",
					"EDC Acquired","Target Achieved",
					"EDC Acquired","Target Achieved",
					"EDC Acquired","Target Achieved",
					"EDC Acquired","Target Achieved",
					"EDC Acquired","Target Achieved",
					"EDC Acquired","Target Achieved",
					"EDC Acquired","Target Achieved")
	i=2
	while(i<=21)
	{
		j=3
		while(j<=length(data1))
		{
			if(j==3)
			{
				if(i%%2==0)
				{
					data1[i,j]=sum(subset(rdata1,REGION==data1[i,1],NO_EDC)[,1])
				}
				else
				{
					data1[i,j]=paste(round(as.numeric(data1[i-1,j])/120*100),"%",sep="")
				}
			}
			else if(j>=4 && j<=14)
			{
				if(i%%2==0)
				{
					data1[i,j]=sum(subset(rdata1,REGION==data1[i,1] & format(rdata1$M_A_DATE,"%B")==data1[1,j],NO_EDC)[,1])
				}
				else
				{
					data1[i,j]=paste(round(as.numeric(data1[i-1,j])/10*100),"%",sep="")
				}
			}
			j=j+1
		}
		i=i+1
	}
	while(i<=23)
	{
		j=3
		while(j<=length(data1))
		{
			if(j==3)
			{
				if(i%%2==0)
				{
					data1[i,j]=sum(as.numeric(data1[2,j]),as.numeric(data1[4,j]),as.numeric(data1[6,j]),as.numeric(data1[8,j]),as.numeric(data1[10,j]),
									as.numeric(data1[12,j]),as.numeric(data1[14,j]),as.numeric(data1[16,j]),as.numeric(data1[18,j]),as.numeric(data1[20,j]))
				}
				else
				{
					data1[i,j]=paste(round(as.numeric(data1[i-1,j])/600*100),"%",sep="")
				}
			}
			else if(j>=4 && j<=14)
			{
				if(i%%2==0)
				{
					data1[i,j]=sum(as.numeric(data1[2,j]),as.numeric(data1[4,j]),as.numeric(data1[6,j]),as.numeric(data1[8,j]),as.numeric(data1[10,j]),
									as.numeric(data1[12,j]),as.numeric(data1[14,j]),as.numeric(data1[16,j]),as.numeric(data1[18,j]),as.numeric(data1[20,j]))
				}
				else
				{
					data1[i,j]=paste(round(as.numeric(data1[i-1,j])/50*100),"%",sep="")
				}
			}
			j=j+1
		}
		i=i+1
	}
	
	data1=data.frame(data1)
	data2=data.frame(data1[2:23,])
	name=c("REGION","VALUE","YTD",MONTH)
	colnames(data2)=name
	E3 = gvisTable(data2,options=list(height=200,width=900),chartid="E3")
	
	E1E2=gvisMerge(E1,E2,horizontal=FALSE,chartid="E1E2")
	E1E2E3=gvisMerge(E1E2,E3,horizontal=FALSE,chartid="E1E2E3")
	E1E2E3$html$chart["jsData"]=gsub("[_]"," ",E1E2E3$html$chart["jsData"])
	E1E2E3$html$chart["jsDrawChart"]= "\n// jsDrawChart\nfunction drawChartE1() {
										\nvar data = gvisDataE1();
										\nvar options = {};
										\noptions[\"allowHtml\"] = true;
										\noptions[\"width\"] =    750;
										\noptions[\"height\"] =    250;
										\noptions[\"colors\"] = [\"#FF4000\",\"#35FD04\"];
										\noptions[\"legend\"] = {position: \"bottom\", textStyle: {color: \"black\", fontSize: 10}};
										\noptions[\"series\"] = [{type:\"bar\",targetAxisIndex: 0},{targetAxisIndex:1}];
										\noptions[\"chartArea\"]={left:40,top:20,width:\"80%\",height:\"65%\"};
										\noptions[\"hAxis\"]={textStyle: {color: 'black'}};
										\noptions[\"vAxis\"]={textStyle: {color: 'black'}};
										\noptions[\"titleTextStyle\"]={color: 'black'};
										\n\n\n    var chart = new google.visualization.ColumnChart(
										\n    document.getElementById('E1')\n    );
										\n    chart.draw(data,options);\n    \n\n}\n  \n\n
										
										\n// jsDrawChart\nfunction drawChartE2() {
										\nvar data = gvisDataE2();
										\nvar options = {};
										\noptions[\"allowHtml\"] = true;
										\noptions[\"height\"] =    400;
										\noptions[\"width\"] =    940;
										\n\n\n    var chart = new google.visualization.Table(
										\n    document.getElementById('E2')\n    );
										\n    chart.draw(data,options);\n    \n\n}\n  \n\n
										
										\n// jsDrawChart\nfunction drawChartE3() {
										\nvar data = gvisDataE3();
										\nvar options = {};
										\noptions[\"allowHtml\"] = true;
										\noptions[\"height\"] =    400;
										\noptions[\"width\"] =    940;
										\n\n\n    var chart = new google.visualization.Table(
										\n    document.getElementById('E3')\n    );
										\n    chart.draw(data,options);\n    \n\n}\n  \n" 
	E1E2E3$html$chart["divChart"]="\n<table border=\"0\">
									\n<tr>\n<td>\n\n<table border=\"0\">\n<tr>
									\n<td>\n\n<!-- divChart -->\n 
									\n<h3>Merchant V/s EDC</h3>\n
									\n<div id=\"E1\"
									\n  style=\"width: 750px; height: 250px;\">
									
									\n</div>\n\n</td>\n</tr>\n<tr>
									\n<td>\n\n<!-- divChart -->\n
									\n<h3>Merchant Acquired</h3>\n
									\n<div id=\"E2\"
									\n  style=\"width: 940px; height: 380px;\">
									
									\n</div>\n\n</td>\n</tr>\n</table>\n\n</td>\n</tr>\n<tr>\n<td>\n
									\n<!-- divChart -->\n  
									\n<h3>EDC Acquired</h3>\n
									\n<div id=\"E3\"
									\n  style=\"width: 940px; height: 380px;\">
									\n</div>\n\n</td>\n</tr>\n</table>\n" 
	
	return(E1E2E3)
	})

###############################################################################################################

#RENDERING Extensification TAB2

###############################################################################################################
requiredata1 = reactive({
	a=subset(tab2, TYPE %in% input$type2 & E_Month %in% input$period2)
	a <- droplevels(a)
	return(a)
	})
	
output$Ex2<- renderGvis({
	rdata1=subset(requiredata1(),M_A_DATE>=year_start,c(M_A_DATE,MARCHANT,NO_EDC,EDC_DATE,REGION,CATEGORY))
	Category=c("Retail Store","Kirana Store","Hotels","Restaurants","Airline","Hardware Store","Computer Store","Gaming Zone","Super Market","Fast Food Centre")
	Merchant=0
	EDC=0
	i=1
	while(i<=length(Category))
	{
		Merchant[i]=length(unique(subset(rdata1,CATEGORY==Category[i],MARCHANT))[,1])
		EDC[i]=sum(subset(rdata1,CATEGORY==Category[i],NO_EDC)[,1])
		i=i+1
	}
	data1=data.frame(Category,Merchant,EDC)
	E1 = gvisBarChart(data1,xvar="Category", yvar=c("Merchant", "EDC"), options=list(width=750, height=250),chartid="E4")
	data1=array(NA,c(11,11))
	data1[1,1]="Region"
	data1[2,1]="Region1"
	data1[3,1]="Region2"
	data1[4,1]="Region3"
	data1[5,1]="Region4"
	data1[6,1]="Region5"
	data1[7,1]="Region6"
	data1[8,1]="Region7"
	data1[9,1]="Region8"
	data1[10,1]="Region9"
	data1[11,1]="Region10"
	data1[1,2:11]=c("Retail Store","Kirana Store","Hotels","Restaurants","Airline","Hardware Store","Computer Store","Gaming Zone","Super Market","Fast Food Centre")
	
	i=2
	while(i<=11)
	{
		j=2
		while(j<=11)
		{		
			data1[i,j]=length(unique(subset(rdata1,CATEGORY==data1[1,j] & REGION==data1[i,1],MARCHANT))[,1])
			j=j+1
		}
		i=i+1
	}
	data1=data.frame(data1)
	data2=data.frame(data1[2:11,])
	name=c("REGION",Category)
	colnames(data2)=name
	E2 = gvisTable(data2,options=list(height=200,width=900),chartid="E5")
	
	data1=array(NA,c(11,11))
	data1[1,1]="Region"
	data1[2,1]="Region1"
	data1[3,1]="Region2"
	data1[4,1]="Region3"
	data1[5,1]="Region4"
	data1[6,1]="Region5"
	data1[7,1]="Region6"
	data1[8,1]="Region7"
	data1[9,1]="Region8"
	data1[10,1]="Region9"
	data1[11,1]="Region10"
	data1[1,2:11]=c("Retail Store","Kirana Store","Hotels","Restaurants","Airline","Hardware Store","Computer Store","Gaming Zone","Super Market","Fast Food Centre")
	
	i=2
	while(i<=11)
	{
		j=2
		while(j<=11)
		{		
			data1[i,j]=sum(subset(rdata1,CATEGORY==data1[1,j] & REGION==data1[i,1],NO_EDC)[,1])
			j=j+1
		}
		i=i+1
	}
	data1=data.frame(data1)
	data2=data.frame(data1[2:11,])
	name=c("REGION",Category)
	colnames(data2)=name
	E3 = gvisTable(data2,options=list(height=200,width=900),chartid="E6")
	
	E1E2=gvisMerge(E1,E2,horizontal=FALSE,chartid="E4E5")
	E1E2E3=gvisMerge(E1E2,E3,horizontal=FALSE,chartid="E4E5E6")
	E1E2E3$html$chart["jsData"]=gsub("[_]"," ",E1E2E3$html$chart["jsData"])
	E1E2E3$html$chart["jsDrawChart"]="\n// jsDrawChart\nfunction drawChartE4() {
										\nvar data = gvisDataE4();
										\nvar options = {};
										\noptions[\"allowHtml\"] = true;
										\noptions[\"width\"] =    750;
										\noptions[\"height\"] =    250;
										\noptions[\"colors\"] = [\"#FF4000\",\"#35FD04\"];
										\noptions[\"legend\"] = {position: \"bottom\", textStyle: {color: \"black\", fontSize: 10}};
										\noptions[\"series\"] = [{type:\"bar\",targetAxisIndex: 0},{targetAxisIndex:1}];
										\noptions[\"chartArea\"]={left:40,top:20,width:\"80%\",height:\"65%\"};
										\noptions[\"hAxis\"]={textStyle: {color: 'black'}};
										\noptions[\"vAxis\"]={textStyle: {color: 'black'}};
										\n\n\n    var chart = new google.visualization.ColumnChart(
										\n    document.getElementById('E4')\n    );
										\n    chart.draw(data,options);\n    \n\n}\n  \n\n
										
										\n// jsDrawChart\nfunction drawChartE5() {
										\nvar data = gvisDataE5();
										\nvar options = {};
										\noptions[\"allowHtml\"] = true;
										\noptions[\"height\"] =    300;
										\noptions[\"width\"] =    900;
										\n\n\n    var chart = new google.visualization.Table(
										\n    document.getElementById('E5')\n    );
										\nvar formatter = new google.visualization.ColorFormat();
										\nformatter.addGradientRange(0, 12, 'black', 'orange','green');
										\nformatter.format(data, 1); // Apply formatter to 2nd column
										\nformatter.format(data, 2); // Apply formatter to 3rd column
										\nformatter.format(data, 3); // Apply formatter to 4th column
										\nformatter.format(data, 4); // Apply formatter to 5th column
										\nformatter.format(data, 5); // Apply formatter to 6th column
										\nformatter.format(data, 6); // Apply formatter to 7th column
										\nformatter.format(data, 7); // Apply formatter to 8th column
										\nformatter.format(data, 8); // Apply formatter to 9th column
										\nformatter.format(data, 9); // Apply formatter to 10th column
										\nformatter.format(data, 10); // Apply formatter to 11th column
										\n    chart.draw(data,options);\n    \n\n}\n  \n\n
										
										\n// jsDrawChart\nfunction drawChartE6() {
										\nvar data = gvisDataE6();
										\nvar options = {};
										\noptions[\"allowHtml\"] = true;
										\noptions[\"height\"] =    300;
										\noptions[\"width\"] =    900;
										\n\n\n    var chart = new google.visualization.Table(
										\n    document.getElementById('E6')\n    );
										\nvar formatter = new google.visualization.ColorFormat();
										\nformatter.addGradientRange(0, 215, 'black', 'orange','green');
										\nformatter.format(data, 1); // Apply formatter to 2nd column
										\nformatter.format(data, 2); // Apply formatter to 3rd column
										\nformatter.format(data, 3); // Apply formatter to 4th column
										\nformatter.format(data, 4); // Apply formatter to 5th column
										\nformatter.format(data, 5); // Apply formatter to 6th column
										\nformatter.format(data, 6); // Apply formatter to 7th column
										\nformatter.format(data, 7); // Apply formatter to 8th column
										\nformatter.format(data, 8); // Apply formatter to 9th column
										\nformatter.format(data, 9); // Apply formatter to 10th column
										\nformatter.format(data, 10); // Apply formatter to 11th column
										\n    chart.draw(data,options);\n    \n\n}\n  \n" 
	E1E2E3$html$chart["divChart"]="\n<table border=\"0\">\n<tr>
									\n<td>\n\n<table border=\"0\">\n<tr>
									
									\n<td>\n\n<!-- divChart -->\n  
									\n<h3>Merchant V/s EDC</h3>\n
									\n<div id=\"E4\"\n  style=\"width: 750px; height: 250px;\">
									\n</div>\n\n</td>\n</tr>\n<tr>
									
									\n<td>\n\n<!-- divChart -->\n
									\n<h3>Merchant V/s EDC</h3>\n									
									\n<div id=\"E5\"\n  style=\"width: 900px; height: 280px;\">
									\n</div>\n\n</td>\n</tr>\n</table>\n\n</td>\n</tr>\n<tr>
									
									\n<td>\n\n<!-- divChart -->\n
									\n<h3>Merchant V/s EDC</h3>\n
									\n<div id=\"E6\"\n  style=\"width: 900px; height: 280px;\">
									\n</div>\n\n</td>\n</tr>\n</table>\n" 
	return(E1E2E3)
	})

###############################################################################################################

#RENDERING Extensification TAB3

###############################################################################################################
requiredata2 = reactive({
	a=subset(tab2, TYPE %in% input$type & CATEGORY %in% input$category & REGION %in% input$region)
	a <- droplevels(a)
	return(a)
	})
	
output$Ex3<- renderPlot({
	rdata1=subset(requiredata2(),M_A_DATE>=year_start,c(M_A_DATE,MARCHANT,NO_EDC,EDC_DATE,REGION,CATEGORY))
	Net_Addition_EDC=sum(rdata1[,"NO_EDC"])/sum(subset(rdata1,M_A_DATE<as.Date("31-10-2013","%d-%m-%Y"),NO_EDC)[,1])
	Net_Addition_Merchant=length(unique(rdata1[,"MARCHANT"]))/length(unique(subset(rdata1,M_A_DATE<as.Date("31-10-2013","%d-%m-%Y"),MARCHANT)[,1]))
	Acq_Momentum_EDC=sum(subset(rdata1,format(M_A_DATE,"%B")=="November",NO_EDC)[,1])/sum(subset(rdata1,format(M_A_DATE,"%B")=="October",NO_EDC)[,1])
	Acq_Momentum_Merchant=length(unique(subset(rdata1,format(M_A_DATE,"%B")=="November",MARCHANT)[,1]))/length(unique(subset(rdata1,format(M_A_DATE,"%B")=="October",MARCHANT)[,1]))
	data1=data.frame(Net_Addition_EDC,Net_Addition_Merchant,Acq_Momentum_EDC,Acq_Momentum_Merchant)
	Net_Addition_EDC=c(0,2)
	Net_Addition_Merchant=c(0,2)
	Acq_Momentum_EDC=c(0,1)
	Acq_Momentum_Merchant=c(0,1)
	maxmin=data.frame(Net_Addition_EDC,Net_Addition_Merchant,Acq_Momentum_EDC,Acq_Momentum_Merchant)
	data1=rbind(maxmin,data1)
	return(radarchart(data1, axistype=2, pcol=topo.colors(3), plty=1, pdensity=30, pfcol=topo.colors(3),title="Radar Chart"))
	})

###############################################################################################################

#RENDERING Extensification TAB4

###############################################################################################################
requiredata3 = reactive({
	a=subset(tab3, TYPE %in% input$type4 & CATEGORY %in% input$category4 & REGION %in% input$region4)
	a <- droplevels(a)
	return(a)
	})
	
output$Ex4<- renderGvis({
	rdata1=subset(requiredata3(),M_A_DATE>=year_start,c(M_A_DATE,MARCHANT,NO_EDC,EDC_DATE,REGION,CATEGORY,MANAGER))
	rdata2=subset(tab3,M_A_DATE>=year_start,c(M_A_DATE,MARCHANT,NO_EDC,EDC_DATE,REGION,CATEGORY,MANAGER))
	Managers=unique(rdata1$MANAGER)
	month=unique(format(rdata2$M_A_DATE,"%B"))
	MONTH=c("January","February","March","April","May","June","July","August","September","October","November","December")
	MONTH=subset(MONTH,MONTH %in% month)
	Marchant=0
	EDC=0
	data1=array(0,c(length(Managers)+1,length(MONTH)+1))
	data1[1,1]="Acquisition Manager"
	data1[2:(length(Managers)+1),1]=as.character(Managers)
	data1[1,2:(length(MONTH)+1)]=MONTH
	i=2
	while(i<length(data1[,1]))
	{
		j=2
		while(j<=length(data1[1,]))
		{
			data1[i,j]=length(unique(subset(rdata1,format(rdata1$M_A_DATE,"%B")==data1[1,j] & MANAGER==as.character(data1[i,1]),MARCHANT))[,1])
			j=j+1
		}
		i=i+1
	}
	data1=data.frame(data1)
	data2=data.frame(data1[2:length(data1[,1]),])
	name=c("Acquisition Manager",MONTH)
	colnames(data2)=name
	E1 = gvisTable(data2,options=list(height=200,width=900),chartid="E7")
	
	data1=array(0,c(length(Managers)+1,length(MONTH)+1))
	data1[1,1]="Acquisition Manager"
	data1[2:(length(Managers)+1),1]=as.character(Managers)
	data1[1,2:(length(MONTH)+1)]=MONTH
	i=2
	while(i<length(data1[,1]))
	{
		j=2
		while(j<=length(data1[1,]))
		{
			data1[i,j]=sum(subset(rdata1,format(rdata1$M_A_DATE,"%B")==data1[1,j] & MANAGER==as.character(data1[i,1]),NO_EDC)[,1])
			j=j+1
		}
		i=i+1
	}
		data1=data.frame(data1)
	data2=data.frame(data1[2:length(data1[,1]),])
	name=c("Acquisition Manager",MONTH)
	colnames(data2)=name
	E2 = gvisTable(data2,options=list(height=200,width=900),chartid="E8")
	E1E2=gvisMerge(E1,E2,horizontal=FALSE,chartid="E7E8")
	E1E2$html$chart["jsData"]=gsub("[_]"," ",E1E2$html$chart["jsData"])
	E1E2$html$chart["jsDrawChart"]="\n// jsDrawChart\nfunction drawChartE7() {
									\nvar data = gvisDataE7();
									\nvar options = {};
									\noptions[\"allowHtml\"] = true;
									\noptions[\"height\"] =    300;
									\noptions[\"width\"] =    900;
									\n\n\n    var chart = new google.visualization.Table(
									\ndocument.getElementById('E7')\n    );
									\nvar formatter = new google.visualization.ColorFormat();
									\nformatter.addGradientRange(0, 6, 'black', 'orange','green');
									\nformatter.format(data, 1); // Apply formatter to 2nd column
									\nformatter.format(data, 2); // Apply formatter to 3rd column
									\nformatter.format(data, 3); // Apply formatter to 4th column
									\nformatter.format(data, 4); // Apply formatter to 5th column
									\nformatter.format(data, 5); // Apply formatter to 6th column
									\nformatter.format(data, 6); // Apply formatter to 7th column
									\nformatter.format(data, 7); // Apply formatter to 8th column
									\nformatter.format(data, 8); // Apply formatter to 9th column
									\nformatter.format(data, 9); // Apply formatter to 10th column
									\nformatter.format(data, 10); // Apply formatter to 11th column
									\nformatter.format(data, 11); // Apply formatter to 12th column
									\n    chart.draw(data,options);\n    \n\n}\n  \n\n
									
									\n// jsDrawChart\nfunction drawChartE8() {
									\nvar data = gvisDataE8();
									\nvar options = {};
									\noptions[\"allowHtml\"] = true;
									\noptions[\"height\"] =    300;
									\noptions[\"width\"] =    900;
									\n\n\n    var chart = new google.visualization.Table(
									\n    document.getElementById('E8')\n    );
									\nvar formatter = new google.visualization.ColorFormat();
									\nformatter.addGradientRange(0, 105, 'black', 'orange','green');
									\nformatter.format(data, 1); // Apply formatter to 2nd column
									\nformatter.format(data, 2); // Apply formatter to 3rd column
									\nformatter.format(data, 3); // Apply formatter to 4th column
									\nformatter.format(data, 4); // Apply formatter to 5th column
									\nformatter.format(data, 5); // Apply formatter to 6th column
									\nformatter.format(data, 6); // Apply formatter to 7th column
									\nformatter.format(data, 7); // Apply formatter to 8th column
									\nformatter.format(data, 8); // Apply formatter to 9th column
									\nformatter.format(data, 9); // Apply formatter to 10th column
									\nformatter.format(data, 10); // Apply formatter to 11th column
									\nformatter.format(data, 11); // Apply formatter to 12th column
									\n    chart.draw(data,options);\n    \n\n}\n  \n" 
	
    E1E2$html$chart["divChart"]="\n<table border=\"0\">\n<tr>\n<td>\n
								\n<!-- divChart -->\n
								\n<h3>Merchant Acquired</h3>\n
								\n<div id=\"E7\"
								\n  style=\"width: 900px; height: 280px;\">
								\n</div>\n\n</td>\n</tr>\n<tr>\n<td>\n
								
								\n<!-- divChart -->\n  
								\n<h3>EDC Acquired</h3>\n
								\n<div id=\"E8\"
								\n  style=\"width: 900px; height: 280px;\">
								\n</div>\n\n</td>\n</tr>\n</table>\n" 
	return(E1E2)
	})	

###############################################################################################################

#RENDERING Extensification TAB5

###############################################################################################################
requiredata4 = reactive({
	a=subset(tab3, TYPE %in% input$type5 & CATEGORY %in% input$category5 & REGION %in% input$region5)
	a <- droplevels(a)
	return(a)
	})
	
output$Ex5<- renderGvis({
	rdata1=subset(requiredata4(),M_A_DATE>=year_start,c(M_A_DATE,MARCHANT,NO_EDC,EDC_DATE,REGION,CATEGORY,MANAGER))
	rdata2=subset(tab3,M_A_DATE>=year_start,c(M_A_DATE,MARCHANT,NO_EDC,EDC_DATE,REGION,CATEGORY,MANAGER))
	Date=0
	Date[1]=min(tab3$EDC_DATE)
	i=2
	while(Date[i-1]<=max(tab3$EDC_DATE))
	{
		Date[i]=Date[i-1]+1
		i=i+1
	}
	Date=as.Date(Date,"1970-01-01")
	Dates=0
	Type=0
	Value=0
	i=1
	j=1
	while(i<=length(Date))
	{
		Dates[j]=Date[i]
		Type[j]="EDC"
		Value[j]=sum(subset(requiredata4(),EDC_DATE==Date[i],NO_EDC)[,1])
		j=j+1
		Dates[j]=Date[i]
		Type[j]="Merchant"
		Value[j]=length(unique(subset(requiredata4(),M_A_DATE==Date[i],MARCHANT))[,1])
		j=j+1
		i=i+1
	}
	Dates=as.Date(Dates,"1970-01-01")
	data1=data.frame(Dates,Type,Value)
	E1=gvisAnnotatedTimeLine(data1, datevar="Dates",numvar="Value", idvar="Type",options=list(displayAnnotations=TRUE,legendPosition="newRow",width=700, height=500,title="Daily Trend"),chartid="E9")
	
	EDC=sum(rdata2$NO_EDC)
	Merchant=length(unique(rdata2$MARCHANT))
	Type="EDC"
	Value=EDC
	data1=data.frame(Type,Value)
	E2=Gauge1 <- gvisGauge(data1, options=list(min=0, max=10000, greenFrom=7000,greenTo=10000, yellowFrom=3000, yellowTo=7000,redFrom=0, redTo=3000,width=200, height=250),chartid="E10")

	Type="Merchant"
	Value=Merchant
	data1=data.frame(Type,Value)
	E3=Gauge1 <- gvisGauge(data1, options=list(min=0, max=600, greenFrom=400,greenTo=600, yellowFrom=200, yellowTo=400,redFrom=0, redTo=200,width=200, height=250),,chartid="E11")
	
	E2E3=gvisMerge(E2,E3,horizontal=TRUE,,chartid="E10E11")
	E1E2E3=gvisMerge(E2E3,E1,horizontal=FALSE,,chartid="E9E10E11")
	E1E2E3$html$chart["jsData"]=gsub("[_]"," ",E1E2E3$html$chart["jsData"])
	E1E2E3$html$chart["jsDrawChart"]="\n// jsDrawChart\nfunction drawChartE10() {
									\nvar data = gvisDataE10();
									\nvar options = {};
									\noptions[\"allowHtml\"] = true;
									\noptions[\"min\"] =      0;
									\noptions[\"max\"] =  10000;
									\noptions[\"greenFrom\"] =   7000;
									\noptions[\"greenTo\"] =  10000;
									\noptions[\"yellowFrom\"] =   3000;
									\noptions[\"yellowTo\"] =   7000;
									\noptions[\"redFrom\"] =      0;
									\noptions[\"redTo\"] =   3000;
									\noptions[\"width\"] =    350;
									\noptions[\"height\"] =    250;
									\n\n\n    var chart = new google.visualization.Gauge(
									\n    document.getElementById('E10')\n    );
									\n    chart.draw(data,options);\n    \n\n}\n  \n\n
									
									\n// jsDrawChart\nfunction drawChartE11() {
									\nvar data = gvisDataE11();
									\nvar options = {};
									\noptions[\"allowHtml\"] = true;
									\noptions[\"min\"] =      0;
									\noptions[\"max\"] =    600;
									\noptions[\"greenFrom\"] =    400;
									\noptions[\"greenTo\"] =    600;
									\noptions[\"yellowFrom\"] =    200;
									\noptions[\"yellowTo\"] =    400;
									\noptions[\"redFrom\"] =      0;
									\noptions[\"redTo\"] =    200;
									\noptions[\"width\"] =    350;
									\noptions[\"height\"] =    250;
									\n\n\n    var chart = new google.visualization.Gauge(
									\n    document.getElementById('E11')\n    );
									\n    chart.draw(data,options);\n    \n\n}\n  \n\n
									
									\n// jsDrawChart\nfunction drawChartE9() {
									\nvar data = gvisDataE9();
									\nvar options = {};
									\noptions[\"width\"] =    700;
									\noptions[\"height\"] =    500;
									\noptions[\"displayAnnotations\"] = true;
									\noptions[\"legendPosition\"] = \"newRow\";
									\noptions[\"scaleType\"]= \"allmaximized\";
									\noptions[\"scaleColumns\"]=[0, 1];
									\noptions[\"displayExactValues\"]= true;
									\n\n\n    var chart = new google.visualization.AnnotatedTimeLine(
									\n    document.getElementById('E9')\n    );
									\n    chart.draw(data,options);\n    \n\n}\n  \n"
	
	E1E2E3$html$chart["divChart"]="\n<table border=\"0\">
									\n<tr>\n<td>\n\n<table border=\"0\">\n<tr>\n<td>\n
									
									\n<!-- divChart -->\n 
									\n<h3>YTD EDC Acquired</h3>\n
									\n<div id=\"E10\"
									\n  style=\"width: 350px; height: 250px;\">\n</div>
									\n\n</td>\n<td>\n
									
									\n<!-- divChart -->\n 
									\n<h3>YTD Merchant Acquired</h3>\n
									\n<div id=\"E11\"
									\n  style=\"width: 350px; height: 250px;\">\n</div>
									\n\n</td>\n</tr>\n</table>\n
									\n</td>\n</tr>\n<tr>\n<td>\n
									
									\n<!-- divChart -->\n
									\n<h3>DAILY TREND LINE</h3>\n
									\n<div id=\"E9\"\n  style=\"width: 700px; height: 500px;\">
									\n</div>\n\n</td>\n</tr>\n</table>\n" 
	
	return(E1E2E3)
	})	
	
###############################################################################################################

#RENDERING Intensification TAB1

###############################################################################################################
requiredata5 = reactive({
	a=subset(tab4, TYPE %in% input$type6 & CATEGORY %in% input$category6)
	a <- droplevels(a)
	return(a)
	})
	
output$In1<- renderGvis({
	rdata1=subset(requiredata5(),M_A_DATE>=year_start,c(M_A_DATE,MARCHANT,NO_EDC,EDC_DATE,REGION,CATEGORY,MANAGER,M1_NO,M2_NO,M3_NO,M4_NO,M5_NO,M6_NO,M7_NO,M8_NO,M9_NO,M10_NO,M11_NO,
														M12_NO,M1_VALUE,M2_VALUE,M3_VALUE,M4_VALUE,M5_VALUE,M6_VALUE,M7_VALUE,M8_VALUE,M9_VALUE,M10_VALUE,M11_VALUE,M12_VALUE))
	
	month=unique(format(rdata1$M_A_DATE,"%B"))
	MONTH=c("January","February","March","April","May","June","July","August","September","October","November","December")
	MONTH=subset(MONTH,MONTH %in% month)
	Transaction=0
	Value=0
	i=length(MONTH)
	j=8
	while(i>0)
	{
		Transaction[i]=sum(rdata1[,j])
		Value[i]=sum(rdata1[,j+12])
		j=j+1
		i=i-1
	}
	data1=data.frame(MONTH,Transaction,Value)
	E1=gvisComboChart(data1, xvar="MONTH",yvar=c("Value", "Transaction"),options=list(seriesType="bars",width=750, height=250),chartid="I1")
	
	data1=array(0,c(34,length(MONTH)+3))
	data1[1,]=c("Region",NA,"YTD",MONTH)
	data1[2:34,1]=c("ALL",NA,NA,
				"Region1",NA,NA,
				"Region2",NA,NA,
				"Region3",NA,NA,
				"Region4",NA,NA,
				"Region5",NA,NA,
				"Region6",NA,NA,
				"Region7",NA,NA,
				"Region8",NA,NA,
				"Region9",NA,NA,
				"Region10",NA,NA)
	data1[2:34,2]=c("# Transaction","Swipe Volume","MDR Amount",
				"# Transaction","Swipe Volume","MDR Amount",
				"# Transaction","Swipe Volume","MDR Amount",
				"# Transaction","Swipe Volume","MDR Amount",
				"# Transaction","Swipe Volume","MDR Amount",
				"# Transaction","Swipe Volume","MDR Amount",
				"# Transaction","Swipe Volume","MDR Amount",
				"# Transaction","Swipe Volume","MDR Amount",
				"# Transaction","Swipe Volume","MDR Amount",
				"# Transaction","Swipe Volume","MDR Amount",
				"# Transaction","Swipe Volume","MDR Amount")
	i=5
	while(i<=length(data1[,1]))
	{
		j=length(data1[1,])
		k=8
		while(j>3)
		{
			if((i-1)%%3==1)
			{
				data1[i,j]=sum(as.numeric(rdata1[,k]))
			}
			else if((i-1)%%3==2)
			{
				data1[i,j]=round(sum(rdata1[,k+12]),2)
			}
			else if((i-1)%%3==0)
			{
				data1[i,j]=round((sum(rdata1[,k+12])/100),2)
			}
			j=j-1
			k=k+1
		}
		i=i+1
	}
	i=2
	while(i<=4)
	{
		j=length(data1[1,])
		k=8
		while(j>3)
		{
			if((i-1)%%3==1)
			{
				data1[i,j]=sum(as.numeric(data1[5,j]),as.numeric(data1[8,j]),as.numeric(data1[11,j]),as.numeric(data1[14,j]),as.numeric(data1[17,j]),as.numeric(data1[20,j]),
								as.numeric(data1[23,j]),as.numeric(data1[26,j]),as.numeric(data1[29,j]),as.numeric(data1[32,j]))
			}
			else if((i-1)%%3==2)
			{
				data1[i,j]=round(sum(as.numeric(data1[6,j]),as.numeric(data1[9,j]),as.numeric(data1[12,j]),as.numeric(data1[15,j]),as.numeric(data1[18,j]),as.numeric(data1[21,j]),
								as.numeric(data1[24,j]),as.numeric(data1[27,j]),as.numeric(data1[30,j]),as.numeric(data1[33,j])),2)
			}
			else if((i-1)%%3==0)
			{
				data1[i,j]=round(sum(as.numeric(data1[7,j]),as.numeric(data1[10,j]),as.numeric(data1[13,j]),as.numeric(data1[16,j]),as.numeric(data1[19,j]),as.numeric(data1[22,j]),
								as.numeric(data1[25,j]),as.numeric(data1[28,j]),as.numeric(data1[31,j]),as.numeric(data1[34,j])),2)
			}
			j=j-1
			k=k+1
		}
		i=i+1
	}
	i=2
	while(i<=length(data1[,1]))
	{
		j=3
		data1[i,j]=sum(as.numeric(data1[i,4:length(data1[1,])]))		
		i=i+1
	}
	data1=data.frame(data1)
	data2=data.frame(data1[2:23,])
	name=c("REGION","Value","YTD",MONTH)
	colnames(data2)=name
	E2=gvisTable(data2,options=list(width=750, height=250),chartid="I2")
	E1E2=gvisMerge(E1,E2,horizontal=FALSE,,chartid="I1I2")
	E1E2$html$chart["jsData"]=gsub("[_]"," ",E1E2$html$chart["jsData"])
	E1E2$html$chart["jsDrawChart"]="\n// jsDrawChart\nfunction drawChartI1() {
									\nvar data = gvisDataI1();
									\nvar options = {};
									\noptions[\"allowHtml\"] = true;
									\noptions[\"seriesType\"] = \"bars\";\
									\noptions[\"width\"] =    750;
									\noptions[\"height\"] =    250;
									\noptions[\"colors\"] = [\"#FF4000\",\"#35FD04\"];
									\noptions[\"legend\"] = {position: \"bottom\", textStyle: {color: \"black\", fontSize: 10}};
									\noptions[\"series\"] = [{type:\"bar\",targetAxisIndex: 0},{targetAxisIndex:1}];
									\noptions[\"chartArea\"]={left:60,top:20,width:\"80%\",height:\"65%\"};
									\noptions[\"hAxis\"]={textStyle: {color: 'black'}};
									\noptions[\"vAxis\"]={textStyle: {color: 'black'}};
									\noptions[\"titleTextStyle\"]={color: 'black'};
									\n\n\n    var chart = new google.visualization.ComboChart(
									\n    document.getElementById('I1')\n    );
									\n    chart.draw(data,options);\n    \n\n}\n  \n\n
									
									\n// jsDrawChart\nfunction drawChartI2() {\
									\nvar data = gvisDataI2();
									\nvar options = {};
									\noptions[\"allowHtml\"] = true;
									\noptions[\"width\"] =    750;
									\noptions[\"height\"] =    450;
									\n\n\n    var chart = new google.visualization.Table(
									\n    document.getElementById('I2')\n    );
									\n    chart.draw(data,options);\n    \n\n}\n  \n" 
	E1E2$html$chart["divChart"]="\n<table border=\"0\">\n<tr>\n<td>\n
								\n<!-- divChart -->\n
								\n<h3>Value V/s Volume of Transaction</h3>\n
								\n<div id=\"I1\"\n  style=\"width: 750px; height: 230px;\">\n</div>\n
								\n</td>\n</tr>\n<tr>\n<td>\n
								
								\n<!-- divChart -->\n
								\n<h3>Value V/s Volume of Transaction in detail</h3>\n
								\n<div id=\"I2\"\n  style=\"width: 750px; height: 450px;\">\n</div>\n
								\n</td>\n</tr>\n</table>\n" 
	return(E1E2)
	})	

###############################################################################################################

#RENDERING Intensification TAB2 part 1

###############################################################################################################
requiredata6 = reactive({
	a=subset(tab5, Type %in% input$type7 & Category %in% input$category7 & Month %in% input$period7)
	a <- droplevels(a)
	return(a)
	})
	
output$In2<- renderGvis({
	rdata1=requiredata6()[,]
	REGION=as.character(unique(tab5$Region))
	i=1
	MDR=0
	COST=0
	BALANCE=0
	INTEREST=0
	REVENUE=0
	while(i<=length(REGION))
	{
		MDR[i]=sum(subset(rdata1,as.character(Region)==REGION[i],MDR)[,1])
		COST[i]=runif(1,MDR[i]/10,MDR[i]/2.5)
		BALANCE[i]=runif(1,MDR[i]*5,MDR[i]*10)
		INTEREST[i]=BALANCE[i]*0.08
		REVENUE[i]=MDR[i]+INTEREST[i]-COST[i]
		i=i+1
	}
	data1=data.frame(REGION,MDR,COST,BALANCE,INTEREST,REVENUE)
	E1=gvisComboChart(data1, xvar="REGION",yvar=c("MDR","INTEREST","COST"),options=list(seriesType="bars",width=700, height=250),chartid="I3")
	E2=gvisComboChart(data1, xvar="REGION",yvar="BALANCE",options=list(seriesType="bars",width=350, height=250),chartid="I4")
	E3=gvisComboChart(data1, xvar="REGION",yvar="REVENUE",options=list(seriesType="bars",width=350, height=250),chartid="I5")
	E2E3=gvisMerge(E2,E3,horizontal=TRUE,chartid="I4I5")
	E1E2E3=gvisMerge(E1,E2E3,horizontal=FALSE,,chartid="I3I4I5")
	E1E2E3$html$chart["jsData"]=gsub("[_]"," ",E1E2E3$html$chart["jsData"])
	E1E2E3$html$chart["jsDrawChart"]="\n// jsDrawChart\nfunction drawChartI3() {
										\nvar data = gvisDataI3();
										\nvar options = {};
										\noptions[\"allowHtml\"] = true;
										\noptions[\"seriesType\"] = \"bars\";
										\noptions[\"width\"] =    700;
										\noptions[\"height\"] =    250;
										\noptions[\"colors\"] = [\"#FF4000\",\"#35FD04\",\"orange\"];
										\noptions[\"legend\"] = {position: \"bottom\", textStyle: {color: \"black\", fontSize: 10}};
										\noptions[\"chartArea\"]={left:60,top:20,width:\"80%\",height:\"75%\"};
										\noptions[\"hAxis\"]={textStyle: {color: 'black'}};
										\noptions[\"vAxis\"]={textStyle: {color: 'black'}};
										\noptions[\"titleTextStyle\"]={color: 'black'};
										\n\n\n    var chart = new google.visualization.ComboChart(
										\n    document.getElementById('I3')\n    );
										\n    chart.draw(data,options);\n    \n\n}\n  \n\n
										
										\n// jsDrawChart\nfunction drawChartI4() {
										\nvar data = gvisDataI4();
										\nvar options = {};
										\noptions[\"allowHtml\"] = true;
										\noptions[\"seriesType\"] = \"bars\";
										\noptions[\"width\"] =    350;
										\noptions[\"height\"] =    250;
										\noptions[\"colors\"] = [\"blue\"];
										\noptions[\"legend\"] = {position: \"bottom\", textStyle: {color: \"black\", fontSize: 10}};
										\noptions[\"chartArea\"]={left:60,top:20,width:\"80%\",height:\"65%\"};
										\noptions[\"hAxis\"]={textStyle: {color: 'black'}};
										\noptions[\"vAxis\"]={textStyle: {color: 'black'}};
										\noptions[\"titleTextStyle\"]={color: 'black'};
										\n\n\n    var chart = new google.visualization.ComboChart(
										\n    document.getElementById('I4')\n    );
										\n    chart.draw(data,options);\n    \n\n}\n  \n\n
										
										\n// jsDrawChart\nfunction drawChartI5() {
										\nvar data = gvisDataI5();
										\nvar options = {};
										\noptions[\"allowHtml\"] = true;
										\noptions[\"seriesType\"] = \"bars\";
										\noptions[\"width\"] =    350;
										\noptions[\"height\"] =    250;
										\noptions[\"colors\"] = [\"yellow\"];
										\noptions[\"legend\"] = {position: \"bottom\", textStyle: {color: \"black\", fontSize: 10}};
										\noptions[\"chartArea\"]={left:60,top:20,width:\"80%\",height:\"65%\"};
										\noptions[\"hAxis\"]={textStyle: {color: 'black'}};
										\noptions[\"vAxis\"]={textStyle: {color: 'black'}};
										\noptions[\"titleTextStyle\"]={color: 'black'};
										\n\n\n    var chart = new google.visualization.ComboChart(
										\n    document.getElementById('I5')\n    );
										\n    chart.draw(data,options);\n    \n\n}\n  \n" 
	E1E2E3$html$chart["divChart"]="\n<table border=\"0\">\n<tr>\n<td>\n
									
									\n<!-- divChart -->\n
									\n<h3>Income and Expense</h3>\n
									\n<div id=\"I3\"\n  style=\"width: 700px; height: 250px;\">\n</div>\n
									\n</td>\n</tr>\n<tr>\n<td>\n\n<table border=\"0\">\n<tr>\n<td>\n
									
									\n<!-- divChart -->\n 
									\n<h3>Acc. balance across Regions</h3>\n
									\n<div id=\"I4\"\n  style=\"width: 350px; height: 250px;\">\n</div>\n
									\n</td>\n<td>\n
									
									\n<!-- divChart -->\n
									\n<h3>Revenue across Regions</h3>\n
									\n<div id=\"I5\"\n  style=\"width: 350px; height: 250px;\">\n</div>\n
									\n</td>\n</tr>\n</table>\n\n</td>\n</tr>\n</table>\n" 
	return(E1E2E3)
	})	

###############################################################################################################

#RENDERING Intensification TAB2 part 2

###############################################################################################################
requiredata7 = reactive({
	a=subset(tab5, Type %in% input$type8 & Category %in% input$category8 & Month %in% input$period8)
	a <- droplevels(a)
	return(a)
	})
output$In3<- renderGvis({
	rdata1=requiredata7()[,]
	REGION=as.character(unique(tab5$Region))
	i=1
	MERCHANT=0
	EDC=0
	New_MERCHANT=0
	New_EDC=0
	Attrition_MERCHANT=0
	Attrition_EDC=0
	while(i<=length(REGION))
	{
		MERCHANT[i]=(length(unique(subset(tab3,as.character(REGION)==REGION[i],MARCHANT)[,1])))*8
		EDC[i]=sum(subset(tab3,as.character(REGION)==REGION[i],NO_EDC)[,1])*3
		New_MERCHANT[i]=sum(subset(rdata1,as.character(Region)==REGION[i],N_Merchant)[,1])
		New_EDC[i]=sum(subset(rdata1,as.character(Region)==REGION[i],N_EDC)[,1])
		Attrition_MERCHANT[i]=sum(subset(rdata1,as.character(Region)==REGION[i],A_Merchant)[,1])
		Attrition_EDC[i]=sum(subset(rdata1,as.character(Region)==REGION[i],A_EDC)[,1])
		i=i+1
	}
	data1=data.frame(REGION,MERCHANT,EDC,New_MERCHANT,New_EDC,Attrition_MERCHANT,Attrition_EDC)
	E1=gvisComboChart(data1, xvar="REGION",yvar=c("MERCHANT","EDC"),options=list(seriesType="bars",width=700, height=250),chartid="I6")
	E2=gvisComboChart(data1, xvar="REGION",yvar=c("New_MERCHANT","New_EDC"),options=list(seriesType="bars",width=350, height=250),chartid="I7")
	E3=gvisComboChart(data1, xvar="REGION",yvar=c("Attrition_MERCHANT","Attrition_EDC"),options=list(seriesType="bars",width=350, height=250),chartid="I8")
	E2E3=gvisMerge(E2,E3,horizontal=TRUE,,chartid="I7I8")
	E1E2E3=gvisMerge(E1,E2E3,horizontal=FALSE,chartid="I6I7I8")
	E1E2E3$html$chart["jsData"]=gsub("[_]"," ",E1E2E3$html$chart["jsData"])
	E1E2E3$html$chart["jsDrawChart"]="\n// jsDrawChart\nfunction drawChartI6() {
										\nvar data = gvisDataI6();
										\nvar options = {};
										\noptions[\"allowHtml\"] = true;
										\noptions[\"seriesType\"] = \"bars\";
										\noptions[\"width\"] =    700;
										\noptions[\"height\"] =    250;
										\noptions[\"colors\"] = [\"#FF4000\",\"#35FD04\"];
										\noptions[\"legend\"] = {position: \"bottom\", textStyle: {color: \"black\", fontSize: 10}};
										\noptions[\"series\"] = [{type:\"bar\",targetAxisIndex: 0},{targetAxisIndex:1}];
										\noptions[\"chartArea\"]={left:40,top:20,width:\"80%\",height:\"75%\"};
										\noptions[\"hAxis\"]={textStyle: {color: 'black'}};
										\noptions[\"vAxis\"]={textStyle: {color: 'black'}};
										\noptions[\"titleTextStyle\"]={color: 'black'};
										\n\n\n    var chart = new google.visualization.ComboChart(
										\n    document.getElementById('I6')\n    );
										\n    chart.draw(data,options);\n    \n\n}\n  \n\n
										
										\n// jsDrawChart\nfunction drawChartI7() {
										\nvar data = gvisDataI7();
										\nvar options = {};
										\noptions[\"allowHtml\"] = true;
										\noptions[\"seriesType\"] = \"bars\";
										\noptions[\"width\"] =    350;
										\noptions[\"height\"] =    250;
										\noptions[\"colors\"] = [\"#FF4000\",\"#35FD04\"];
										\noptions[\"legend\"] = {position: \"bottom\", textStyle: {color: \"black\", fontSize: 10}};
										\noptions[\"series\"] = [{type:\"bar\",targetAxisIndex: 0},{targetAxisIndex:1}];
										\noptions[\"chartArea\"]={left:40,top:20,width:\"80%\",height:\"65%\"};
										\noptions[\"hAxis\"]={textStyle: {color: 'black'}};
										\noptions[\"vAxis\"]={textStyle: {color: 'black'}};
										\noptions[\"titleTextStyle\"]={color: 'black'};
										\n\n\n    var chart = new google.visualization.ComboChart(
										\n    document.getElementById('I7')\n    );
										\n    chart.draw(data,options);\n    \n\n}\n  \n\n
										
										\n// jsDrawChart\nfunction drawChartI8() {
										\nvar data = gvisDataI8();
										\nvar options = {};
										\noptions[\"allowHtml\"] = true;
										\noptions[\"seriesType\"] = \"bars\";
										\noptions[\"width\"] =    350;
										\noptions[\"height\"] =    250;
										\noptions[\"colors\"] = [\"#FF4000\",\"#35FD04\"];
										\noptions[\"legend\"] = {position: \"bottom\", textStyle: {color: \"black\", fontSize: 10}};
										\noptions[\"series\"] = [{type:\"bar\",targetAxisIndex: 0},{targetAxisIndex:1}];
										\noptions[\"chartArea\"]={left:40,top:20,width:\"80%\",height:\"65%\"};
										\noptions[\"hAxis\"]={textStyle: {color: 'black'}};
										\noptions[\"vAxis\"]={textStyle: {color: 'black'}};
										\noptions[\"titleTextStyle\"]={color: 'black'};
										\n\n\n    var chart = new google.visualization.ComboChart(
										\n    document.getElementById('I8')\n    );
										\n    chart.draw(data,options);\n    \n\n}\n  \n" 
	 E1E2E3$html$chart["divChart"]="\n<table border=\"0\">\n<tr>\n<td>\n
									\n<!-- divChart -->\n  
									\n<h3>Total Merchants and EDC</h3>\n
									\n<div id=\"I6\"\n  style=\"width: 700px; height: 230px;\">\n</div>\n
									\n</td>\n</tr>\n<tr>\n<td>\n\n<table border=\"0\">\n<tr>\n<td>\n
									
									\n<!-- divChart -->\n
									\n<h3>New Merchants and EDC</h3>\n
									\n<div id=\"I7\"\n  style=\"width: 350px; height: 230px;\">\n</div>\n
									\n</td>\n<td>\n
									
									\n<!-- divChart -->\n
									\n<h3>Attrition in Merchants and EDC</h3>\n
									\n<div id=\"I8\"\n  style=\"width: 350px; height: 230px;\">\n</div>\n
									\n</td>\n</tr>\n</table>\n\n</td>\n</tr>\n</table>\n" 
	return(E1E2E3)
	})	

###############################################################################################################

#RENDERING Intensification TAB2 part 3

###############################################################################################################
requiredata8 = reactive({
	a=subset(tab5, Type %in% input$type9 & Category %in% input$category9 & Month %in% input$period9)
	a <- droplevels(a)
	return(a)
	})
output$In4<- renderGvis({
	rdata1=requiredata8()[,]
	REGION=as.character(unique(tab5$Region))
	i=1
	Tranasaction=0
	Value=0
	Inactive_Terminal=0
	Active_Terminal=0
	while(i<=length(REGION))
	{
		Tranasaction[i]=sum(subset(rdata1,as.character(Region)==REGION[i],Tranasaction)[,1])
		Value[i]=sum(subset(rdata1,as.character(Region)==REGION[i],Value)[,1])
		Inactive_Terminal[i]=sum(subset(rdata1,as.character(Region)==REGION[i],Inactive_Terminal)[,1])
		Active_Terminal[i]=sum(subset(rdata1,as.character(Region)==REGION[i],Active_Terminal)[,1])
		i=i+1
	}
	data1=data.frame(REGION,Tranasaction,Value,Inactive_Terminal,Active_Terminal)
	E1=gvisComboChart(data1, xvar="REGION",yvar=c("Tranasaction","Value"),options=list(seriesType="bars",width=500, height=250),chartid="I9")
	E2=gvisComboChart(data1, xvar="REGION",yvar=c("Inactive_Terminal","Active_Terminal"),options=list(seriesType="bars",width=500, height=250),chartid="I10")
	E1E2=gvisMerge(E1,E2,horizontal=TRUE,,chartid="I9I10")
	E1E2$html$chart["jsData"]=gsub("[_]"," ",E1E2$html$chart["jsData"])
	E1E2$html$chart["jsDrawChart"]="\n// jsDrawChart\nfunction drawChartI9() {
									\nvar data = gvisDataI9();
									\nvar options = {};
									\noptions[\"allowHtml\"] = true;
									\noptions[\"seriesType\"] = \"bars\";
									\noptions[\"width\"] =    500;
									\noptions[\"height\"] =    250;
									\noptions[\"colors\"] = [\"#FF4000\",\"#35FD04\"];
									\noptions[\"legend\"] = {position: \"bottom\", textStyle: {color: \"black\", fontSize: 10}};
									\noptions[\"series\"] = [{type:\"line\",targetAxisIndex: 1},{targetAxisIndex:0}];
									\noptions[\"chartArea\"]={left:60,top:20,width:\"80%\",height:\"75%\"};
									\noptions[\"hAxis\"]={textStyle: {color: 'black'}};
									\noptions[\"vAxis\"]={textStyle: {color: 'black'}};
									\noptions[\"titleTextStyle\"]={color: 'black'};
									\n\n\n    var chart = new google.visualization.ComboChart(
									\n    document.getElementById('I9')\n    );
									\n    chart.draw(data,options);\n    \n\n}\n  \n\n
									
									\n// jsDrawChart\nfunction drawChartI10() {
									\nvar data = gvisDataI10();
									\nvar options = {};
									\noptions[\"allowHtml\"] = true;
									\noptions[\"seriesType\"] = \"bars\";
									\noptions[\"width\"] =    500;
									\noptions[\"height\"] =    250;
									\noptions[\"colors\"] = [\"#FF4000\",\"#35FD04\"];
									\noptions[\"legend\"] = {position: \"bottom\", textStyle: {color: \"black\", fontSize: 10}};
									\noptions[\"series\"] = [{type:\"line\",targetAxisIndex: 1},{targetAxisIndex:0}];
									\noptions[\"chartArea\"]={left:60,top:20,width:\"80%\",height:\"75%\"};
									\noptions[\"hAxis\"]={textStyle: {color: 'black'}};
									\noptions[\"vAxis\"]={textStyle: {color: 'black'}};
									\noptions[\"titleTextStyle\"]={color: 'black'};
									\n\n\n    var chart = new google.visualization.ComboChart(
									\n    document.getElementById('I10')\n    );
									\n    chart.draw(data,options);\n    \n\n}\n  \n" 
	E1E2$html$chart["divChart"]="\n<table border=\"0\">\n<tr>\n<td>\n
									\n<!-- divChart -->\n
									\n<h3>Value and Volume</h3>\n
									\n<div id=\"I9\"\n  style=\"width: 500px; height: 250px;\">\n</div>\n
									\n</td>\n<td>\n
									
									\n<!-- divChart -->\n
									\n<h3>Active and Inactive Terminals</h3>\n
									\n<div id=\"I10\"\n  style=\"width: 500px; height: 250px;\">\n</div>\n
									\n</td>\n</tr>\n</table>\n" 
	return(E1E2)
	})	

###############################################################################################################

#RENDERING Intensification TAB3 part 1

###############################################################################################################
requiredata9 = reactive({
	a=subset(tab5, Type %in% input$type10 & Region %in% input$region10 & Month %in% input$period10)
	a <- droplevels(a)
	return(a)
	})
	
output$In5<- renderGvis({
	rdata1=requiredata9()[,]
	CATEGORY=as.character(unique(tab5$Category))
	i=1
	MDR=0
	COST=0
	BALANCE=0
	INTEREST=0
	REVENUE=0
	while(i<=length(CATEGORY))
	{
		MDR[i]=sum(subset(rdata1,as.character(Category)==CATEGORY[i],MDR)[,1])
		COST[i]=runif(1,MDR[i]/10,MDR[i]/2.5)
		BALANCE[i]=runif(1,MDR[i]*5,MDR[i]*10)
		INTEREST[i]=BALANCE[i]*0.08
		REVENUE[i]=MDR[i]+INTEREST[i]-COST[i]
		i=i+1
	}
	data1=data.frame(CATEGORY,MDR,COST,BALANCE,INTEREST,REVENUE)
	E1=gvisComboChart(data1, xvar="CATEGORY",yvar=c("MDR","INTEREST","COST"),options=list(seriesType="bars",width=700, height=250),chartid="I11")
	E2=gvisComboChart(data1, xvar="CATEGORY",yvar="BALANCE",options=list(seriesType="bars",width=350, height=250),chartid="I12")
	E3=gvisComboChart(data1, xvar="CATEGORY",yvar="REVENUE",options=list(seriesType="bars",width=350, height=250),chartid="I13")
	E2E3=gvisMerge(E2,E3,horizontal=TRUE,chartid="I12I13")
	E1E2E3=gvisMerge(E1,E2E3,horizontal=FALSE,chartid="I11I12I13")
	E1E2E3$html$chart["jsData"]=gsub("[_]"," ",E1E2E3$html$chart["jsData"])
	E1E2E3$html$chart["jsDrawChart"]="\n// jsDrawChart\nfunction drawChartI11() {
										\nvar data = gvisDataI11();
										\nvar options = {};
										\noptions[\"allowHtml\"] = true;
										\noptions[\"seriesType\"] = \"bars\";
										\noptions[\"width\"] =    700;
										\noptions[\"height\"] =    250;
										\noptions[\"colors\"] = [\"#FF4000\",\"#35FD04\",\"orange\"];
										\noptions[\"legend\"] = {position: \"bottom\", textStyle: {color: \"black\", fontSize: 10}};
										\noptions[\"chartArea\"]={left:60,top:20,width:\"80%\",height:\"75%\"};
										\noptions[\"hAxis\"]={textStyle: {color: 'black'}};
										\noptions[\"vAxis\"]={textStyle: {color: 'black'}};
										\noptions[\"titleTextStyle\"]={color: 'black'};
										\n\n\n    var chart = new google.visualization.ComboChart(
										\n    document.getElementById('I11')\n    );
										\n    chart.draw(data,options);\n    \n\n}\n  \n\n
										
										\n// jsDrawChart\nfunction drawChartI12() {
										\nvar data = gvisDataI12();
										\nvar options = {};
										\noptions[\"allowHtml\"] = true;
										\noptions[\"seriesType\"] = \"bars\";
										\noptions[\"width\"] =    350;
										\noptions[\"height\"] =    250;
										\noptions[\"colors\"] = [\"blue\"];
										\noptions[\"legend\"] = {position: \"bottom\", textStyle: {color: \"black\", fontSize: 10}};
										\noptions[\"chartArea\"]={left:60,top:20,width:\"80%\",height:\"65%\"};
										\noptions[\"hAxis\"]={textStyle: {color: 'black'}};
										\noptions[\"vAxis\"]={textStyle: {color: 'black'}};
										\noptions[\"titleTextStyle\"]={color: 'black'};
										\n\n\n    var chart = new google.visualization.ComboChart(
										\n    document.getElementById('I12')\n    );
										\n    chart.draw(data,options);\n    \n\n}\n  \n\n
										
										\n// jsDrawChart\nfunction drawChartI13() {
										\nvar data = gvisDataI13();
										\nvar options = {};
										\noptions[\"allowHtml\"] = true;
										\noptions[\"seriesType\"] = \"bars\";
										\noptions[\"width\"] =    350;
										\noptions[\"height\"] =    250;
										\noptions[\"colors\"] = [\"yellow\"];
										\noptions[\"legend\"] = {position: \"bottom\", textStyle: {color: \"black\", fontSize: 10}};
										\noptions[\"chartArea\"]={left:60,top:20,width:\"80%\",height:\"65%\"};
										\noptions[\"hAxis\"]={textStyle: {color: 'black'}};
										\noptions[\"vAxis\"]={textStyle: {color: 'black'}};
										\noptions[\"titleTextStyle\"]={color: 'black'};
										\n\n\n    var chart = new google.visualization.ComboChart(
										\n    document.getElementById('I13')\n    );
										\n    chart.draw(data,options);\n    \n\n}\n  \n" 
	E1E2E3$html$chart["divChart"]="\n<table border=\"0\">\n<tr>\n<td>\n
									
									\n<!-- divChart -->\n
									\n<h3>Income and Expense</h3>\n
									\n<div id=\"I11\"\n  style=\"width: 700px; height: 250px;\">\n</div>\n
									\n</td>\n</tr>\n<tr>\n<td>\n\n<table border=\"0\">\n<tr>\n<td>\n
									
									\n<!-- divChart -->\n 
									\n<h3>Acc. balance across Regions</h3>\n
									\n<div id=\"I12\"\n  style=\"width: 350px; height: 250px;\">\n</div>\n
									\n</td>\n<td>\n
									
									\n<!-- divChart -->\n
									\n<h3>Revenue across Regions</h3>\n
									\n<div id=\"I13\"\n  style=\"width: 350px; height: 250px;\">\n</div>\n
									\n</td>\n</tr>\n</table>\n\n</td>\n</tr>\n</table>\n" 
	
	return(E1E2E3)
	})	

###############################################################################################################

#RENDERING Intensification TAB3 part 2

###############################################################################################################
requiredata10 = reactive({
	a=subset(tab5, Type %in% input$type11 & Region %in% input$region11 & Month %in% input$period11)
	a <- droplevels(a)
	return(a)
	})
output$In6<- renderGvis({
	rdata1=requiredata10()[,]
	CATEGORY=as.character(unique(tab5$Category))
	i=1
	MERCHANT=0
	EDC=0
	New_MERCHANT=0
	New_EDC=0
	Attrition_MERCHANT=0
	Attrition_EDC=0
	while(i<=length(CATEGORY))
	{
		MERCHANT[i]=length(unique(subset(tab3,as.character(CATEGORY)==CATEGORY[i],MARCHANT)[,1]))*8
		EDC[i]=sum(subset(tab3,as.character(CATEGORY)==CATEGORY[i],NO_EDC)[,1])*3
		New_MERCHANT[i]=sum(subset(rdata1,as.character(Category)==CATEGORY[i],N_Merchant)[,1])
		New_EDC[i]=sum(subset(rdata1,as.character(Category)==CATEGORY[i],N_EDC)[,1])
		Attrition_MERCHANT[i]=sum(subset(rdata1,as.character(Category)==CATEGORY[i],A_Merchant)[,1])
		Attrition_EDC[i]=sum(subset(rdata1,as.character(Category)==CATEGORY[i],A_EDC)[,1])
		i=i+1
	}
	data1=data.frame(CATEGORY,MERCHANT,EDC,New_MERCHANT,New_EDC,Attrition_MERCHANT,Attrition_EDC)
	E1=gvisComboChart(data1, xvar="CATEGORY",yvar=c("MERCHANT","EDC"),options=list(seriesType="bars",width=700, height=250),chartid="I14")
	E2=gvisComboChart(data1, xvar="CATEGORY",yvar=c("New_MERCHANT","New_EDC"),options=list(seriesType="bars",width=350, height=250),chartid="I15")
	E3=gvisComboChart(data1, xvar="CATEGORY",yvar=c("Attrition_MERCHANT","Attrition_EDC"),options=list(seriesType="bars",width=350, height=250),chartid="I16")
	E2E3=gvisMerge(E2,E3,horizontal=TRUE,chartid="I15I16")
	E1E2E3=gvisMerge(E1,E2E3,horizontal=FALSE,chartid="I14I15I16")
	E1E2E3$html$chart["jsData"]=gsub("[_]"," ",E1E2E3$html$chart["jsData"])
	E1E2E3$html$chart["jsDrawChart"]="\n// jsDrawChart\nfunction drawChartI14() {
										\nvar data = gvisDataI14();
										\nvar options = {};
										\noptions[\"allowHtml\"] = true;
										\noptions[\"seriesType\"] = \"bars\";
										\noptions[\"width\"] =    700;
										\noptions[\"height\"] =    250;
										\noptions[\"colors\"] = [\"#FF4000\",\"#35FD04\"];
										\noptions[\"legend\"] = {position: \"bottom\", textStyle: {color: \"black\", fontSize: 10}};
										\noptions[\"series\"] = [{type:\"bar\",targetAxisIndex: 0},{targetAxisIndex:1}];
										\noptions[\"chartArea\"]={left:40,top:20,width:\"80%\",height:\"75%\"};
										\noptions[\"hAxis\"]={textStyle: {color: 'black'}};
										\noptions[\"vAxis\"]={textStyle: {color: 'black'}};
										\noptions[\"titleTextStyle\"]={color: 'black'};
										\n\n\n    var chart = new google.visualization.ComboChart(
										\n    document.getElementById('I14')\n    );
										\n    chart.draw(data,options);\n    \n\n}\n  \n\n
										
										\n// jsDrawChart\nfunction drawChartI15() {
										\nvar data = gvisDataI15();
										\nvar options = {};
										\noptions[\"allowHtml\"] = true;
										\noptions[\"seriesType\"] = \"bars\";
										\noptions[\"width\"] =    350;
										\noptions[\"height\"] =    250;
										\noptions[\"colors\"] = [\"#FF4000\",\"#35FD04\"];
										\noptions[\"legend\"] = {position: \"bottom\", textStyle: {color: \"black\", fontSize: 10}};
										\noptions[\"series\"] = [{type:\"bar\",targetAxisIndex: 0},{targetAxisIndex:1}];
										\noptions[\"chartArea\"]={left:40,top:20,width:\"80%\",height:\"65%\"};
										\noptions[\"hAxis\"]={textStyle: {color: 'black'}};
										\noptions[\"vAxis\"]={textStyle: {color: 'black'}};
										\noptions[\"titleTextStyle\"]={color: 'black'};
										\n\n\n    var chart = new google.visualization.ComboChart(
										\n    document.getElementById('I15')\n    );
										\n    chart.draw(data,options);\n    \n\n}\n  \n\n
										
										\n// jsDrawChart\nfunction drawChartI16() {
										\nvar data = gvisDataI16();
										\nvar options = {};
										\noptions[\"allowHtml\"] = true;
										\noptions[\"seriesType\"] = \"bars\";
										\noptions[\"width\"] =    350;
										\noptions[\"height\"] =    250;
										\noptions[\"colors\"] = [\"#FF4000\",\"#35FD04\"];
										\noptions[\"legend\"] = {position: \"bottom\", textStyle: {color: \"black\", fontSize: 10}};
										\noptions[\"series\"] = [{type:\"bar\",targetAxisIndex: 0},{targetAxisIndex:1}];
										\noptions[\"chartArea\"]={left:40,top:20,width:\"80%\",height:\"65%\"};
										\noptions[\"hAxis\"]={textStyle: {color: 'black'}};
										\noptions[\"vAxis\"]={textStyle: {color: 'black'}};
										\noptions[\"titleTextStyle\"]={color: 'black'};
										\n\n\n    var chart = new google.visualization.ComboChart(
										\n    document.getElementById('I16')\n    );
										\n    chart.draw(data,options);\n    \n\n}\n  \n" 
	 E1E2E3$html$chart["divChart"]="\n<table border=\"0\">\n<tr>\n<td>\n
									\n<!-- divChart -->\n  
									\n<h3>Total Merchants and EDC</h3>\n
									\n<div id=\"I14\"\n  style=\"width: 700px; height: 230px;\">\n</div>\n
									\n</td>\n</tr>\n<tr>\n<td>\n\n<table border=\"0\">\n<tr>\n<td>\n
									
									\n<!-- divChart -->\n
									\n<h3>New Merchants and EDC</h3>\n
									\n<div id=\"I15\"\n  style=\"width: 350px; height: 230px;\">\n</div>\n
									\n</td>\n<td>\n
									
									\n<!-- divChart -->\n
									\n<h3>Attrition in Merchants and EDC</h3>\n
									\n<div id=\"I16\"\n  style=\"width: 350px; height: 230px;\">\n</div>\n
									\n</td>\n</tr>\n</table>\n\n</td>\n</tr>\n</table>\n" 
	return(E1E2E3)
	})	

###############################################################################################################

#RENDERING Intensification TAB3 part 3

###############################################################################################################
requiredata11 = reactive({
	a=subset(tab5, Type %in% input$type12 & Region %in% input$region12 & Month %in% input$period12)
	a <- droplevels(a)
	return(a)
	})
output$In7<- renderGvis({
	rdata1=requiredata11()[,]
	CATEGORY=as.character(unique(tab5$Category))
	i=1
	Tranasaction=0
	Value=0
	Inactive_Terminal=0
	Active_Terminal=0
	while(i<=length(CATEGORY))
	{
		Tranasaction[i]=sum(subset(rdata1,as.character(Category)==CATEGORY[i],Tranasaction)[,1])
		Value[i]=sum(subset(rdata1,as.character(Category)==CATEGORY[i],Value)[,1])
		Inactive_Terminal[i]=sum(subset(rdata1,as.character(Category)==CATEGORY[i],Inactive_Terminal)[,1])
		Active_Terminal[i]=sum(subset(rdata1,as.character(Category)==CATEGORY[i],Active_Terminal)[,1])
		i=i+1
	}
	data1=data.frame(CATEGORY,Tranasaction,Value,Inactive_Terminal,Active_Terminal)
	E1=gvisComboChart(data1, xvar="CATEGORY",yvar=c("Tranasaction","Value"),options=list(seriesType="bars",width=500, height=250),chartid="I17")
	E2=gvisComboChart(data1, xvar="CATEGORY",yvar=c("Inactive_Terminal","Active_Terminal"),options=list(seriesType="bars",width=500, height=250),chartid="I18")
	E1E2=gvisMerge(E1,E2,horizontal=TRUE,chartid="I17I18")
	E1E2$html$chart["jsData"]=gsub("[_]"," ",E1E2$html$chart["jsData"])
	E1E2$html$chart["jsDrawChart"]="\n// jsDrawChart\nfunction drawChartI17() {
									\nvar data = gvisDataI17();
									\nvar options = {};
									\noptions[\"allowHtml\"] = true;
									\noptions[\"seriesType\"] = \"bars\";
									\noptions[\"width\"] =    500;
									\noptions[\"height\"] =    250;
									\noptions[\"colors\"] = [\"#FF4000\",\"#35FD04\"];
									\noptions[\"legend\"] = {position: \"bottom\", textStyle: {color: \"black\", fontSize: 10}};
									\noptions[\"series\"] = [{type:\"line\",targetAxisIndex: 1},{targetAxisIndex:0}];
									\noptions[\"chartArea\"]={left:60,top:20,width:\"80%\",height:\"65%\"};
									\noptions[\"hAxis\"]={textStyle: {color: 'black'}};
									\noptions[\"vAxis\"]={textStyle: {color: 'black'}};
									\noptions[\"titleTextStyle\"]={color: 'black'};
									\n\n\n    var chart = new google.visualization.ComboChart(
									\n    document.getElementById('I17')\n    );
									\n    chart.draw(data,options);\n    \n\n}\n  \n\n
									
									\n// jsDrawChart\nfunction drawChartI18() {
									\nvar data = gvisDataI18();
									\nvar options = {};
									\noptions[\"allowHtml\"] = true;
									\noptions[\"seriesType\"] = \"bars\";
									\noptions[\"width\"] =    500;
									\noptions[\"height\"] =    250;
									\noptions[\"colors\"] = [\"#FF4000\",\"#35FD04\"];
									\noptions[\"legend\"] = {position: \"bottom\", textStyle: {color: \"black\", fontSize: 10}};
									\noptions[\"series\"] = [{type:\"line\",targetAxisIndex: 1},{targetAxisIndex:0}];
									\noptions[\"chartArea\"]={left:60,top:20,width:\"80%\",height:\"65%\"};
									\noptions[\"hAxis\"]={textStyle: {color: 'black'}};
									\noptions[\"vAxis\"]={textStyle: {color: 'black'}};
									\noptions[\"titleTextStyle\"]={color: 'black'};
									\n\n\n    var chart = new google.visualization.ComboChart(
									\n    document.getElementById('I18')\n    );
									\n    chart.draw(data,options);\n    \n\n}\n  \n" 
	E1E2$html$chart["divChart"]="\n<table border=\"0\">\n<tr>\n<td>\n
									\n<!-- divChart -->\n
									\n<h3>Value and Volume</h3>\n
									\n<div id=\"I17\"\n  style=\"width: 500px; height: 250px;\">\n</div>\n
									\n</td>\n<td>\n
									
									\n<!-- divChart -->\n
									\n<h3>Active and Inactive Terminals</h3>\n
									\n<div id=\"I18\"\n  style=\"width: 500px; height: 250px;\">\n</div>\n
									\n</td>\n</tr>\n</table>\n" 

	return(E1E2)
	})	

###############################################################################################################

#RENDERING Intensification TAB4

###############################################################################################################
requiredata12 = reactive({
	a=subset(tab6, Type %in% input$type13 & Category %in% input$category13 & Region %in% input$region13)
	a <- droplevels(a)
	return(a)
	})
	
output$In8<- renderGvis({
	rdata1=requiredata12()
	Date=0
	Date[1]=min(tab6$Date)
	i=2
	while(Date[i-1]<=max(tab6$Date))
	{
		Date[i]=Date[i-1]+1
		i=i+1
	}
	Date=as.Date(Date,"1970-01-01")
	Dates=0
	Type=0
	Value=0
	i=1
	j=1
	while(i<=length(Date))
	{
		Dates[j]=Date[i]
		Type[j]="Transactions"
		Value[j]=sum(as.numeric(subset(rdata1,Date==Date[i],NO_TRANSACTION)[,1]))
		j=j+1
		Dates[j]=Date[i]
		Type[j]="Amount"
		Value[j]=sum(as.numeric(subset(rdata1,Date==Date[i],VALUE)[,1]))
		j=j+1
		i=i+1
	}
	Dates=as.Date(Dates,"1970-01-01")
	data1=data.frame(Dates,Type,Value)
	E1=gvisAnnotatedTimeLine(data1, datevar="Dates",numvar="Value", idvar="Type",options=list(displayAnnotations=TRUE,legendPosition="newRow",width=700, height=500,title="Daily Trend"),chartid="I19")
	
	EDC=round(sum(tab6$NO_TRANSACTION)/20)
	Merchant=sum(tab6$VALUE)/10

	Type="Transaction"
	Value=EDC
	data1=data.frame(Type,Value)
	E2= gvisGauge(data1, options=list(min=0, max=300000, greenFrom=200000,greenTo=300000, yellowFrom=100000, yellowTo=200000,redFrom=0, redTo=100000,width=350, height=250),chartid="I20")

	Type="Amount"
	Value=Merchant
	data1=data.frame(Type,Value)
	E3= gvisGauge(data1, options=list(min=0, max=150000000, greenFrom=100000000,greenTo=150000000, yellowFrom=50000000, yellowTo=100000000,redFrom=0, redTo=50000000,width=350, height=250),chartid="I21")
	
	E2E3=gvisMerge(E2,E3,horizontal=TRUE,chartid="I20I21")
	E1E2E3=gvisMerge(E2E3,E1,horizontal=FALSE,chartid="I19I20I21")
	E1E2E3$html$chart["jsData"]=gsub("[_]"," ",E1E2E3$html$chart["jsData"])
	E1E2E3$html$chart["jsDrawChart"]="\n// jsDrawChart\nfunction drawChartI20() {
									\nvar data = gvisDataI20();
									\nvar options = {};
									\noptions[\"allowHtml\"] = true;
									\noptions[\"min\"] =      0;
									\noptions[\"max\"] =  300000;
									\noptions[\"greenFrom\"] =   200000;
									\noptions[\"greenTo\"] =  300000;
									\noptions[\"yellowFrom\"] =   100000;
									\noptions[\"yellowTo\"] =   200000;
									\noptions[\"redFrom\"] =      0;
									\noptions[\"redTo\"] =   100000;
									\noptions[\"width\"] =    350;
									\noptions[\"height\"] =    250;
									\n\n\n    var chart = new google.visualization.Gauge(
									\n    document.getElementById('I20')\n    );
									\n    chart.draw(data,options);\n    \n\n}\n  \n\n
									
									\n// jsDrawChart\nfunction drawChartI21() {
									\nvar data = gvisDataI21();
									\nvar options = {};
									\noptions[\"allowHtml\"] = true;
									\noptions[\"min\"] =      0;
									\noptions[\"max\"] =    150000000;
									\noptions[\"greenFrom\"] =    100000000;
									\noptions[\"greenTo\"] =    150000000;
									\noptions[\"yellowFrom\"] =    50000000;
									\noptions[\"yellowTo\"] =    100000000;
									\noptions[\"redFrom\"] =      0;
									\noptions[\"redTo\"] =    50000000;
									\noptions[\"width\"] =    350;
									\noptions[\"height\"] =    250;
									\n\n\n    var chart = new google.visualization.Gauge(
									\n    document.getElementById('I21')\n    );
									\n    chart.draw(data,options);\n    \n\n}\n  \n\n
									
									\n// jsDrawChart\nfunction drawChartI19() {
									\nvar data = gvisDataI19();
									\nvar options = {};
									\noptions[\"width\"] =    700;
									\noptions[\"height\"] =    500;
									\noptions[\"displayAnnotations\"] = true;
									\noptions[\"legendPosition\"] = \"newRow\";
									\noptions[\"scaleType\"]= \"allmaximized\";
									\noptions[\"scaleColumns\"]=[0, 1];
									\noptions[\"displayExactValues\"]= true;
									\n\n\n    var chart = new google.visualization.AnnotatedTimeLine(
									\n    document.getElementById('I19')\n    );
									\n    chart.draw(data,options);\n    \n\n}\n  \n"
	
	E1E2E3$html$chart["divChart"]="\n<table border=\"0\">
									\n<tr>\n<td>\n\n<table border=\"0\">\n<tr>\n<td>\n
									
									\n<!-- divChart -->\n 
									\n<h3>YTD No of Transactions</h3>\n
									\n<div id=\"I20\"
									\n  style=\"width: 350px; height: 250px;\">\n</div>
									\n\n</td>\n<td>\n
									
									\n<!-- divChart -->\n 
									\n<h3>YTD Total Amount</h3>\n
									\n<div id=\"I21\"
									\n  style=\"width: 350px; height: 250px;\">\n</div>
									\n\n</td>\n</tr>\n</table>\n
									\n</td>\n</tr>\n<tr>\n<td>\n
									
									\n<!-- divChart -->\n
									\n<h3>DAILY TREND LINE</h3>\n
									\n<div id=\"I19\"\n  style=\"width: 700px; height: 500px;\">"
	return(E1E2E3)
	})	

###############################################################################################################

#RENDERING Actionable TAB1

###############################################################################################################
requiredata13 = reactive({
	a=subset(tab7, Type %in% input$type14 & Category %in% input$category14 & Region %in% input$region14)
	a <- droplevels(a)
	return(a)
	})
	
output$Ac1<- renderGvis({
	rdata1=requiredata13()
	MONTH=unique(rdata1$Month)
	i=1
	New_Terminal=0
	Old_Terminal=0
	Active_Terminal=0
	Inactive_Terminal=0
	Active_New_Terminal=0
	Inactive_New_Terminal=0
	Active_Old_Terminal=0
	Inactive_Old_Terminal=0
	while(i<=length(MONTH))
	{
		New_Terminal[i]=sum(as.numeric(subset(rdata1,Month==MONTH[i],new)[,1]))
		Old_Terminal[i]=sum(as.numeric(subset(rdata1,Month==MONTH[i],old)[,1]))
		Active_Terminal[i]=sum(as.numeric(subset(rdata1,Month==MONTH[i],active)[,1]))
		Inactive_Terminal[i]=sum(as.numeric(subset(rdata1,Month==MONTH[i],inactive)[,1]))
		Active_New_Terminal[i]=sum(as.numeric(subset(rdata1,Month==MONTH[i],new_active)[,1]))
		Inactive_New_Terminal[i]=sum(as.numeric(subset(rdata1,Month==MONTH[i],new_inactive)[,1]))
		Active_Old_Terminal[i]=sum(as.numeric(subset(rdata1,Month==MONTH[i],old_active)[,1]))
		Inactive_Old_Terminal[i]=sum(as.numeric(subset(rdata1,Month==MONTH[i],old_inactive)[,1]))
		i=i+1
	}
	data1=data.frame(MONTH,New_Terminal,Old_Terminal,Active_Terminal,Inactive_Terminal,Active_New_Terminal,Inactive_New_Terminal,Active_Old_Terminal,Inactive_Old_Terminal)
	E1=gvisColumnChart(data1, xvar="MONTH",yvar=c("New_Terminal","Old_Terminal"),options=list(seriesType="bars",width=300, height=250,title="New Vs Old Terminals"),chartid="A1")
	E2=gvisColumnChart(data1, xvar="MONTH",yvar=c("Active_New_Terminal","Inactive_New_Terminal"),options=list(seriesType="bars",width=300, height=250,title="New Terminals"),chartid="A2")
	E3=gvisColumnChart(data1, xvar="MONTH",yvar=c("Active_Old_Terminal","Inactive_Old_Terminal"),options=list(seriesType="bars",width=300, height=250,title="Old Terminals"),chartid="A3")
	
	data1=array(NA,c(21,length(MONTH)+2))
	data1[1,]=c("Head","Sub-Head",as.character(MONTH))
	data1[2:21,1]=c("Total # Terminals",NA,"Active Terminals",NA,NA,"Inactive Terminals",NA,NA,"Terminal Inactivity",NA,NA,NA,NA,NA,"Swipe Value",NA,NA,"Swipe Volume",NA,NA)
	data1[2:21,2]=c("Total","New Terminals","Total","New Terminals","Old Terminals","Total","New Terminal","Old Terminal","Total Inactive Terminal","New Terminal Inactive",
					"2 Months old Terminal","3 Months Old Terminal"," - 12  Months Old Terminal","More than 12 Months Old Terminal","Less by 10 - 20% on previous month",
					"Less by 20 - 30% on previous month","Less by more than 30% on previous month","Less by 10 - 20% on previous month","Less by 20 - 30% on previous month",
					"Less by more than 30% on previous month")
	i=3
	while(i<=length(data1[1,]))
	{
		data1[2,i]=sum(as.numeric(subset(rdata1,rdata1$Month==data1[1,i],total)[,1]))
		data1[3,i]=sum(as.numeric(subset(rdata1,rdata1$Month==data1[1,i],new)[,1]))
		data1[4,i]=sum(as.numeric(subset(rdata1,rdata1$Month==data1[1,i],active)[,1]))
		data1[5,i]=sum(as.numeric(subset(rdata1,rdata1$Month==data1[1,i],new_active)[,1]))
		data1[6,i]=sum(as.numeric(subset(rdata1,rdata1$Month==data1[1,i],old_active)[,1]))
		data1[7,i]=sum(as.numeric(subset(rdata1,rdata1$Month==data1[1,i],inactive)[,1]))
		data1[8,i]=sum(as.numeric(subset(rdata1,rdata1$Month==data1[1,i],new_inactive)[,1]))
		data1[9,i]=sum(as.numeric(subset(rdata1,rdata1$Month==data1[1,i],old_inactive)[,1]))
		data1[10,i]=sum(as.numeric(subset(rdata1,rdata1$Month==data1[1,i],inactive)[,1]))
		data1[11,i]=sum(as.numeric(subset(rdata1,rdata1$Month==data1[1,i],new_inactive)[,1]))
		data1[12,i]=sum(as.numeric(subset(rdata1,rdata1$Month==data1[1,i],month_2)[,1]))
		data1[13,i]=sum(as.numeric(subset(rdata1,rdata1$Month==data1[1,i],month_3)[,1]))
		data1[14,i]=sum(as.numeric(subset(rdata1,rdata1$Month==data1[1,i],month_4)[,1]))
		data1[15,i]=sum(as.numeric(subset(rdata1,rdata1$Month==data1[1,i],month_12)[,1]))
		data1[16,i]=sum(as.numeric(subset(rdata1,rdata1$Month==data1[1,i],sv_10)[,1]))
		data1[17,i]=sum(as.numeric(subset(rdata1,rdata1$Month==data1[1,i],sv_20)[,1]))
		data1[18,i]=sum(as.numeric(subset(rdata1,rdata1$Month==data1[1,i],sv_30)[,1]))
		data1[19,i]=sum(as.numeric(subset(rdata1,rdata1$Month==data1[1,i],svo_10)[,1]))
		data1[20,i]=sum(as.numeric(subset(rdata1,rdata1$Month==data1[1,i],svo_20)[,1]))
		data1[21,i]=sum(as.numeric(subset(rdata1,rdata1$Month==data1[1,i],svo_30)[,1]))
		i=i+1
	}
	
	data1=data.frame(data1)
	data2=data.frame(data1[2:21,])
	name=c("Head","Sub-Head",as.character(MONTH))
	colnames(data2)=name
	E4 = gvisTable(data2,options=list(height=200,width=900),chartid="A4")
	
	E2E3=gvisMerge(E2,E3,horizontal=TRUE,chartid="A2A3")
	E1E2E3=gvisMerge(E1,E2E3,horizontal=TRUE,chartid="A1A2A3")
	E1E2E3E4=gvisMerge(E1E2E3,E4,horizontal=FALSE,chartid="A1A2A3A4")
	E1E2E3E4$html$chart["jsData"]=gsub("[_]"," ",E1E2E3E4$html$chart["jsData"])
	E1E2E3E4$html$chart["jsDrawChart"]="\n// jsDrawChart\nfunction drawChartA1() {
										\nvar data = gvisDataA1();
										\nvar options = {};
										\noptions[\"allowHtml\"] = true;
										\noptions[\"seriesType\"] = \"bars\";
										\noptions[\"width\"] =    300;
										\noptions[\"height\"] =    250;
										\noptions[\"colors\"] = [\"#FF4000\",\"#35FD04\"];
										\noptions[\"legend\"] = {position: \"bottom\", textStyle: {color: \"black\", fontSize: 10}};
										\noptions[\"series\"] = [{type:\"bar\",targetAxisIndex: 0},{targetAxisIndex:1}];
										\noptions[\"chartArea\"]={left:30,top:20,width:\"80%\",height:\"65%\"};
										\noptions[\"hAxis\"]={textStyle: {color: 'black'}};
										\noptions[\"vAxis\"]={textStyle: {color: 'black'}};
										\noptions[\"titleTextStyle\"]={color: 'black'};
										\n\n\n    var chart = new google.visualization.ColumnChart(
										\n    document.getElementById('A1')\n    )
										;\n    chart.draw(data,options);\n    \n\n}\n  \n\n
										
										\n// jsDrawChart\nfunction drawChartA2() {
										\nvar data = gvisDataA2();
										\nvar options = {};
										\noptions[\"allowHtml\"] = true;
										\noptions[\"seriesType\"] = \"bars\";
										\noptions[\"width\"] =    300;
										\noptions[\"height\"] =    250;
										\noptions[\"colors\"] = [\"#FF4000\",\"#35FD04\"];
										\noptions[\"legend\"] = {position: \"bottom\", textStyle: {color: \"black\", fontSize: 10}};
										\noptions[\"series\"] = [{type:\"bar\",targetAxisIndex: 0},{targetAxisIndex:1}];
										\noptions[\"chartArea\"]={left:30,top:20,width:\"80%\",height:\"65%\"};
										\noptions[\"hAxis\"]={textStyle: {color: 'black'}};
										\noptions[\"vAxis\"]={textStyle: {color: 'black'}};
										\noptions[\"titleTextStyle\"]={color: 'black'};
										\n\n\n    var chart = new google.visualization.ColumnChart(
										\n    document.getElementById('A2')\n    );
										\n    chart.draw(data,options);\n    \n\n}\n  \n\n
										
										\n// jsDrawChart\nfunction drawChartA3() {
										\nvar data = gvisDataA3();
										\nvar options = {};
										\noptions[\"allowHtml\"] = true;
										\noptions[\"seriesType\"] = \"bars\";
										\noptions[\"width\"] =    300;
										\noptions[\"height\"] =    250;
										\noptions[\"colors\"] = [\"#FF4000\",\"#35FD04\"];
										\noptions[\"legend\"] = {position: \"bottom\", textStyle: {color: \"black\", fontSize: 10}};
										\noptions[\"series\"] = [{type:\"bar\",targetAxisIndex: 0},{targetAxisIndex:1}];
										\noptions[\"chartArea\"]={left:30,top:20,width:\"80%\",height:\"65%\"};
										\noptions[\"hAxis\"]={textStyle: {color: 'black'}};
										\noptions[\"vAxis\"]={textStyle: {color: 'black'}};
										\noptions[\"titleTextStyle\"]={color: 'black'};
										\n\n\n    var chart = new google.visualization.ColumnChart(
										\n    document.getElementById('A3')\n    );
										\n    chart.draw(data,options);\n    \n\n}\n  \n\n
										
										\n// jsDrawChart\nfunction drawChartA4() {
										\nvar data = gvisDataA4();
										\nvar options = {};
										\noptions[\"allowHtml\"] = true;
										\noptions[\"height\"] =    400;
										\noptions[\"width\"] =    900;
										\n\n\n    var chart = new google.visualization.Table(
										\n    document.getElementById('A4')\n    );
										\n    chart.draw(data,options);\n    \n\n}\n  \n" 
	E1E2E3E4$html$chart["divChart"]="\n<table border=\"0\">\n<tr>
									\n<td>\n\n<table border=\"0\">\n<tr>\n<td>\n									
									\n<!-- divChart -->\n
									\n<h5>New Vs Old Terminals</h5>\n
									\n<div id=\"A1\"\n  style=\"width: 300px; height: 250px;\">\n</div>\n
									\n</td>\n<td>\n\n<table border=\"0\">\n<tr>\n<td>\n
									
									\n<!-- divChart -->\n 
									\n<h5>New Terminals</h5>\n
									\n<div id=\"A2\"\n  style=\"width: 300px; height: 250px;\">\n</div>\n
									\n</td>\n<td>\n
									
									\n<!-- divChart -->\n  
									\n<h5>Old Terminals</h5>\n
									\n<div id=\"A3\"\n  style=\"width: 300px; height: 250px;\">\n</div>\n
									\n</td>\n</tr>\n</table>\n\n</td>\n</tr>\n</table>\n\n</td>\n</tr>\n<tr>\n<td>\n
									
									\n<!-- divChart -->\n  
									\n<h4>Summery Table</h4>\n
									\n<div id=\"A4\"\n  style=\"width: 900px; height: 400px;\">\n</div>\n
									\n</td>\n</tr>\n</table>\n" 
	return(E1E2E3E4)
})	

###############################################################################################################

#RENDERING Actionable TAB2

###############################################################################################################
requiredata14 = reactive({
	a=subset(tab8, Type %in% input$type15 & Category %in% input$category15 & Region %in% input$region15)
	a <- droplevels(a)
	return(a)
	})
	
output$Ac2<- renderGvis({
	rdata1=requiredata14()
	E1 = gvisTable(rdata1,options=list(height=200,width=900),chartid="A5")
	Type="Inactive Terminals"
	Value=length(rdata1[,1])
	data1=data.frame(Type,Value)
	E2= gvisGauge(data1, options=list(min=0, max=25, greenFrom=0,greenTo=5, yellowFrom=5, yellowTo=15,redFrom=15, redTo=25,width=200, height=250),chartid="A6")
	E1E2=gvisMerge(E1,E2,horizontal=FALSE,,chartid="A5A6")
	E1E2$html$chart["jsData"]=gsub("[_]"," ",E1E2$html$chart["jsData"])
	E1E2$html$chart["jsDrawChart"]="\n// jsDrawChart\nfunction drawChartA5() {
									\nvar data = gvisDataA5();
									\nvar options = {};
									\noptions[\"allowHtml\"] = true;
									\noptions[\"height\"] =    200;
									\noptions[\"width\"] =    900;
									\n\n\n    var chart = new google.visualization.Table(
									\n    document.getElementById('A5')\n    );
									\n    chart.draw(data,options);
									\n    \n\n}\n  \n\n
									
									\n// jsDrawChart\nfunction drawChartA6() {
									\nvar data = gvisDataA6();
									\nvar options = {};
									\noptions[\"allowHtml\"] = true;
									\noptions[\"min\"] =      0;
									\noptions[\"max\"] =     25;
									\noptions[\"greenFrom\"] =      0;
									\noptions[\"greenTo\"] =      5;
									\noptions[\"yellowFrom\"] =      5;
									\noptions[\"yellowTo\"] =     15;
									\noptions[\"redFrom\"] =     15;
									\noptions[\"redTo\"] =     25;
									\noptions[\"width\"] =    200;
									\noptions[\"height\"] =    250;
									\n\n\n    var chart = new google.visualization.Gauge(
									\n    document.getElementById('A6')\n    );
									\n    chart.draw(data,options);\n    \n\n}\n  \n" 
	E1E2$html$chart["divChart"]="\n<table border=\"0\">\n<tr>\n<td>\n
								\n<!-- divChart -->\n
								\n<h3>Inactive Terminal Summery</h3>\n
								\n<div id=\"A5\"\n  style=\"width: 900px; height: 200px;\">\n</div>\n
								\n</td>\n</tr>\n<tr>\n<td>\n
								
								\n<!-- divChart -->\n
								\n<h3>Inactive Terminals</h3>\n
								\n<div id=\"A6\"\n  style=\"width: 200px; height: 250px;\">\n</div>\n
								\n</td>\n</tr>\n</table>\n" 

	return(E1E2)
	})	
		
		
})