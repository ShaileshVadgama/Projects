
#Loading the required packages

library(readr)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(tidyr)
library(gridExtra)
```
```{r,warnings=FALSE,messages=FALSE}
#The dataset contain FDI(in Crore) in India from 2000-2017.

#Assigning the data to a variable "FDI_1"
FDI_1 <- read.csv("C:/Users/Admin.DESKTOP-Q9CVR13/Desktop/iNUR/archive (1) (1)/FDI_in_India.csv")

#Trying the understand the structure of the dataset
#Listing the columns in the dataset
colnames(FDI_1)

#Getting the summary of each column
summary(FDI_1)
```
```{r,warnings=FALSE,messages=FALSE}
#Converting the "Sector" from char to factor type
FDI_1$Sector<-as.factor(FDI_1$Sector)


#Transforming the dataset
FDI_2 <- gather(FDI_1,"Year","Investment",2:18)
View(FDI_2)

#Now starting the analytics on the dataset
FDI_3 <- FDI_2 %>% group_by(Sector) %>% summarise(Total=sum(Investment)) 

#Sectors with below 100Crores of investment during 2000-2017
FDI_3[(FDI_3$Total<100),c(1,2)]

#Bar plot for FDI in each sector
ggplot2::ggplot(aes(x=Sector,y=Total,fill=Sector),data=subset(FDI_3,FDI_3$Total<30000))+
  geom_bar(stat = "identity")+ylab("FDI In India(2000-2017)")+
  theme(axis.text.x=element_text(angle=90),plot.title = element_text(hjust = .5,vjust =1,colour = "darkblue",face="bold"),text = element_text(size = 10),
        legend.position="none",panel.background = element_rect(fill = "black"),panel.grid.major = element_blank())+coord_flip()+ggtitle("FDI In India By Sectors(2000-2017)")

#Grouping the data by "year"
FDI_4 <- FDI_2 %>% group_by(Year) %>% summarise(TotalFDI=sum(Investment))
FDI_4$Year <- as.factor(FDI_4$Year)

#Generating line graph for YOY investment
ggplot(aes(x=Year,y=TotalFDI),data=FDI_4)+geom_point(color="darkred",shape=1,size=2.5)+geom_line(aes(group=1,color=Year),size=1.2)+
  xlab("Year")+ylab("Foreign Direct Investment")+theme(plot.title = element_text(hjust = .5,vjust = -10,colour = "darkblue",face = "bold"),
                                                       axis.text.x = element_text(angle = 45,hjust=1,colour = "white"),axis.text.y = element_text(colour = "white"),
                                                       axis.title = element_text(color = "lightblue",size = 14),legend.position = "none",plot.background = element_rect(fill = "black"),
                                                       panel.grid.major = element_blank(),panel.background = element_rect(fill = "grey"))+ggtitle("YOY FDI In India (2000-2017)")
