library(lubridate)  
library(openxlsx)  
library(haven)  
library(Hmisc)  
library(corrplot)  
library (gridExtra)  
library (ggplot2)  
library (DescTools)  
library (dplyr)  
library (ggrepel)  
library(scales)  
library (ggmap)  
library (jpeg)  
library (grid)  

# choose file with XLSX
df <- choose.files(default="", caption="Select file",  
                   multi=FALSE, filters=Filters,  
                   index=nrow(Filters))   
df <- read.xlsx(df, colNames=TRUE, detectDates = TRUE, startRow= 2)  

raw1 <- df  

raw1 <- subset(df, Amount > 0)  

raw1 <- raw1[!grepl("USA", raw1$Attribution.Country),]  

raw1$Contract.End.Date[is.na(raw1$Contract.End.Date)] <-   
  raw1$Receipt.Date[is.na(raw1$Contract.End.Date)]  

raw1$Contract.Start.Date[is.na(raw1$Contract.Start.Date)] <-   
  raw1$Receipt.Date[is.na(raw1$Contract.Start.Date)]  

clean <- raw1  

clean$Contract.Start.Date <- if_else(clean$Contract.Start.Date > clean$Contract.End.Date,  
                                     clean$Receipt.Date,clean$Contract.Start.Date)  
wrongdates <- subset(df, Contract.End.Date < df$Contract.Start.Date)  

negative <- subset(df, Amount<0)  

sum(negative$Amount)  

USA <- subset(df, Attribution.Country == "USA")   

sum(USA$Amount)  

####first key  

df1 <- clean[, c("School.Name","Attribution.Country","Amount")]   

df1$count<-ifelse(df1$`School.Name`== "Harvard University" |   
                    df1$`School.Name`== "Brown University"|   
                    df1$`School.Name`== "Columbia University in the City of New York"|   
                    df1$`School.Name`== "Cornell University"|   
                    df1$`School.Name`== "Dartmouth College"|   
                    df1$`School.Name`== "Princeton University"|   
                    df1$`School.Name`== "University of Pennsylvania"|   
                    df1$`School.Name`== "Yale University","Ivy","Non_Ivy")   

IVY <- subset(df1,count == "Ivy")   

sum (IVY$Amount)  

NonIVY <- subset(df1,count == "Non_Ivy")   

sum (NonIVY$Amount)  

dfIvy <- aggregate(df1$Amount~df1$count,df1,sum)  
names(dfIvy)[1] <- "Type"  
names(dfIvy)[2] <- "Received_amount"  

total <- sum(dfIvy$Received_amount)   
dfIvy$dec <- dfIvy$Received_amount/total   

dfIvy$Percentage <- percent(dfIvy$dec)   

ggplot(dfIvy, aes(x="",y=Received_amount,fill=Type))+ 
  geom_bar(stat="identity",width=1)+  
  coord_polar("y",start=0)+  
  ggtitle("Percentage of Total Contribution")+  
  theme_void()+  
  scale_fill_brewer(palette="PuRd")+  
  geom_text(aes(label=Percentage),  
            position=position_stack(vjust=0.5))+  
  scale_y_continuous(labels = scales::percent_format(scale=1))  


IVYSchool <- aggregate(IVY$Amount~IVY$School.Name,df1,sum)   
names(IVYSchool)[1] <- "School.Name"  
names(IVYSchool)[2] <- "Amount"  

IVYSchool <- IVYSchool[order(IVYSchool$Amount,decreasing = TRUE),]   

IVYSchool$School.Name <- recode(IVYSchool$School.Name, "Columbia University in the City of New York" = "Columbia University")   

IVYSchool$Amount <- IVYSchool$Amount/1000000  

IVYPlot <- ggplot(data=IVYSchool, aes(School.Name, Amount)) +  
  geom_bar(stat="identity", fill= "blue") + coord_flip()+  
  labs(y="Contribution Amount(million)", x="IVY School") +   
  scale_y_continuous(labels=scales::dollar_format(big.mark=","))  
ggtitle("IVY Schools")  

Non_IVYSchool <- aggregate(NonIVY$Amount~NonIVY$School.Name,df1,sum)  
names(Non_IVYSchool)[1] <- "School.Name"  
names(Non_IVYSchool)[2] <- "Amount"  

Non_IVYSchool <- Non_IVYSchool[order(Non_IVYSchool$Amount,decreasing = TRUE),]   

Non_IVYSchool$School.Name <- recode(Non_IVYSchool$School.Name, "University of Texas MD Anderson Cancer Center" = "University of Texas")   

Non_IVYSchool$Amount <- Non_IVYSchool$Amount/1000000  

Non_Ivy8 <- head(Non_IVYSchool,8)  

Non_IVYPlot <- ggplot(data=Non_Ivy8, aes(School.Name, Amount)) +  
  geom_bar(stat="identity", fill= "red") + coord_flip()+  
  labs(y="Contribution Amount(million)", x="Non IVY School")+  
  scale_y_continuous(labels=scales::dollar_format(big.mark=","))  
ggtitle("Non IVY Schools")  

sum(Non_Ivy8$Amount)  

grid.arrange(IVYPlot,Non_IVYPlot, nrow=1,   
             top = textGrob("Contribution by University",gp=gpar(fontsize=20,font=3)))  

IVYcountries <- aggregate(IVY$Amount~IVY$Attribution.Country,IVY,sum)  

names(IVYcountries)[1] <- "Countries"  
names(IVYcountries)[2] <- "Amount"  

IVYcountries <- IVYcountries[order(IVYcountries$Amount,decreasing = TRUE),]  

IVYcountries$Amount <- IVYcountries$Amount/1000000  

IVYcountries8 <- head(IVYcountries,8)  

IVYcountries8 <- IVYcountries8 %>%   
  mutate(Amount = scales::dollar_format()(IVYcountries8$Amount))  

IVYcountries8 <- tableGrob(IVYcountries8, rows=NULL)  
Non_IVYcountries <- aggregate(NonIVY$Amount~NonIVY$Attribution.Country,NonIVY,sum)  
names(Non_IVYcountries)[1] <- "Countries"  
names(Non_IVYcountries)[2] <- "Amount"  

Non_IVYcountries <- Non_IVYcountries[order(Non_IVYcountries$Amount,decreasing = TRUE),]   

Non_IVYcountries$Amount <- Non_IVYcountries$Amount/1000000  

Non_IVYcountries8 <- head(Non_IVYcountries,8)  

Non_IVYcountries8 <- Non_IVYcountries8 %>%   
  mutate(Amount = scales::dollar_format()(Non_IVYcountries8$Amount))  

Non_IVYcountries8 <- tableGrob(Non_IVYcountries8, rows=NULL)  
grid.arrange(arrangeGrob(IVYcountries8, top = 'IVY(million)'), arrangeGrob(Non_IVYcountries8, top = 'Non IVY(million)'), top = "Contribution by Countries", ncol=2)  

#####part 2#####  

df2 <- clean[, c("Contract.Start.Date","Amount","Attribution.Country")]    

df2$Contract.Start.Date <- as.Date(as.character(df2$Contract.Start.Date),format="%Y%m%d")   

df2$Year <- format(clean$Contract.Start.Date, "%Y")    

df20 <- aggregate(df2$Amount~df2$Year,df,sum)    

names(df20)[1] <- "Year"    
names(df20)[2] <- "Amount"    

df20$Amount <- df20$Amount/1000000   

ggplot(data=df20, aes(Year, Amount)) +     
  geom_bar(stat="identity", fill= "slateblue4")  +    
  ggtitle("Total Annual Contributions")+   
  labs(y="Contribution Amount (in millions)", x="Year")+    
  scale_y_continuous(labels=scales::dollar_format(big.mark=","))+   
  theme_bw()   

df21 <- df2[, c("Attribution.Country","Amount","Year")]    

df21$count <- ifelse(df21$Attribution.Country == "CHINA","China","Other")    
China <- subset(df21,count == "China")    

df22 <- aggregate(China$Amount~China$Year,df21,sum)   

names(df22)[1] <- "Year"    

names(df22)[2] <- "Amount"    

df22$Amount <- df22$Amount/1000000   

ggplot(data=df22, aes(Year, Amount)) +     
  geom_bar(stat="identity", fill= "slateblue4")  +     
  ggtitle("Total Chinese Annual Contributions")+   
  labs(y="Contribution Amount (in millions)", x="Year")+    
  scale_y_continuous(labels=scales::dollar_format(big.mark=","))+   
  theme_bw()   

df21$count2 <- ifelse(df21$Attribution.Country == "RUSSIA","Russia","Other")    

Russia <- subset(df21,count2 == "Russia")      
df23 <- aggregate(Russia$Amount~Russia$Year,df21,sum)    

names(df23)[1] <- "Year"    
names(df23)[2] <- "Amount"    

df23$Amount <- df23$Amount/1000000   
ggplot(data=df23, aes(Year, Amount)) +     
  geom_bar(stat="identity", fill= "slateblue4")+   
  ggtitle("Total Russian Annual Contributions")+   
  labs(y="Contribution Amount (in millions)", x="Year")+  
  scale_y_continuous(labels=scales::dollar_format(big.mark=","))+   
  theme_bw()   
total <- merge(df22,df23,by="Year")   

names(total)[2]<- "China"   
names(total)[3] <- "Russia"    

ggplot(total, aes(China,Russia)) + geom_point(color="slateblue4") +   
  labs(y="Russia",x="China") +   
  ggtitle("Scatterplot of Russia and China total contributions") + theme_bw() 
cor.test(total$China, total$Russia, method = "pearson")   

######3rd key aspect######  

Country <- df1[, c("Attribution.Country", "Amount")]    

Country <- subset(Country, Amount>1)    

Country%>%    
  group_by(Attribution.Country)%>%    
  summarize(sum_Amount = sum(Amount),    
            average_Amount = mean(Amount))    

Country2 <- aggregate(Country$Amount~Country$Attribution.Country,Country,sum)    
names(Country2)[1] <- "Country"    
names(Country2)[2] <- "Amount"    

total <- sum(Country2$Amount)      

Country2$dec <- Country2$Amount/total     
Country2$Percentage <- percent(Country2$dec)    

map.world1 <- map_data("world")   
map.world1$region<- toupper(map.world1$region)   

countries3 <- unique(map.world1$region)   
country3 <- aggregate(Country2$Amount~Country2$Country,Country2,sum)   

names(country3)[1] <- "region"   
names(country3)[2] <- "Amount"   

map_world_joined1  <- inner_join(country3,map.world1, by="region")   

map_world_joined1 <- map.world1 %>% left_join(country3,by=("region"))   

ggplot()+   
  geom_polygon(data=map_world_joined1,aes(x=long,y=lat,group=group,fill=Amount))+  
  scale_fill_continuous(low="darkorchid1",high="darkorchid4",guide="colorbar",   
                labels=scales::dollar_format())+  
  theme(panel.border=element_blank(),  
        panel.background = element_rect(fill = "white",   
                                        size = 0.5, linetype = "solid"))+   
  labs(title="Global Transactions to U.S. University",   
       x="Longitude", y="Latitude",   
       caption = "Source of data: College Foreign Gift Reporting") 


