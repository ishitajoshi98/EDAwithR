#Project - 2: COMCAST TELECOM CONSUMER COMPLAINTS

#Set working directory
setwd("C:/Users/ARPITA/Documents/ISHITA/Simplilearn/Projects/R")
getwd()

#Import data
complain_data = read.csv("Comcast Telecom Complaints data.csv", header=TRUE, sep = ",",
                stringsAsFactors = FALSE)
View(complain_data)

str(complain_data)

#Convering all dates into one format 
complain_data$Date <- gsub("/", "-", complain_data$Date)
complain_data$Date <- gsub("-1-", "-01-", complain_data$Date)
complain_data$Date <- gsub("-2-", "-02-", complain_data$Date)
complain_data$Date <- gsub("-3-", "-03-", complain_data$Date)
complain_data$Date <- gsub("-4-", "-04-", complain_data$Date)
complain_data$Date <- gsub("-5-", "-05-", complain_data$Date)
complain_data$Date <- gsub("-6-", "-06-", complain_data$Date)
complain_data$Date <- gsub("-7-", "-07-", complain_data$Date)
complain_data$Date <- gsub("-8-", "-08-", complain_data$Date)
complain_data$Date <- gsub("-9-", "-09-", complain_data$Date)

#Converting date from chr to date
complain_data$Date <- as.Date(complain_data$Date, "%d-%m-%Y")

#Sorting date helps in getting a sorted x-axis for trendline
complain_data$Date <- sort(complain_data$Date)

#Extract month and day from date column
library(tidyr)
library(dplyr)
complain_data = complain_data %>%
  mutate(separate(data = complain_data, col="Date", into= c("year", "month", "day") ))
complain_data
a <- table(complain_data$month)
b <- table(complain_data$Date)

#Provide the trend chart for the number of complaints at monthly and daily granularity levels
plot(a, type = "b", xlab = "Month", ylab = "Number of complaints",
     main = "Complaints received per month")
plot(b, type = "l", xlab = "Day of the month", ylab = "Number of complaints", 
     main = "Trend chart for daily complaints")


#Provide a table with the frequency of complaint types
library(dplyr)
z <- complain_data %>%
  mutate(Customer.Complaint = tolower(Customer.Complaint))

fct <- table(z$Customer.Complaint)
fct <- data.frame(fct)
fct <- fct %>%
  arrange(desc(Freq))
fct <- rename(fct, c("Customer complaint type" = "Var1" ))
h <- head(fct, 20)
h1 <- head(h,6)

library(ggplot2)
ggplot(h1, aes(x=`Customer complaint type`, y=Freq)) +
  geom_bar(stat = "identity") + ggtitle("Frequency of Complaint type")


#Create a new categorical variable with value as Open and Closed
#Open & Pending = Open
#Closed & Solved = Closed

complain_data$Status_new <- gsub("Solved", "Closed", complain_data$Status)
complain_data$Status_new <- gsub("Pending", "Open", complain_data$Status_new )  

#Check if we have only the two required status
unique(complain_data$Status_new)

#Provide state wise status of complaints in a stacked bar chart. Use the categorized variable from Q3.
unique(complain_data$State) 

#Calculate the statewise complaint count
scc <- table(complain_data$State, complain_data$Status_new)
scc <- as.data.frame.matrix(scc)
scc

library(ggplot2)
viz = ggplot(complain_data)+
  geom_bar(mapping=aes(y=State, fill = Status_new)) + ggtitle("State wise complaint count")
viz

#Viz insights: 1. Georgia has the maximum number of complaints 

#Adding the necesscary columns to our data frame scc
scc$Total <- scc$Closed + scc$Open
scc$Percentage_of_unresolved_complaints <- (scc$Open/scc$Total)*100

#Adding column name for first column - State
scc<- cbind(rownames(scc), scc)
rownames(scc) <- NULL
colnames(scc) <- c("State", "Closed", "Open", "Total", "Percentage of unresolved complaints")


#Which state has the maximum complaints
max_complaints <- max(scc$Total)
scc[scc$Total == max_complaints, "State"]

#Which state has the highest percentage of unresolved complaints
max_unresolved <- max(scc$`Percentage of unresolved complaints`)
scc[scc$`Percentage of unresolved complaints` == max_unresolved, "State"]


#Provide the percentage of complaints resolved till date, which were received through 
#the Internet and customer care calls.

#Total resolved
total_resolved <- sum(scc$Closed)/sum(scc$Total)*100
total_resolved

#New frequency table to calculate the number of complaints received via call and internet
res <- table(complain_data$Received.Via ,complain_data$Status_new)
res <- as.data.frame.matrix(res)
res$Total <- res$Closed + res$Open
res<- cbind(rownames(res), res)
rownames(res) <- NULL
colnames(res) <- c("Received Via", "Closed", "Open", "Total")
res

#% of total resolved complaints received via call
res_customercarecall <- (res$Closed[1]/(res$Closed[1]+res$Closed[2]))*100
paste0("Out of the total complaints resolved till date, ", toString(round(res_customercarecall,0)), "% of them were received by Call")

#% of total resolved complaints received via internet
res_internet <- (res$Closed[2]/(res$Closed[1]+res$Closed[2]))*100
paste0("Out of the total complaints resolved till date, ", toString(round(res_internet,0)), "% of them were received via Internet")


#Complaints received via call -> % resolved
res_ccc <- (res$Closed[1]/(res$Total[1]))*100
paste0("Out of the total complaints received via call," , toString(round(res_ccc,0)), "% are resolved till date")

#Plot
slices <- c(864, 255)
lbls <- c("Closed", "Open")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls,col=c("orange", "light blue"), main="Status of Complaints Received Via Call")


#Complaints received via internet -> % resolved
res_int <- (res$Closed[2]/(res$Total[2]))*100
paste0("Out of the total complaints received via internet," , toString(round(res_int,0)), "% are resolved till date")

#Plot
parts <- c(843, 262)
lb <- c("Closed", "Open")
p <- round(parts/sum(parts)*100)
lb <- paste(lb, p) # add percents to labels
lb <- paste(lb,"%",sep="") # ad % to labels
pie(slices,labels = lb,col=c("blue", "yellow"), main="Status of Complaints Received Via Internet")