library(ggplot2)
library(cluster)

data <- read.csv("census.csv")
dim(data)

#Preparing data
newData <- data.frame(data$Town.Village_Code,data$No.of.Households,data$Total.Population.Person,data$Scheduled.Castes.population.Person
                      ,data$Scheduled.Tribes.population.Person,data$Literates.Population.Female,
                      data$Main.Household.Industries.Population.Person,data$Non.Working.Population.Person,
                      data$Marginal.Worker.Population.Person,data$Main.Working.Population.Female,
                      data$Main.Agricultural.Labourers.Population.Person)



#dim(newData)



newData$data.Scheduled.Castes.population.Person <- newData$data.Scheduled.Castes.population.Person/newData$data.Total.Population.Person
newData$data.Scheduled.Tribes.population.Person <- newData$data.Scheduled.Tribes.population.Person/newData$data.Total.Population.Person
newData$data.Literates.Population.Female <- newData$data.Literates.Population.Female/data$Total.Population.Female
newData$data.Marginal.Worker.Population.Person <- newData$data.Marginal.Worker.Population.Person/newData$data.Total.Population.Person
newData$data.Non.Working.Population.Person <- newData$data.Non.Working.Population.Person/newData$data.Total.Population.Person
newData$data.Main.Household.Industries.Population.Person <- newData$data.Main.Household.Industries.Population.Person/newData$data.Total.Population.Person
newData$data.Main.Working.Population.Female <- newData$data.Main.Working.Population.Female/data$Total.Population.Female
newData$data.Main.Agricultural.Labourers.Population.Person <- newData$data.Main.Agricultural.Labourers.Population.Person/newData$data.Total.Population.Person

#Removing NA's
newData$data.Scheduled.Castes.population.Person <- ifelse(is.na(newData$data.Scheduled.Castes.population.Person),mean(newData$data.Scheduled.Castes.population.Person,
                                                                                                                      na.rm = T),newData$data.Scheduled.Castes.population.Person)
newData$data.Main.Agricultural.Labourers.Population.Person <- ifelse(is.na(newData$data.Main.Agricultural.Labourers.Population.Person),mean(newData$data.Main.Agricultural.Labourers.Population.Person,
                                                                                                                      na.rm = T),newData$data.Main.Agricultural.Labourers.Population.Person)
newData$data.Main.Working.Population.Female <- ifelse(is.na(newData$data.Main.Working.Population.Female),mean(newData$data.Main.Working.Population.Female,
                                                                                                                      na.rm = T),newData$data.Main.Working.Population.Female)
newData$data.Marginal.Worker.Population.Person <- ifelse(is.na(newData$data.Marginal.Worker.Population.Person),mean(newData$data.Marginal.Worker.Population.Person,
                                                                                                                      na.rm = T),newData$data.Marginal.Worker.Population.Person)
newData$data.Non.Working.Population.Person <- ifelse(is.na(newData$data.Non.Working.Population.Person),mean(newData$data.Non.Working.Population.Person,
                                                                                                                      na.rm = T),newData$data.Non.Working.Population.Person)
newData$data.Main.Household.Industries.Population.Person <- ifelse(is.na(newData$data.Main.Household.Industries.Population.Person),mean(newData$data.Main.Household.Industries.Population.Person,
                                                                                                                      na.rm = T),newData$data.Main.Household.Industries.Population.Person)
newData$data.Literates.Population.Female <- ifelse(is.na(newData$data.Literates.Population.Female),mean(newData$data.Literates.Population.Female,
                                                                                                                      na.rm = T),newData$data.Literates.Population.Female)
newData$data.Scheduled.Tribes.population.Person <- ifelse(is.na(newData$data.Scheduled.Tribes.population.Person),mean(newData$data.Scheduled.Tribes.population.Person,
                                                                                                                      na.rm = T),newData$data.Scheduled.Tribes.population.Person)

newData <- newData[data$Town.Village_Code > 0,]
dim(newData)
str(newData)
summary(newData)

#Scaling total population and number of households

newData$data.No.of.Households <- scale(newData$data.No.of.Households)
newData$data.Total.Population.Person <- scale(newData$data.Total.Population.Person)

#Finding the best value for K by elbow method

w <- rep(1,14)

size <- dim(newData)
print(size[2])

set.seed(12)

for (i in 1:15){
  w[i] <- sum(kmeans(newData[c(2:size[2])],centers=i)$withinss)
}
print(w)

plot(1:15,w,type = "b")

#Selecting K value as 4

finalModel = kmeans(newData[c(2:size[2])],centers=4)
finalModel$size
clusplot(newData[c(2:size[2])],finalModel$cluster)
newData$cluster <-  finalModel$cluster

str(newData)
outputData <- data.frame(newData$data.Town.Village_Code)
outputData$Cluster <- finalModel$cluster
write.csv(outputData,file = "Resul.csv")

#Selecting K value as 2
finalModel2 <- kmeans(newData[c(2:size[2])],centers=2)
finalModel2$size
clusplot(newData[c(2:size[2])],finalModel2$cluster)
newData$cluster <-  finalModel2$cluster

str(newData)
outputData <- data.frame(newData$data.Town.Village_Code)
outputData$Cluster <- finalModel2$cluster
write.csv(outputData,file = "ResulE.csv")








