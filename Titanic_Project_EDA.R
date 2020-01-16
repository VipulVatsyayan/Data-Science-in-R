library(tidyverse)
library(ggplot2)
#install.packages('Hmisc')
library(Hmisc)
library(knitr)
library(ggplot2)
library(dplyr)
library(caret)
library(randomForest)
library(gridExtra)
library(ROCR)
library(corrplot)

test <- read_csv("test.csv")
train <- read_csv("train.csv")
str(train)
str(test)


# Adding the survived colum to test so as to combine the data.

test$Survived <- NA
all <- rbind(train, test)

names(all)

# CHecking formissing values.

a<- sapply(all, function(x) {sum(is.na(x))}
barplot(a)

#We can see that cabit has a lot of missing values. Also age has a few missing values.
# We can see that sex, Plass and survived are catagorical and should be turned to factors.

all$Sex <- as.factor(all$Sex)
all$Survived <- as.factor(all$Survived)
all$Pclass <- as.factor(all$Pclass)

#  CHecking the survived variable.

ggplot(data = all[!is.na(all$Survived),], aes(x= Survived, fill = Survived))+
  geom_bar(stat = 'count')+
  geom_label(stat='count',aes(label=..count..)) 


p1 <- ggplot(all, aes(x = Sex, fill = Sex)) +
  geom_bar(stat='count', position='dodge') +
  geom_label(stat='count', aes(label=..count..)) +
  scale_fill_manual("legend", values = c("female" = "pink", "male" = "green"))

p2 <- ggplot(data = all[!is.na(all$Survived),], aes(x = Sex, fill = Survived))+ 
  geom_bar(stat = 'count', position = "dodge")+
  geom_label(stat = "count", aes(label = ..count..))

library(gridExtra)
library(grid)

grid.arrange(p1,p2, nrow=1)


#  Checking for the class variables.

ggplot(data = all[!is.na(all$Survived),], aes(x = Pclass, fill = Pclass))+
  geom_bar(stat = "count")

ggplot(data = all[!is.na(all$Survived),], aes(x = Pclass, fill = Survived))+
  geom_bar(stat = "count", position = "dodge")

ggplot(data = all[!is.na(all$Survived),], aes(x = Pclass, fill = Survived))+
  geom_bar(stat = "count", position = "stack")

ggplot(data = all[!is.na(all$Survived),], aes(x = Pclass, fill = Survived))+
  geom_bar(stat = "count", position = "fill")
#Pclass 1 and 2 are almost guaranteed survival for women, and Pclass 2 is almost as bad as Pclass 3 for men

all$PclassSex[all$Pclass == '1' & all$Sex == 'male'] <- 'P1Male'
all$PclassSex[all$Pclass=='2' & all$Sex=='male'] <- 'P2Male'
all$PclassSex[all$Pclass=='3' & all$Sex=='male'] <- 'P3Male'
all$PclassSex[all$Pclass=='1' & all$Sex=='female'] <- 'P1Female'
all$PclassSex[all$Pclass=='2' & all$Sex=='female'] <- 'P2Female'
all$PclassSex[all$Pclass=='3' & all$Sex=='female'] <- 'P3Female'
all$PclassSex <- as.factor(all$PclassSex)



#   Creatig new variables for surname.

#Extracting Title and Surname from Name
all$Surname <- sapply(all$Name, function(x) {strsplit(x, split='[,.]')[[1]][1]})
#correcting some surnames that also include a maiden name
all$Surname <- sapply(all$Surname, function(x) {strsplit(x, split='[-]')[[1]][1]})
all$Title <- sapply(all$Name, function(x) {strsplit(x, split='[,.]')[[1]][2]})
all$Title <- sub(' ', '', all$Title) #removing spaces before title
table(all$Sex, all$Title)

all$Title[all$Title %in% c("Mlle", "Ms")] <- "Miss"
all$Title[all$Title== "Mme"] <- "Mrs"
all$Title[!(all$Title %in% c('Master', 'Miss', 'Mr', 'Mrs'))] <- "Rare Title"
all$Title <- as.factor(all$Title)
table(all$Sex, all$Title)


ggplot(all[!is.na(all$Survived),], aes(x = Title, fill = Survived)) +
  geom_bar(stat='count', position='stack') +
  labs(x = 'Title') +theme_grey()


#   Creating family size.

all$Fsize <- all$SibSp+all$Parch +1

ggplot(all[!is.na(all$Survived),], aes(x = Fsize, fill = Survived)) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') + theme_grey()

ggplot(all[!is.na(all$Survived),], aes(x = Fsize, fill = Survived)) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') + theme_grey()


#   Checking for inconcistencies in the family size data.

#composing variable that combines total Fsize and Surname
all$FsizeName <- paste(as.character(all$Fsize), all$Surname, sep="")

SizeCheck <- all %>%
  group_by(FsizeName, Fsize) %>%
  summarise(NumObs=n())

SizeCheck$NumFam <- SizeCheck$NumObs/SizeCheck$Fsize
SizeCheck$modulo <- SizeCheck$NumObs %% SizeCheck$Fsize
SizeCheck <- SizeCheck[SizeCheck$modulo !=0,]
sum(SizeCheck$NumObs) #total number of Observations with inconsistencies


SizeCheck[SizeCheck$FsizeName %in% c('3Davies', '5Hocking', '6Richards', '2Wilkes', '3Richards', '4Hocking'),]

all[all$FsizeName=='3Davies',c(2,3,14,5,6,7,8,17,9,15)]

all$FsizeName[c(550, 1222)] <- '2Davies'
all$SibSp[550] <- 0
all$Parch[1222] <- 1
all$Fsize[c(550, 1222)] <- 2
all[all$FsizeName=='2Davies',c(2,3,14,5,6,7,8,17,9,15)]


NC <- all[all$FsizeName %in% SizeCheck$FsizeName,]


#extracting maiden names
NC$Name <- sub("\\s$", "", NC$Name) #removing spaces at end Name
NC$Maiden <- sub(".*[^\\)]$", "", NC$Name) #remove when not ending with ')'
NC$Maiden <- sub(".*\\s(.*)\\)$", "\\1", NC$Maiden)
NC$Maiden[NC$Title!='Mrs'] <- "" #cleaning up other stuff between brackets (including Nickname of a Mr)
NC$Maiden <- sub("^\\(", '', NC$Maiden) #removing opening brackets (sometimes single name, no spaces between brackets)
#making an exceptions match
NC$Maiden[NC$Name=='Andersen-Jensen, Miss. Carla Christine Nielsine'] <- 'Jensen'

#take only Maiden names that also exist as surname in other Observations
NC$Maiden2[NC$Maiden %in% NC$Surname] <- NC$Maiden[NC$Maiden %in% NC$Surname] 
#create surname+maiden name combinations
NC$Combi[!is.na(NC$Maiden2)] <- paste(NC$Surname[!is.na(NC$Maiden2)], NC$Maiden[!is.na(NC$Maiden2)])


#create labels dataframe with surname and maiden merged into one column
labels1 <- NC[!is.na(NC$Combi), c('Surname','Combi')]
labels2 <- NC[!is.na(NC$Combi), c('Maiden','Combi')]
colnames(labels2) <- c('Surname', 'Combi')
labels1 <- rbind(labels1, labels2)

NC$Combi <- NULL
NC <- left_join(NC, labels1, by='Surname')

CombiMaxF <- NC[!is.na(NC$Combi),] %>%
  group_by(Combi) %>%
  summarise(MaxF=max(Fsize)) #summarise(MaxF=n())
NC <- left_join(NC, CombiMaxF, by = "Combi")

#create family names for those larger families
NC$FsizeCombi[!is.na(NC$Combi)] <- paste(as.character(NC$Fsize[!is.na(NC$Combi)]), NC$Combi[!is.na(NC$Combi)], sep="")

#find the ones in which not all Fsizes are the same
FamMaid <- NC[!is.na(NC$FsizeCombi),] %>%
  group_by(FsizeCombi, MaxF, Fsize) %>%
  summarise(NumObs=n())
FamMaidWrong <- FamMaid[FamMaid$MaxF!=FamMaid$NumObs,]

unique(NC[!is.na(NC$Combi) & NC$FsizeCombi %in% FamMaidWrong$FsizeCombi, c('Combi', 'MaxF')])

NC$MaxF <- NULL #erasing MaxF column maiden combi's

#Find the maximum Fsize within remaining families (no maiden combi's)
FamMale <- NC[is.na(NC$Combi),] %>%
  group_by(Surname) %>%
  summarise(MaxF=max(Fsize))
NC <- left_join(NC, FamMale, by = "Surname")

NCMale <- NC[is.na(NC$Combi),] %>%
  group_by(Surname, FsizeName, MaxF) %>%
  summarise(count=n()) %>%
  group_by(Surname, MaxF) %>%
  filter(n()>1) %>%
  summarise(NumFsizes=n())

NC$Combi[NC$Surname %in% NCMale$Surname] <- NC$Surname[NC$Surname %in% NCMale$Surname]

NCMale[, c(1,2)]

all[all$Surname=='Vander Planke', c(2,3,4,5,6,7,8,9,15)]

#selecting those 37 passengers In Not Correct dataframe
NC <- NC[(NC$FsizeCombi %in% FamMaidWrong$FsizeCombi)|(NC$Surname %in% NCMale$Surname),]

#calculating the average Fsize for those 9 families
NC1 <- NC %>%
  group_by(Combi) %>%
  summarise(Favg=mean(Fsize))
table(NC1)
# 
# A result is that for instance the Fsize is 4 for all 6 people in the Richards-Hockings family.
# This exactly what I wanted, as I wanted to combine those people into a group with all 
# members having the same Fsize (to give equal survival chances to all members within the group) 
# but also not the maximum size as they are less likely to stay together than first degree families.
NC <- left_join(NC, NC1, by = "Combi") #adding Favg to NC dataframe 


NC$Favg <- round(NC$Favg) #rounding those averages to integers
NC <- NC[, c('PassengerId', 'Favg')]
all <- left_join(all, NC, by='PassengerId')

#replacing Fsize by Favg
all$Fsize[!is.na(all$Favg)] <- all$Favg[!is.na(all$Favg)]

#creating a variable with almost the same ticket numbers (only last 2 digits varying)
all$Ticket2 <- sub("..$", "xx", all$Ticket)


rest <- all %>%
  select(PassengerId, Title, Age, Ticket, Ticket2, Surname, Fsize) %>%
  filter(Fsize=='1') %>%
  group_by(Ticket2, Surname) %>%
  summarise(count=n())
rest <- rest[rest$count>1,]
rest1 <- all[(all$Ticket2 %in% rest$Ticket2 & all$Surname %in% rest$Surname & all$Fsize=='1'), c('PassengerId', 'Surname', 'Title', 'Age', 'Ticket', 'Ticket2', 'Fsize', 'SibSp', 'Parch')]
rest1 <- left_join(rest1, rest, by = c("Surname", "Ticket2"))
rest1 <- rest1[!is.na(rest1$count),]
rest1 <- rest1 %>%
  arrange(Surname, Ticket2)
rest1[1:12,]

#replacing Fsize size in my overall dataframe with the count numbers in the table above
all <- left_join(all, rest1)
## Joining, by = c("PassengerId", "Age", "SibSp", "Parch", "Ticket", "Surname", "Title", "Fsize", "Ticket2")
for (i in 1:nrow(all)){
  if (!is.na(all$count[i])){
    all$Fsize[i] <- all$count[i]
  }
}

#composing data frame with group size for each Ticket
TicketGroup <- all %>%
  select(Ticket) %>%
  group_by(Ticket) %>%
  summarise(Tsize=n())
all <- left_join(all, TicketGroup, by = "Ticket")

#Very similarly to the family group sizes, 
#small groups of 2-4 people traveling together on the same ticket have a higher chance of survival.


ggplot(all[!is.na(all$Survived),], aes(x = Tsize, fill = Survived)) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Ticket Size') + theme_grey()


#taking the max of family and ticket size as the group size
all$Group <- all$Fsize
for (i in 1:nrow(all)){
  all$Group[i] <- max(all$Group[i], all$Tsize[i])
}

#Creating final group categories
all$GroupSize[all$Group==1] <- 'solo'
all$GroupSize[all$Group==2] <- 'duo'
all$GroupSize[all$Group>=3 & all$Group<=4] <- 'group'
all$GroupSize[all$Group>=5] <- 'large group'
all$GroupSize <- as.factor(all$GroupSize)


# As ‘1’ and ‘2’ are large groups with their own typical survival rates, I am keeping them as separate groups. 
# Sizes ‘3’ and ‘4’clearly have the best survival chances, and the groups of 5 and more clearly have worse chances.

g1 <- ggplot(all[!is.na(all$Survived),], aes(x = Group, fill = Survived)) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Final Group Sizes') + theme_grey()

g2 <- ggplot(all[!is.na(all$Survived),], aes(x = GroupSize, fill = Survived)) +
  geom_bar(stat='count', position='dodge') +
  labs(x = 'Final Group Categories') + theme_grey() +
  scale_x_discrete (limits = c('solo', 'duo', 'group', 'large group'))
grid.arrange(g2, g1)

#display passengers with missing Embarked
all[which(is.na(all$Embarked)),
    c('Surname', 'Title', 'Survived', 'Pclass', 'Age', 'SibSp', 'Parch', 'Ticket', 'Fare', 'Cabin', 'Embarked', 'Group') ]


all$FarePP <- all$Fare/all$Tsize #creating the Fare Per Person variable

tab2 <- all[(!is.na(all$Embarked) & !is.na(all$Fare)),] %>%
  group_by(Embarked, Pclass) %>%
  summarise(FarePP=median(FarePP))
#imputing missing Embarked values
all$Embarked[all$Ticket=='113572'] <- 'C'
#converting Embarked into a factor
all$Embarked <- as.factor(all$Embarked)

#display passengers with missing Fare
kable(all[which(is.na(all$Fare)),
          c('Surname', 'Title', 'Survived', 'Pclass', 'Age', 'SibSp', 'Parch', 'Ticket', 'Fare', 'Cabin', 'Embarked', 'Group')])


#   FAre per person variables.

tab3 <- all[(!is.na(all$FarePP)),] %>%
  group_by(Pclass) %>%
  summarise(MedianFarePP=median(FarePP))
ggplot(all, aes(x=FarePP)) +
  geom_histogram(binwidth = 5, fill='blue') + theme_grey() +
  scale_x_continuous(breaks= seq(0, 150, by=10))

# We can see that the value is skewed.

all <- left_join(all, tab3, by = "Pclass")
all$FarePP[which(all$FarePP==0)] <- all$MedianFarePP[which(all$FarePP==0)]

#Note Hmisc needs to be loaded before dplyr, as the other way around errors occured due to the kernel using the Hmisc summarize function instead of the dplyr summarize function
all$FareBins <- cut2(all$FarePP, g=5)

ggplot(all[!is.na(all$Survived),], aes(x=FareBins, fill=Survived))+
  geom_bar(stat='count') + theme_grey() + facet_grid(.~Pclass)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#   Missing age value.

ggplot(all[(!is.na(all$Survived) & !is.na(all$Age)),], aes(x = Age, fill = Survived)) +
  geom_density(alpha=0.5, aes(fill=factor(Survived))) + labs(title="Survival density and Age") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + theme_grey()

ggplot(all[!is.na(all$Age),], aes(x = Title, y = Age, fill=Pclass )) +
  geom_boxplot() + scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + theme_grey()

#predicting Age with Linear Regression
set.seed(12000)
AgeLM <- lm(Age ~ Pclass + Sex + SibSp + Parch + Embarked + Title + GroupSize, data=all[!is.na(all$Age),])
summary(AgeLM)

all$AgeLM <- predict(AgeLM, all)

par(mfrow=c(1,2))
hist(all$Age[!is.na(all$Age)], main='Original data, non-missing', xlab='Age', col='green')
hist(all$AgeLM[is.na(all$Age)], main= 'LM NA predictions', xlab='Age', col='orange', xlim=range(0:80))

#display which passengers are predicted to be children (age<18) with Linear Regression.
all[(is.na(all$Age) & all$AgeLM <18), c('Sex', 'SibSp', 'Parch', 'Title', 'Pclass', 'Survived', 'AgeLM')]

#imputing Linear Regression predictions for missing Ages
indexMissingAge <- which(is.na(all$Age))
indexAgeSurvivedNotNA<- which(!is.na(all$Age) & (!is.na(all$Survived))) #needed in sections 4.6 and 4.7
all$Age[indexMissingAge] <- all$AgeLM[indexMissingAge]


#  Cabin

#replacing NAs with imaginary Deck U, and keeping only the first letter of ech Cabin (=Deck)
all$Cabin[is.na(all$Cabin)] <- "U"
all$Cabin <- substring(all$Cabin, 1, 1)
all$Cabin <- as.factor(all$Cabin)

ggplot(all[(!is.na(all$Survived)& all$Cabin!='U'),], aes(x=Cabin, fill=Survived)) +
  geom_bar(stat='count') + theme_grey() + facet_grid(.~Pclass) + labs(title="Survivor split by class and Cabin")

c1 <- round(prop.table(table(all$Survived[(!is.na(all$Survived)&all$Cabin!='U')], all$Cabin[(!is.na(all$Survived)&all$Cabin!='U')]),2)*100)
c1


#  Dealing with Children.

ggplot(all[all$Age<14.5 & !is.na(all$Survived),], aes(x=Pclass, fill=Survived))+
  geom_bar(stat='count') + theme_grey(base_size = 18)

all$IsChildP12 <- 'No'
all$IsChildP12[all$Age<=14.5 & all$Pclass %in% c('1', '2')] <- 'Yes'
all$IsChildP12 <- as.factor(all$IsChildP12)


tab1 <- rbind(table(all$Embarked[!is.na(all$Survived)]),table(all$Embarked[indexAgeSurvivedNotNA]))
tab1 <- cbind(tab1, (rowSums(tab1)))
tab1 <- rbind(tab1, tab1[1,]-tab1[2,])
tab1 <- rbind(tab1, round((tab1[3,]/tab1[1,])*100))
rownames(tab1) <- c("All", "With Age", "Missing Age", "Percent Missing")
colnames(tab1) <- c("C", "Q", "S", "Total")
tab1


#   cheecking if people in a group survivved.

library(tidyverse)

TicketSurvivors <- all %>%
  group_by(Ticket) %>%
  plyr::summarize(Tsize = length(Survived),
            NumNA = sum(is.na(Survived)),
            SumSurvived = sum(as.numeric(Survived)-1, na.rm=T))
all <- left_join(all, TicketSurvivors)
## Joining, by = c("Ticket", "Tsize")
all$AnySurvivors[all$Tsize==1] <- 'other'
all$AnySurvivors[all$Tsize>=2] <- ifelse(all$SumSurvived[all$Tsize>=2]>=1, 'survivors in group', 'other')
all$AnySurvivors <- as.factor(all$AnySurvivors)
kable(x=table(all$AnySurvivors), col.names= c('AnySurvivors', 'Frequency'))

# Adding an “Is Solo” variable" based on Siblings and Spouse (SibSp) only
all$IsSolo[all$SibSp==0] <- 'Yes'
all$IsSolo[all$SibSp!=0] <- 'No'
all$IsSolo <- as.factor(all$IsSolo)

ggplot(all[!is.na(all$Survived),], aes(x = IsSolo, fill = Survived)) +
  geom_bar(stat='count') + theme_grey(base_size = 18)


#   Building models.

#splitting data into train and test set again
trainClean <- all[!is.na(all$Survived),]
testClean <- all[is.na(all$Survived),]

#   Random Forest

set.seed(2017)
caret_matrix <- train(x=trainClean[,c('PclassSex', 'GroupSize', 'FarePP', 'AnySurvivors', 'IsChildP12')], 
                      y=trainClean$Survived, data=trainClean, method='rf', trControl=trainControl(method="cv", number=5))
caret_matrix
warnings()

caret_matrix$results


#extracting variable importance and make graph with ggplot (looks nicer that the standard varImpPlot)
rf_imp <- varImp(caret_matrix, scale = FALSE)
rf_imp <- rf_imp$importance
rf_gini <- data.frame(Variables = row.names(rf_imp), MeanDecreaseGini = rf_imp$Overall)

ggplot(rf_gini, aes(x=reorder(Variables, MeanDecreaseGini), y=MeanDecreaseGini, fill=MeanDecreaseGini)) +
  geom_bar(stat='identity') + coord_flip() + theme(legend.position="none") + labs(x="") +
  ggtitle('Variable Importance Random Forest') + theme(plot.title = element_text(hjust = 0.5))


# Support Vector Machine (SVM) model

set.seed(2017)
caret_svm <- train(Survived~ PclassSex + FarePP + AnySurvivors + IsChildP12 + IsSolo, data=trainClean, method='svmRadial', preProcess= c('center', 'scale'), trControl=trainControl(method="cv", number=5))
caret_svm

caret_svm$results

#using the model to make Survival predictions on the test set
solution_svm <- predict(caret_svm, testClean)


# Gradient Boosting Machine (GBM) model


set.seed(2017)
caret_boost <- train(Survived~ PclassSex + GroupSize + FareBins + AnySurvivors + IsChildP12, data=trainClean, method='gbm', preProcess= c('center', 'scale'), trControl=trainControl(method="cv", number=7), verbose=FALSE)
print(caret_boost)


#   Combining the models.

#adding model predictions to test dataframe
testClean$RF <- as.numeric(solution_rf)-1
testClean$SVM <- as.numeric(solution_svm)-1
testClean$Boost <- as.numeric(solution_boost)-1

#compose correlations plot
corrplot.mixed(cor(testClean[, c('RF', 'SVM', 'Boost')]), order="hclust", tl.col="black")

testClean$Sum <- testClean$RF + testClean$SVM + testClean$Boost
testClean$Majority <- ifelse(testClean$Sum<=1, 0, 1)

#Taking predictions from one model, unless the others both disagree

testClean$DisagreeSVM <- ifelse(testClean$RF==testClean$Boost & testClean$SVM != testClean$RF, testClean$RF, testClean$SVM)

# Selectively combining models for PclassSex combinations

#predictions of the models on the training set
trainClean$RF <- predict(caret_matrix, trainClean)
trainClean$SVM <- predict(caret_svm, trainClean)
trainClean$Boost <- predict(caret_boost, trainClean)


#plot differences between actual survived and predictions
f1 <- ggplot(trainClean[trainClean$Survived != trainClean$RF,], aes(x=PclassSex, fill=RF)) +
  geom_bar(stat='count') + labs(title="FP and FN, RF model") + theme_grey() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position="none") + xlab("")

f2 <- ggplot(trainClean[trainClean$Survived != trainClean$SVM,], aes(x=PclassSex, fill=SVM)) +
  geom_bar(stat='count')+ labs(title="FP and FN, SVM") + theme_grey() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position="none") + xlab("")

f3 <- ggplot(trainClean[trainClean$Survived != trainClean$Boost,], aes(x=PclassSex, fill=Boost)) +
  geom_bar(stat='count')+ labs(title="FP and FN, GBM") + theme_grey() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position="none") + xlab("")

grid.arrange(f1, f2, f3, nrow = 1)



#selecting SVM prediction, and GMB predictions for P3
testClean$Select <- ifelse(testClean$Pclass != 3, testClean$SVM, testClean$Boost)


#writing final data frame.
Final_file <- data.frame(PassengerId = test$PassengerId, Survived = testClean$Select)


