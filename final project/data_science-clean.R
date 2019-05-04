####Libraries needed####
library(dplyr)
library(lme4)
library(plotrix)
library(tidyr)
library(ggplot2)
library(brms)
library(lmerTest)
library(gtools)

####load data. PartData is participant demo data and Mem Data is core for analysis####
PartData <- read.csv('/Users/raider/Dropbox/backup/grad school/Lab work/Eye-Tracking Causal/Results Memory/CausalMemDemo_py.csv')
MemData <- read.csv('/Users/raider/Dropbox/backup/grad school/Lab work/Eye-Tracking Causal/Results Memory/CausalMem_py.csv')

####Clean it up####
#by getting rid of columns I won't be using
MemData$LogDetJOL <- NULL 
MemData$LogInferJOL <- NULL 
MemData$LogQuestNum <- NULL 

#Some variables with numbers are factors
MemData$ID <- as.factor(MemData$ID)
MemData$QuestNum <- as.factor(MemData$QuestNum)
PartData$List <- as.factor(PartData$List)

####Find participants whose data is chance and remove them####
MbyPart <- MemData %>%
  group_by(ID) %>%
  summarise(IAccM = mean(InferAns, na.rm=TRUE), #Straight up accuracy on inference questions
            DAccM = mean(DetAns,na.rm=TRUE)) #Straight up accuracy on detail questions

#Criterion for removing participants requires less than chance accuracy on both measures (not based on JOL corrected performance)
badacc <- MbyPart[MbyPart$IAccM < .5 & MbyPart$DAccM < .5,] #only one participant meets this criteria
MemData <- MemData[!(MemData$ID==9),] #because it's just the one, removal is hard coded to minimize lines of code
PartData <- PartData[!(PartData$ID==9),]
rm(badacc)
rm(MbyPart)

#####the lists and conditions are even####
summary(PartData$List) #14 participants per list
summary(MemData$OrderofClauses) #2520 observations in each level
summary(MemData$ConnectLoc) #1680 observations in each level
summary(MemData$DetailLoc) #2520 of each
MemData$CombineCond <- as.factor(paste(MemData$OrderofClauses,MemData$ConnectLoc,MemData$DetailLoc))
summary(MemData$CombineCond) #420 observations in each interaction
MemData$CombineCond <- NULL #only used for checking that observations are spread evenly
rm(PartData) #Demographics are not part of the analysis
#in anticipation of later need. This gets a data set where participants confidence is over 60 (i.e. they weren't blindly guessing)
#this requires splitting the data sets into detail and inferences, so only taking applicable columns
ConfidentDataI <- MemData[MemData$InferJOL>60,c(1:2,5,9:10)]
ConfidentDataD <- MemData[MemData$DetJOL>60,c(1,5:6,9:11)]



#check out the tidiness
head(MemData)

####A model of Adjusted Accuracy####
#The Adjust accuracy score is the participant's confidence in their answer if they're correct and 100 - their confidence if they're incorrect
#Assumption testing
hist(MemData$IJolCalibrAns) #not good
hist(MemData$DJolCalibrAns) #a little better
#With bounded data, too much data at the edge of the bounds violates linear regression, beta regression is needed instead
lmer.full <- lmer(IJolCalibrAns~OrderofClauses*ConnectLoc+(1|ID)+(1|QuestNum),data=MemData)
plot(lmer.full) #There's a linear relationship among the residuals because the data is bounded
lmer.fullD <- lmer(DJolCalibrAns~OrderofClauses*ConnectLoc+(1|ID)+(1|QuestNum),data=MemData)
plot(lmer.fullD) #Same deal.
rm(lmer.full)
rm(lmer.fullD) #removing these as there's no point in moving forward with any analysis on these models
####Do the means suggest that guessing was an issue?####
GenMeans <- MemData %>% group_by(ConnectLoc,OrderofClauses) %>%
  summarise(Infer = mean(InferAns)*100,
            SEInfer = std.error(InferAns)*100,#Mean correct for Inference questions
            Det = mean(DetAns)*100,#Mean correct for Detail questions
            SEDet = std.error(DetAns)*100,
            InferAdj=mean(IJolCalibrAns, na.rm=TRUE), #Mean correct for Inference questions
            SEInferAdj = std.error(IJolCalibrAns),
            DetAdj=mean(DJolCalibrAns),
            SEDetAdj=std.error(DJolCalibrAns),
            InferDif = Infer-InferAdj,
            DetDif = Det-DetAdj) #Mean correct for Detail questions
GenMeans$OrdLoc <- paste(GenMeans$OrderofClauses,GenMeans$ConnectLoc)
GenMeans$OrderofClauses <- NULL
GenMeans$ConnectLoc <- NULL
GenMeans1 <- data.frame(gather(GenMeans[,c("Infer","Det","InferAdj","DetAdj")],AccuracyType,Mean),GenMeans$OrdLoc)
#Means show that the correct accuracy is slightly lower than overall accuracy but does not change the pattern of results
GenMeans2 <- data.frame(gather(GenMeans[,c("SEInfer","SEDet","SEInferAdj","SEDetAdj")],AccuracyType,StdError),GenMeans$OrdLoc)
GenMeans2$AccuracyType <- gsub("SE","",GenMeans2$AccuracyType)
GenMeans3 <- merge(GenMeans1,GenMeans2)
GenMeansDif <- data.frame(gather(GenMeans[,c("InferDif","DetDif")],AccuracyType,Mean),GenMeans$OrdLoc)
ggplot(GenMeans3,aes(AccuracyType,Mean,fill=GenMeans.OrdLoc)) + geom_bar(stat="identity",position="dodge") + 
  geom_errorbar(aes(ymin=GenMeans3$Mean-GenMeans3$StdError,ymax=GenMeans3$Mean+GenMeans3$StdError),width=.2,position=position_dodge(.9))
#At first glance, the adjusted accuracy is lower but doesn't seem to vary by condition - the effect of guessing is evenly spread across conditions
ggplot(GenMeansDif,aes(AccuracyType,Mean,fill=GenMeans.OrdLoc)) + geom_bar(stat="identity",position="dodge") + scale_y_continuous(name="Difference bt raw acc and acc adjusted",breaks= seq(0,10,by=1),limits=c(0,10))+
#maybe there is some reason to worry about the effect of guessing being difference across conditions
rm(GenMeans)
rm(GenMeans1)
rm(GenMeans2)
rm(GenMeans3)
rm(GenMeansDif)

####Model set up for Inference Logistic Mixed Effects Model####
#Assumptions
#Repeated measures data warrants mixed effects model as assumption of independence of variables is not met
#1. Binomial
hist(MemData$InferAns)
#2. Large Sample
MemData$InferAns <- as.factor(MemData$InferAns)
summary(MemData$InferAns)
1350/3690 #= .3659
(10*3)/.3659 #= 81.9896; actual 5040
#3. No Multicollinearity among IVs (contrasts set intercept to 0. Necessary to avoid dummy coding in order to find
#main effects of Connective Presence and then Location)
contrasts(MemData$OrderofClauses) <- c(.5,-.5)
colnames(contrasts(MemData$OrderofClauses)) <- c("CE") #CE for cause-effect
contrasts(MemData$ConnectLoc) <- cbind(c(1/3,1/3,-2/3),c(1/2,-1/2,0))
colnames(contrasts(MemData$ConnectLoc)) <- c("Presence","BegLocation")
#4. Independence of residuals - checked through evaluating model fit
#5. Linearity of independent varialbes and log odds - checked through evaluating model fit

#Inference Model
#Lower AIC with only random intercepts
glmer.full <- glmer(InferAns~OrderofClauses*ConnectLoc+(1|ID)+(1|QuestNum), data=MemData,family=binomial)

glm_prob_df = data.frame(predict(glmer.full,type="response"))
colnames(glm_prob_df) = c('predicted_prob')
glm_prob_df$index = seq(1, nrow(glm_prob_df))
ggplot(glm_prob_df, aes(index,predicted_prob)) + geom_point() + xlab('observation') + ylab('predicted response probability')
contrasts(MemData$InferAns)
threshold = .5
glm_prob_df$predicted_binary = rep("0",nrow(glm_prob_df))
glm_prob_df$predicted_binary[glm_prob_df$predicted_prob>threshold]="1"
confusion_df = data.frame(glm_prob_df$predicted_binary,MemData$InferAns)
colnames(confusion_df) = c('predicted','actual')
table(confusion_df) #model overpredicts correct responses
mean(confusion_df$predicted == confusion_df$actual) #.7506, decent accuracy
#again with the intercept only model
glmer.int <- glmer(InferAns~1+(1|ID)+(1|QuestNum), data=MemData,family=binomial)
glm_prob_df = data.frame(predict(glmer.int,type="response"))
colnames(glm_prob_df) = c('predicted_prob')
glm_prob_df$index = seq(1, nrow(glm_prob_df))
ggplot(glm_prob_df, aes(index,predicted_prob)) + geom_point() + xlab('observation') + ylab('predicted response probability')
glm_prob_df$predicted_binary = rep("0",nrow(glm_prob_df))
glm_prob_df$predicted_binary[glm_prob_df$predicted_prob>threshold]="1"
confusion_df = data.frame(glm_prob_df$predicted_binary,MemData$InferAns)
colnames(confusion_df) = c('predicted','actual')
table(confusion_df) #model overpredicts correct responses
mean(confusion_df$predicted == confusion_df$actual) #.7504, barely smaller accuracy
#Most of the effects can be explained by the random effects. This model isn't good for prediction
rm(glm_prob_df)
rm(confusion_df)
rm(glmer.int)
####kfold cross validation####
#kfold for 1 question (84 observations) + 1 full participant (60 observations) - 1 question for overlap: 143 obs per trial
#8580 iterations
PTOT= as.data.frame(x=NA)
colnames(PTOT) <- "x"

for (j in c(1:60))
  for (i in c(1:8,10:85))
  {
    ##Data that will be predicted
    Test=MemData[MemData$ID==i|MemData$QuestNum==j,c(1,2,5,9,10)]
    ###To train the model
    Train=MemData[MemData$ID!=i&MemData$QuestNum!=j,c(1,2,5,9,10)]
    M1 <- glmer(InferAns~OrderofClauses*ConnectLoc+(1|ID)+(1|QuestNum), data=Train,family=binomial)
    P1=as.data.frame(predict(M1, Test,type="response",allow.new.levels = TRUE))
    colnames(P1) <- "predicted_prob"
    P1$predicted_binary = rep("0",nrow(Test))
    P1$predicted_binary[P1$predicted_prob>threshold]="1"
    confusion_df = data.frame(P1$predicted_binary,Test$InferAns)
    colnames(confusion_df) = c('predicted','actual')
    Acc <- mean(confusion_df$predicted == confusion_df$actual)
    PTOT = rbind(PTOT,Acc)
    print(cbind(c("Question:",j),c("ID",i)))
  }
AvgAcc <- mean(PTOT$x,na.rm=TRUE)
SEAvgAcc <- std.error(PTOT$x,na.rm=TRUE)
#The model is as accurate on average for test sets as it is on the data in trained on
rm(Test)
rm(Train)
####Look at the model####
#Prediction isn't great, but there are some inferences to be made
summary(glmer.full)
#Betas and CIs
Labels <- c("CE vs EC","Connect Present","Connect Begin","Connect Present x Order","Connect Begin x Order")
Beta <- c(exp(.2019),exp(.337),exp(.0117),exp(-.01),exp(.1432))
CI1 <- c(exp(.2019-1.96*.0675),exp(.337-1.96*.0702),exp(.0117-1.96*.00843),exp(-.01-1.96*.1403),exp(.1432-1.96*.1686))
CI2 <- c(exp(.2019+1.96*.0675),exp(.337+1.96*.0702),exp(.0117+1.96*.00843),exp(-.01+1.96*.1403),exp(.1432+1.96*.1686))

####Bayes Factors####
MemData$InferAns.Num <- as.numeric(ifelse(MemData$InferAns==0,0,1)) #brm requires numeri
full_BF = brm(InferAns.Num~OrderofClauses*ConnectLoc+(1|ID)+(1|QuestNum),data=MemData,family=binomial,save_all_pars = TRUE)
Null_BF = update(full_BF,formula = ~.-OrderofClauses*ConnectLoc)
NoInt_BF = update(full_BF,formula = ~.-OrderofClauses:ConnectLoc)
NoOrd_BF = brm(InferAns.Num~ConnectLoc+(1|ID)+(1|QuestNum),data=MemData,family=binomial,save_all_pars = TRUE)
NoConn_BF = brm(InferAns.Num~OrderofClauses+(1|ID)+(1|QuestNum),data=MemData,family=binomial,save_all_pars = TRUE)

BF.Null = bayes_factor(full_BF,Null_BF) 
BF.Null #H1 is more likely than H0
BF.Null <- 202.9763
BF.NoInt = bayes_factor(full_BF,NoInt_BF) 
BF.NoInt #H without the intercept is more likely than with it
BF.NoInt <- .1266
BF.NoOrd = bayes_factor(full_BF,NoOrd_BF) 
BF.NoOrd #H without OrderofClauses included is more likely than with it
BF.NoOrd <- .6836
Null.NoOrd = bayes_factor(NoOrd_BF,Null_BF) 
Null.NoOrd #H with OrderofClauses is more likely than null
Null.NoOrd <- 284.4162
BF.NoConn = bayes_factor(full_BF,NoConn_BF) 
BF.NoConn #H1 is more liekly than H0
BF.NoConn <- 25.2175
Bayes <- c(BF.NoOrd,BF.NoConn,BF.NoConn,BF.NoInt,BF.NoInt)
BayesNull <- c(Null.NoOrd,NA,NA,NA,NA)

Descriptive <- c("We can accept that the odds of a correct answer were 1.22 times greater for that were easier to read (CE)",
                 "We can accept that the odds of a correct answer were 1.4 times greater when the passage had a connective",
                 "While the CI does not include 0, the p value is > .05. We cannot reject the H0 or confirm it as no Bayes factor was retrieved for this level of the factor",
                 "We can accept the H0 that there are no interaction effects",
                 "We can accept the H0 that there are no interaction effects")
Null <- data.frame("Full-Null",NA,NA,NA,NA,BF.Null,"We can accept H1 that the full model is better than one with only random intercepts")
colnames(Null) <- c("Labels","Beta","CI1","CI2","Bayes","BayesNull","Descriptive")

####Create a dataframe to present all of this####
BetaDF <- data.frame(Labels,Beta,CI1,CI2,Bayes,BayesNull,Descriptive)
BetaDF <- rbind(BetaDF,Null)
ResultsI <- as.matrix(BetaDF)
ResultsI <- as.table(ResultsI)
ResultsI
rm(BetaDF)
rm(BetaDF.NoConn)
rm(BetaDF.Null)
rm(BetaDF.NoOrd)
rm(BetaDF.Int)
rm(Bayes)
rm(BayesNull)
rm(Descriptive)
rm(Null)
rm(Labels)
rm(Beta)
rm(CI1)
rm(CI2)
####Checking on the guess work####
contrasts(ConfidentDataI$OrderofClauses) <- c(.5,-.5)
contrasts(ConfidentDataI$ConnectLoc) <- cbind(c(1/3,1/3,-2/3),c(.5,-.5,0))
guess_glmer <- glmer(InferAns~OrderofClauses*ConnectLoc+(1|ID)+(1|QuestNum),data=ConfidentDataI,family=binomial)
summary(guess_glmer)
summary(glmer.full)
rm(guess_glmer)
rm(glmer.full)
#Only makes the effects larger

####Model set up for Detail Logistic Mixed Effects Model####
#Assumptions
#Repeated measures data warrants mixed effects model as assumption of independence of variables is not met
#1. Binomial
hist(MemData$DetAns)
MemData$DetAns <- as.factor(MemData$DetAns)
summary(MemData$DetAns)
#2-3 see assumptions check for Inferences
#4. Independence of residuals - checked through evaluating model fit
#5. Linearity of independent varialbes and log odds - checked through evaluating model fit

#Check that Detail Loc can be ommitted as a variable
AccCheck <- MemData %>% group_by(DetailLoc,DetAns) %>% summarise(Acc <- n())
#They are virtually identical in the means. These were counterbalanced to insure they weren't an issue and they are not
#They don't need to be included in the model
#Lower AIC with only random intercepts
#Prediction Model
glmer.fullD <- glmer(DetAns~OrderofClauses*ConnectLoc+(1|ID)+(1|QuestNum), data=MemData,family=binomial)
glm_prob_df = data.frame(predict(glmer.fullD,type="response"))
colnames(glm_prob_df) = c('predicted_prob')
glm_prob_df$index = seq(1, nrow(glm_prob_df))
ggplot(glm_prob_df, aes(index,predicted_prob)) + geom_point() + xlab('observation') + ylab('predicted response probability')
glm_prob_df$predicted_binary = rep("0",nrow(glm_prob_df))
glm_prob_df$predicted_binary[glm_prob_df$predicted_prob>threshold]="1"
confusion_df = data.frame(glm_prob_df$predicted_binary,MemData$DetAns)
colnames(confusion_df) = c('predicted','actual')
table(confusion_df) #model overpredicts correct responses
mean(confusion_df$predicted == confusion_df$actual) #.6639, decent accuracy
#And again with only the intercept
glmer.fullD <- glmer(DetAns~(1|ID)+(1|QuestNum), data=MemData,family=binomial)
glm_prob_df = data.frame(predict(glmer.fullD,type="response"))
colnames(glm_prob_df) = c('predicted_prob')
glm_prob_df$index = seq(1, nrow(glm_prob_df))
ggplot(glm_prob_df, aes(index,predicted_prob)) + geom_point() + xlab('observation') + ylab('predicted response probability')
glm_prob_df$predicted_binary = rep("0",nrow(glm_prob_df))
glm_prob_df$predicted_binary[glm_prob_df$predicted_prob>threshold]="1"
confusion_df = data.frame(glm_prob_df$predicted_binary,MemData$DetAns)
colnames(confusion_df) = c('predicted','actual')
table(confusion_df) #model overpredicts correct responses
mean(confusion_df$predicted == confusion_df$actual) #.6657, Accuracy is actually a little better for the model with only intercepts
rm(glm_prob_df)
rm(confusion_df)
####K-folds####
#kfold for 1 question (84 observations) + 1 full participant (60 observations) - 1 question for overlap: 143 obs per trial
#8580 iterations
PTOTD= as.data.frame(x=NA)
colnames(PTOTD) <- "x"

for (j in c(1:60))
  for (i in c(1:8,10:85))
  {
    ##Data that will be predicted
    Test=MemData[MemData$ID==i|MemData$QuestNum==j,c(1,5,6,9,10)]
    ###To train the model
    Train=MemData[MemData$ID!=i&MemData$QuestNum!=j,c(1,5,6,9,10)]
    M1 <- glmer(DetAns~OrderofClauses*ConnectLoc+(1|ID)+(1|QuestNum), data=Train,family=binomial)
    P1=as.data.frame(predict(M1, Test,type="response",allow.new.levels = TRUE))
    colnames(P1) <- "predicted_prob"
    P1$predicted_binary = rep("0",nrow(Test))
    P1$predicted_binary[P1$predicted_prob>threshold]="1"
    confusion_df = data.frame(P1$predicted_binary,Test$DetAns)
    colnames(confusion_df) = c('predicted','actual')
    Acc <- mean(confusion_df$predicted == confusion_df$actual)
    PTOTD = rbind(PTOTD,Acc)
    print(cbind(c("Question: ",j),c("ID",i)))
  }
AvgAcc <- mean(PTOTD$x,na.rm=TRUE)
SEAvgAcc <- std.error(PTOTD$x,na.rm=TRUE)
rm(Test)
rm(Train)
rm(confusion_df)
#The model is as accurate on average for test sets as it is on the data in trained on
####Bayes Factors####
MemData$DetAns.Num <- as.numeric(ifelse(MemData$DetAns==0,0,1)) #brm requires numeric
full_BF = brm(DetAns.Num~OrderofClauses*ConnectLoc+(1|ID)+(1|QuestNum),data=MemData,family=bernoulli,save_all_pars = TRUE)
Null_BF = update(full_BF,formula = ~.-OrderofClauses*ConnectLoc)
NoInt_BF = update(full_BF,formula = ~.-OrderofClauses:ConnectLoc)
NoOrd_BF = brm(DetAns.Num~ConnectLoc+(1|ID)+(1|QuestNum),data=MemData,family=bernoulli,save_all_pars = TRUE)
NoConn_BF = brm(DetAns.Num~OrderofClauses+(1|ID)+(1|QuestNum),data=MemData,family=bernoulli,save_all_pars = TRUE)

BF.Null = bayes_factor(full_BF,Null_BF) 
BF.Null #H1 is more likely than H0
BF.Null <- 202.9763
BF.NoInt = bayes_factor(full_BF,NoInt_BF) 
BF.NoInt #H without the intercept is more likely than with it
BF.NoInt <- .1266
BF.NoOrd = bayes_factor(full_BF,NoOrd_BF) 
BF.NoOrd #H without OrderofClauses included is more likely than with it
BF.NoOrd <- .6836
Null.NoOrd = bayes_factor(NoOrd_BF,Null_BF) 
Null.NoOrd #H with OrderofClauses is more likely than null
Null.NoOrd <- 284.4162
BF.NoConn = bayes_factor(full_BF,NoConn_BF) 
BF.NoConn #H1 is more liekly than H0
BF.NoConn <- 25.2175
Bayes <- c(BF.NoOrd,BF.NoConn,BF.NoConn,BF.NoInt,BF.NoInt)
BayesNull <- c(Null.NoOrd,NA,NA,NA,NA)

Descriptive <- c("We can accept that the odds of a correct answer were 1.22 times greater for that were easier to read (CE)",
                 "We can accept that the odds of a correct answer were 1.4 times greater when the passage had a connective",
                 "While the CI does not include 0, the p value is > .05. We cannot reject the H0 or confirm it as no Bayes factor was retrieved for this level of the factor",
                 "We can accept the H0 that there are no interaction effects",
                 "We can accept the H0 that there are no interaction effects")
Null <- data.frame("Full-Null",NA,NA,NA,NA,BF.Null,"We can accept H1 that the full model is better than one with only random intercepts")
colnames(Null) <- c("Labels","Beta","CI1","CI2","Bayes","BayesNull","Descriptive")

####Create a dataframe to present all of this####
BetaDF <- data.frame(Labels,Beta,CI1,CI2,Bayes,BayesNull,Descriptive)
BetaDF <- rbind(BetaDF,Null)
BetaDFTD <- as.matrix(BetaDF)
BetaDFTD <- as.table(BetaDFT)
BetaDFTD

####Checking on the guess work####
contrasts(ConfidentDataD$OrderofClauses) <- c(.5,-.5)
contrasts(ConfidentDataD$ConnectLoc) <- cbind(c(1/3,1/3,-2/3),c(.5,-.5,0))
guess_glmer <- glmer(DetAns~OrderofClauses*ConnectLoc+(1|ID)+(1|QuestNum),data=ConfidentDataD,family=binomial)
summary(guess_glmer)
#Only makes the effects larger

####Final Plot####
MbyOrdLoc <- MemData %>%
  group_by(OrderofClauses, ConnectLoc) %>%
  summarise(IAccM = mean(InferAns.Num),
    IAccSE = std.error(InferAns.Num),
    DAccM = mean(DetAns.Num),
    DAccSE = std.error(DetAns.Num)
  )
MbyOrdLoc1 <- gather(MbyOrdLoc[,c("ConnectLoc","OrderofClauses","IAccM","DAccM")],AccType,Mean,-OrderofClauses,-ConnectLoc)
MbyOrdLoc2 <- gather(MbyOrdLoc[,c("ConnectLoc","OrderofClauses","IAccSE","DAccSE")],AccType,StdErr,-OrderofClauses,-ConnectLoc)
MbyOrdLoc2$AccType <- gsub("SE","M",MbyOrdLoc2$AccType)
MbyOrdLoc3 <- merge(MbyOrdLoc1,MbyOrdLoc2)
labelsAcc <- c(DAccM = "Detail Memory", IAccM = "Inference Memory")
ggplot(MbyOrdLoc3,aes(OrderofClauses,Mean,fill=factor(ConnectLoc,labels = c("Beginning\n- Because\n- The reason\n", "Middle\n- so\n- because\n","\nNo\nConnective\n\n")))) + 
  facet_grid(. ~ AccType, labeller=labeller(AccType=labelsAcc))+theme(strip.background=element_rect(fill="#F5F5F5")) + 
  geom_bar(stat="identity",position="dodge") +
  geom_errorbar(aes(ymin=Mean-StdErr,ymax=Mean+StdErr),width=.2,position=position_dodge(.9)) +
  labs(title="Memory Probe Results",x="Clause Order", y="Accuracy",
       fill="Connective\nLocation\n")+scale_fill_manual(values=c("#F80000", "#700000", "#A9A9A9"))+
  scale_x_discrete(labels=c("Cause\nEffect","Effect\nCause"))+
  theme_bw()+
  theme(axis.text.x = element_text(size = 20,color="black"),strip.text.x = element_text(size = 26, colour = "black"),
        axis.title.x = element_text(size = 30, color="black"),axis.title.y=element_text(size=30, color="black"),
        plot.title=element_text(size=44,color="black",face="bold"),plot.subtitle=element_text(size=20),
        legend.title = element_text(size=25),legend.text=element_text(size=20),
        axis.text.y = element_text(size = 18,color="black"))+
  theme(plot.title = element_text(hjust = 0.5))

