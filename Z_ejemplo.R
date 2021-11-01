################################################################################
# BigFive.5q Project
# A model to predict 45 questions based in only 5 answered
# Author: Juan Eloy Suarez
################################################################################


## =============================================================================
## =============================================================================
## Initialize environment
## =============================================================================
## =============================================================================
#
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(gtools)) install.packages("gtools", repos = "http://cran.us.r-project.org")
if(!require(psych)) install.packages("psych", repos = "http://cran.us.r-project.org")
if(!require(recommenderlab)) install.packages("recommenderlab", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(extraDistr)) install.packages("extraDistr", repos = "http://cran.us.r-project.org")
if(!require(tinytex)) tinytex::install_tinytex()  # For RMarkdown .PDF generation

library(tidyverse)
library(patchwork)
library(lubridate)
library(ggrepel)
library(ggplot2)
library(patchwork)
library(ggthemes)
library(gridExtra)
library(caret)
library(corrplot)
library(extraDistr)

library(recommenderlab)
library(psych)
library(gtools)
library(matrixStats)

options(digits=5)

Sys.setlocale("LC_TIME", "english")


## =============================================================================
## =============================================================================
## Load dataset
## =============================================================================
## =============================================================================
#
##########################################################
# Reading input data files 
##########################################################
# 
# Data are available in RDS format in directory ./data_source after being loaded by previous code ./load_data.R. This R native format allows a very significant reduction in input data size
df <- readRDS("./data_source/BFtests.rds")
dictionary <- readRDS("./data_source/dictionary.rds")
head(df)
dictionary





## =============================================================================
## =============================================================================
## Exploratory Data Analysis
## =============================================================================
## =============================================================================


##########################################################
# Calculate scores for each observation based on its questions
# Version AS-IS: We score before change of "reverted questions"
##########################################################
# Scoring analysis requires calculation of each trait score based on its question's answers. Process is to obtain the average within group (taking sign for reverted questions into account), and the obtain the percentile of that value (using as reference the whole population of answers) 
# This list identifies each column to its corresponding group and informs of the sign (to revert if necessary) based in our previous analysis of correlation
keys.list <- list(openess = c("OPN1","-OPN2","OPN3","-OPN4","OPN5","-OPN6","OPN7","OPN8","OPN9","OPN10"), 
                  conscienciousness = c("CSN1","-CSN2","CSN3","-CSN4","CSN5","-CSN6","CSN7","-CSN8","CSN9","CSN10"), 
                  extroversion = c("EXT1","-EXT2","EXT3","-EXT4","EXT5", "-EXT6", "EXT7", "-EXT8", "EXT9", "-EXT10"), 
                  agreeability = c("-AGR1", "AGR2", "-AGR3", "AGR4", "-AGR5", "AGR6", "-AGR7", "AGR8", "AGR9", "AGR10"), 
                  natural_reactions = c("EST1","-EST2","EST3","-EST4","EST5", "EST6", "EST7", "EST8", "EST9", "EST10"))
# We use psych::scoreItems to calculate scores (i.e. mean per group taking sign into account)
# See: https://www.rdocumentation.org/packages/psych/versions/2.1.3/topics/scoreItems
scoresfile <- scoreFast(keys.list, df) #scoresfile <- scoreVeryFast(keys.list, df)
# Add calculated columns (scores)
df_scored_preReversion <- cbind(df, scoresfile)
# Once obtained the observation average per group, we need the p-value (0 to 100) per observation in group scope
P1 = ecdf(scoresfile[,1])    # P is a function giving the empirical CDF of X
P2 = ecdf(scoresfile[,2])
P3 = ecdf(scoresfile[,3])
P4 = ecdf(scoresfile[,4])
P5 = ecdf(scoresfile[,5])
# Store a rounded percentage (percentile) as resulting score for each trait
df_scored_preReversion <- df_scored_preReversion %>% mutate(
  `openess-P` = round(P1(`openess-A`)*100), 
  `conscienciousness-P` = round(P2(`conscienciousness-A`)*100), 
  `extroversion-P` = round(P3(`extroversion-A`)*100), 
  `agreeability-P` = round(P4(`agreeability-A`)*100), 
  `natural_reactions-P` = round(P5(`natural_reactions-A`)*100) )
###########################
# Scores visualization
# First, we take a look at the different indexes against each other, to see what sort of interrelationships they might have.
df_scored_preReversion %>% ggplot() +
  theme_bw() +
  geom_density(aes(x=`extroversion-A`, fill = "Extroversion"), alpha=.2)+
  geom_density(aes(x=`natural_reactions-A`, fill = "Natural Reactions"), alpha = .2)+
  geom_density(aes(x=`agreeability-A`, fill = "Agreeability"), alpha = .2)+
  geom_density(aes(x=`conscienciousness-A`, fill = "Concienciousness"), alpha = .2)+
  geom_density(aes(x=`openess-A`, fill = "Openess"), alpha = .2)+
  labs(title="Distributions of Scores", x="Response Score")


##########################################################
# Test reliability in each personality trait
##########################################################
# Reliability (internal consistency)
# Questions of the test are grouped in 5 categories (personality trait). We can expect a high correlation (positive or negative) among the 10 questions in each group. We also expect also reliability alfa vualues. The Cronbach’s alpha coefficient measures reliability, or internal consistency, to see if multiple-question Likert scale surveys are reliable. Cronbach’s alpha will tell us how closely related a set of test items are as a group.
# https://www.rdocumentation.org/packages/psych/versions/2.1.3/topics/alpha
#
# Preparation, questions structure
# To prepare an analysis per "traits" (each one includes groups of questions), we create a list of 5 concepts (groups of questions per personality dimension)
extroversion = dictionary[c(1:10), "ID"]
natural_reactions = dictionary[c(11:20), "ID"]
agreeability = dictionary[c(21:30), "ID"]
conscienciousness = dictionary[c(31:40), "ID"]
openess = dictionary[c(41:50), "ID"]
buckets = list("extroversion" = extroversion,
               "natural_reactions" = natural_reactions,
               "agreeability" = agreeability,
               "conscienciousness" = conscienciousness,
               "openess"= openess)
# Groups analysis
# For each concept, let's calculate alfa to check "internal consistency" of its questions
# The result also identify which variables should be inverted (negative correlation)
questionsSigns <- numeric()
traitsAlphas <- data.frame()
for(i in 1:length(buckets)){
  questiondf <- df[, colnames(df) %in% unlist(buckets[i])]
  cronbach <- psych::alpha(questiondf, check.keys=TRUE)
  questionsSigns <- c(questionsSigns, cronbach$keys)
  traitsAlphas <- rbind(traitsAlphas, cronbach$total[,1:3])
}

# Reverted questions: signs indicating if questions correlate directly or "reverted"
questionsSigns

# Cronbach's alpha per trait's questions
# We want to visualize alphas: value 0.8 >= alpha > 0.9 is generally interpreted "Good Internal Consistency", and alfa >= 0.9 as "Excellent Internal consistency"
rownames(traitsAlphas) <- NULL
# Assign question's group
traitsAlphas <- cbind(Group = paste("Questions_", seq(1,5,1), "X", sep=""), traitsAlphas)
tmpAvg <- mean(traitsAlphas$raw_alpha)
traitsAlphas %>%
  ggplot(aes(Group, raw_alpha)) +
  geom_point(color="blue") +
  geom_hline(yintercept=0.8, linetype="dashed", color = "green", size=2) +
  geom_hline(yintercept=0.9, linetype="dashed", color = "green", size=2) +
  geom_hline(yintercept=tmpAvg, linetype="dashed", color = "gray", size=1) +
  geom_label_repel(aes(label = Group),
                   label.size = NA,
                   fill = "transparent",
                   box.padding   = 0.65,
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  ggtitle("Tests Reliability") +
  xlab(element_blank()) +
  ylab("Cronbach's alpha") +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(color="blue", size=14, face="bold"))


##########################################################
# Correlation of the answers to questions
##########################################################
# We calculate correlation among all questions. Some questions are reverted, but this affects only to the sign of the correlation.
# Correlation calculation. Calculate global correlation matrix to see full landscape instead of just per trait
corAllQuestions <- cor(df[,2:51])
# Sample of values (two per trait)
head(corAllQuestions[,c(1,11,21,31,41)])
# Significance calculation. This function calculates de significance test associated to the corrrelation matrix
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
p.mat <- cor.mtest(corAllQuestions)  # significance test
# Correlation within same group:
# Questions explitly associated to each "trait" strongly correlate, i.e. all of them are related because all of them explain the final value of the score for its specific group (trait). 
# We see now two examples two different traits (groups). Correlation is clearly high within same group.
corrplot(corAllQuestions[41:50,41:50], 
         method = "circle",
         type = "lower", 
         order = "alphabet", 
         tl.col = "black", tl.srt = 0, tl.cex = 1, 
         title =  "\n\n Trait: Openess", 
         p.mat = p.mat, sig.level = 0.01
)
corrplot(corAllQuestions[31:40,31:40], 
         method = "circle",
         type = "lower", 
         order = "alphabet", 
         tl.col = "black", tl.srt = 0, tl.cex = 1, 
         title =  "\n\n Trait: Conscientioussness", 
         p.mat = p.mat, sig.level = 0.01
)
corrplot(corAllQuestions[1:10,1:10], 
         method = "circle",
         type = "lower", 
         order = "alphabet", 
         tl.col = "black", tl.srt = 0, tl.cex = 1, 
         bg = "White", 
         title =  "\n\n Trait: Extraversion", 
         p.mat = p.mat, sig.level = 0.01
)
corrplot(corAllQuestions[21:30,21:30], 
         method = "circle",
         type = "lower", 
         order = "alphabet", 
         tl.col = "black", tl.srt = 0, tl.cex = 1, 
         title =  "\n\n Trait: Agreeableness", 
         p.mat = p.mat, sig.level = 0.01
)
corrplot(corAllQuestions[11:20,11:20], 
         method = "circle",
         type = "lower", 
         order = "alphabet", 
         tl.col = "black", tl.srt = 0, tl.cex = 1, 
         title =  "\n\n Trait: Natural Reactions", 
         p.mat = p.mat, sig.level = 0.01
)

# Correlation with other groups. However, we see also some other significant correlations of some questions with questions that "belong" to different traits. Since our challenge is preciselly to use few (only five) questions to explain as mach as possible of the result for all traits, it will be useful to use these correlations to select what specific questions we show to get answer. 
# http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram
corrplot(corAllQuestions, 
         method = "color",
         type = "lower",
         order = "alphabet", 
         tl.col = "black", tl.srt = 45, tl.cex = 0.4, 
         bg = "white", 
         p.mat = p.mat, sig.level = 0.01, insig='blank', 
         title =  "\n\n Questions correlation", 
         mar = c(0,0,1,0)
)


##########################################################
# Clean environment before training
##########################################################
rm(questiondf, cronbach)
rm(scoresfile, df_scored_preReversion, keys.list, buckets)






## =============================================================================
## =============================================================================
## Data preparation for modeling
## =============================================================================
## =============================================================================


##########################################################
# Reverted questions treatment
##########################################################
# Our data contains answers to questions considered "reverted". This means that the question is written in a negative way and, for global analysis,  must be scored reverting results recorded.
# This fact is already covered by the generic scoring analysis used for package PSYCH, and just identifying with a minus sign before question Id, function internally reverts answers value. This was used in the EDA part above.
# However, we are going to use functions based in recommendation methods (recommenderLab) based in linear algebra and distances that use "ratings" allways meaning "positive", never "reverted". For this reason, we will, before analysis, revert results of "reverse" questions and when when necessary keep using rgular PSYCH functions, but marking those questions as regular/positive (removing the minus sign)
#
# Revert negative questions of a Likert Scale: (n+1)-x
df <- df %>% mutate(
  OPN2 = abs(6-OPN2), OPN4 = abs(6-OPN4), OPN6 = abs(6-OPN6), 
  CSN2 = abs(6-CSN2), CSN4 = abs(6-CSN4), CSN6 = abs(6-CSN6), CSN8 = abs(6-CSN8), 
  EXT2 = abs(6-EXT2), EXT4 = abs(6-EXT4), EXT6 = abs(6-EXT6), EXT8 = abs(6-EXT8), EXT10 = abs(6-EXT10), 
  AGR1 = abs(6-AGR1), AGR3 = abs(6-AGR3), AGR5 = abs(6-AGR5), AGR7 = abs(6-AGR7), 
  EST2 = abs(6-EST2), EST4 = abs(6-EST4))
# We score AFTER change of "reverted questions"
# Removing minus sign of the list. All questions will be "positive" 
keys.list.allPositive <- list(
  openess = c("OPN1","OPN2","OPN3","OPN4","OPN5","OPN6","OPN7","OPN8","OPN9","OPN10"), 
  conscienciousness = c("CSN1","CSN2","CSN3","CSN4","CSN5","CSN6","CSN7","CSN8","CSN9","CSN10"), 
  extroversion = c("EXT1","EXT2","EXT3","EXT4","EXT5", "EXT6", "EXT7", "EXT8", "EXT9", "EXT10"), 
  agreeability = c("AGR1", "AGR2", "AGR3", "AGR4", "AGR5", "AGR6", "AGR7", "AGR8", "AGR9", "AGR10"), 
  natural_reactions = c("EST1","EST2","EST3","EST4","EST5", "EST6", "EST7", "EST8", "EST9", "EST10"))


##########################################################
# We see mean and standard deviation of each trait's questions
##########################################################
traitsMeans <- c(mean(rowMeans(df[,2:11])), mean(rowMeans(df[,12:21])), mean(rowMeans(df[,22:31])), mean(rowMeans(df[,32:41])), mean(rowMeans(df[,42:51])))
traitsSds <- c(sd(rowMeans(df[,2:11])), sd(rowMeans(df[,12:21])), sd(rowMeans(df[,22:31])), sd(rowMeans(df[,32:41])), sd(rowMeans(df[,42:51])))
names(traitsMeans) <- c("extroversion_EXT", "natural_reactions_EST", "agreeability_AGR", "conscienciousness_CSN", "openess_OPN")
names(traitsSds) <- c("extroversion_EXT", "natural_reactions_EST", "agreeability_AGR", "conscienciousness_CSN", "openess_OPN")
traitsMeans %>% knitr::kable(digits = 4)
traitsSds %>% knitr::kable(digits = 4)

##########################################################
# Dataset size reduction
##########################################################
# Let reduce dataset size to facilitate development. This step is intended to allow agile analysis in locale environment. Previous EDA has been done using all observations 
nObservsDevelopment <- 5000
set.seed(1, sample.kind="Rounding")
df <- df[sample(nrow(df), nObservsDevelopment), ]


##########################################################
## Save input data for shiny app
##########################################################
# The shiny app is an interactive implementation of this model. It simplifies some parts of the process and shows prediction generated for each questions, as well as the calculated scores. Its purpose is educational and as a demo, it does not store data nor submit specific transaction, i.e. allows simulation with different values.
# For this tool to work we pass reduced version of the dataseet to allow training and online prediction.
BFdata <- as(data.matrix(df[,2:51]), "realRatingMatrix")
saveRDS(BFdata, "./data_source/BFdata.rds")
saveRDS(dictionary, "./data_source/dictionary.rds")


##########################################################
# Separate data in partitions
##########################################################
# Create Train and Test partitions: Validation (Test) set will be 20% of data
devReduction <- 0.2 # Percentage of original data we extract for development
set.seed(1, sample.kind="Rounding")
test_index <- sample(c(1:nrow(df)), (nrow(df) * devReduction), replace = FALSE)
BF_train <- df[-test_index,]
BF_test <- df[test_index,]
totRowsValidation <- nrow(BF_test) # number of observations to predict
rm(test_index)




## =============================================================================
## =============================================================================
## Training and prediction
## =============================================================================
## =============================================================================

##########################################################
# Accuracy measurement: metrics of prediction success
##########################################################
# We need to define clearly how our model increases the performance of prediction. The first step is define clear measures of the accuracy it reaches. The challenge of the project is to predict Scores for each one of the five personality traits, but these scores directly depend on the answers to 50 questions (10 per trait) where we only know real value of five of them. Thus, our accuracy will depend on how close we are to the real Score fro each trait, which is originally expressed in percentile. Since this measure can be understood as approximate/soft, we assume that, for a specific trait, hitting (predicting) the correct quartile could be considered as reasonable good result. In similar terms, hitting same half (high-low) is also -though less- a good result. Looking at all five trait as a set, we will also establish as a good result to correctly predict (high-low) most traits, i.e. three or more of the five. So, our accuracy metrics will be defined as:
# - "Hits quartile" for a trait: Score for a specific trait predicts correct quartile (1-25, 26-50, 51-75, 76-100)
# - "Hits quartile all traits": average of "Hits quartile" for all traits of an observation (test) 
# - "Hits HighLow" for a trait: Score for a specific trait predicts correct half (1-50, 51-100)
# - "Hits HigLow all traits": average of "Hits quartile" for all traits of an observation (test) 
# - "3+ hits HighLow" for all traits: Scores of three or more traits predict correctly halves 

# ==============================
# Accuracy measurement: metrics of prediction success
# As reference for accuracy improvement, we will estimate what the Scores would be if using just a random criteria for predicting answers to each question. The random distribution of answers to 50 questions with 5 possible answers same probability (1/5) follow a binomial (Bernouilli) pattern. However, due to the calculations required for our accuracy indicators, we will code a Montecarlo simulation of a random selection, which will allow us to simulate results. These accuracies will be considered then as "base reference" for further improvements during modeling process.
# This function adjusts random score taking into account that answers of one of every 10 question is known. For simplicity, we assume a linear improvement of accuracy for 1/10 and correct by chance of randomly hitting (1/5) 
adjustScore <- function(x) {
  x + (4/5)*((realScores - x) / 10)
}
# Generate a real result for our simulation
set.seed(1, sample.kind="Rounding")
realScores <- sample(c(0:100), 5, replace = TRUE)
realQuartiles <- 1+floor(realScores/25)
realHalves <- 1+floor(realScores/50)
# iterate simulated results obtained randomly
B <- 10000
estimAccuracy <- NULL
for (b in c(1:B)) {
  predictedScores <- sample(c(0:100), 5, replace = TRUE)
  predictedScores <- round(adjustScore(predictedScores),0)
  predictedQuarters <- 1+floor(predictedScores/25)
  predictedHalves <- 1+floor(predictedScores/50)
  hitsQuartile <- (realQuartiles == predictedQuarters)
  hitsHiLo <- (realHalves == predictedHalves)
  mostHitHiLo <- (sum(hitsHiLo) >= 3)
  estimAccuracy <- rbind(estimAccuracy, c(hitsQuartile, hitsHiLo, mostHitHiLo))
}
colnames(estimAccuracy) <- c(
  c("hitsQuartile_O", "hitsQuartile_C", "hitsQuartile_E", "hitsQuartile_A", "hitsQuartile_N"), 
  c("hitsHiLo_O", "hitsHiLo_C", "hitsHiLo_E", "hitsHiLo_A", "hitsHiLo_N"), 
  "MostHitHilo")
estimAccuracy <- colMeans(estimAccuracy)
# We just obtained accuracies for each category
estimAccuracy
# Create a dataframe to store results of the analysis
analysis_results <- data_frame(
  Trait="All", Score = estimAccuracy["MostHitHilo"], Accuracy_type = "3+ hits HighLow", Algorithm = "Montecarlo") %>% 
  rbind(
    data_frame(Trait="O_score", Score=estimAccuracy["hitsQuartile_O"], Accuracy_type = "Hits quartile", Algorithm = "Montecarlo"), 
    data_frame(Trait="C_score", Score=estimAccuracy["hitsQuartile_C"], Accuracy_type = "Hits quartile", Algorithm = "Montecarlo"), 
    data_frame(Trait="E_score", Score=estimAccuracy["hitsQuartile_E"], Accuracy_type = "Hits quartile", Algorithm = "Montecarlo"), 
    data_frame(Trait="A_score", Score=estimAccuracy["hitsQuartile_A"], Accuracy_type = "Hits quartile", Algorithm = "Montecarlo"), 
    data_frame(Trait="N_score", Score=estimAccuracy["hitsQuartile_N"], Accuracy_type = "Hits quartile", Algorithm = "Montecarlo"), 
    data_frame(Trait="O_score", Score=estimAccuracy["hitsHiLo_O"], Accuracy_type = "Hits HighLow", Algorithm = "Montecarlo"), 
    data_frame(Trait="C_score", Score=estimAccuracy["hitsHiLo_C"], Accuracy_type = "Hits HighLow", Algorithm = "Montecarlo"), 
    data_frame(Trait="E_score", Score=estimAccuracy["hitsHiLo_E"], Accuracy_type = "Hits HighLow", Algorithm = "Montecarlo"), 
    data_frame(Trait="A_score", Score=estimAccuracy["hitsHiLo_A"], Accuracy_type = "Hits HighLow", Algorithm = "Montecarlo"), 
    data_frame(Trait="N_score", Score=estimAccuracy["hitsHiLo_N"], Accuracy_type = "Hits HighLow", Algorithm = "Montecarlo"), 
    data_frame(Trait="All", Score=mean(estimAccuracy[1:5]), Accuracy_type = "Hits quartile", Algorithm = "Montecarlo"), 
    data_frame(Trait="All", Score=mean(estimAccuracy[6:10]), Accuracy_type = "Hits HighLow", Algorithm = "Montecarlo")
  )
# Show results (theoretical random estimate with a montecarlo approach) as base reference for next improvements during modelling
analysis_results %>% knitr::kable(digits = 4)


##########################################################
# Questions selection: Minimum Combined Correlation Algorithm (MCCA)
##########################################################
# Our model relies on only five questions, out of 50, to predict the other 45. This means the best possible selection of the five questions we get real aswer for is very important to better predict the others. However, choosing just the same apparent best combination (always the same for given dataset) seems not to be the most open, realistic, rich option, since in case of moving this model into production, would result in a very poor diversity of input rsults, which long term would drive to worse results and lack of diversity. Thus, we will state as premise that, one of the questions to get real answer from, must be randomly selected (seed question), whilst the other four ones can be generated based on expected best results.
# The algorithm we propose to select those other four question can be called "minimum combined correlation method", and consists in selecting those questions having less (absolute) correlation with the seed question and with the other chose questions. We implement therefore a recursive approach based in comparing the average correlation of all pairs of questions for each combination (10) of potential questions including the seed question (10000)
# Minimum combined correlation algorithm:
# Generate all possible combinations of questions (10^5)
potentialQuestionsSets <- as(expand.grid(keys.list.allPositive), "matrix")
head(potentialQuestionsSets) 
# generate first "seed" question
set.seed(1, sample.kind="Rounding")
randomSeedQuestion <- colnames(BF_test[,2:51])[sample(1:50, 1)]
randomSeedQuestion
# Filter only sets of questions containing the initial ramdom seed question
potentialQuestionsSets <- 
  potentialQuestionsSets[which(rowAnys(potentialQuestionsSets == randomSeedQuestion)),]
head(potentialQuestionsSets) 
# Once we know potential combinations of questions to get answer from, we need to calculate which is best for predicting. Our premise will be to get the set with minimum correlation among its pairs. The reason is that low correlation will inform us better of the "difficult" questions where model will be weaker
correlPerSet <- NULL
# Loop each potential set of questions
for (qs in 1:nrow(potentialQuestionsSets)) {
  # generate all pairs of questions (10) within given set
  couplesThisCombination <- combinations(5,2,v=potentialQuestionsSets[qs,])
  pairsCorrelationsThisSet <- NULL
  # now we loop those pairs to get a summary number based on their correlation
  for (c in 1:nrow(couplesThisCombination)) {
    question1 <- couplesThisCombination[c,1]
    question2 <- couplesThisCombination[c,2]
    pairsCorrelationsThisSet <- 
      c(pairsCorrelationsThisSet, abs(corAllQuestions[question1,question2]))
  }
  correlPerSet <- c(correlPerSet, mean(pairsCorrelationsThisSet))
}
# Based on the minimum correlation among its questions, we select a set
potentialQuestionsSets[which.min(correlPerSet),]
# convert to column index for continuing modelling proccess
chosen_questions <- which(colnames(BF_test[,2:51]) %in% potentialQuestionsSets[which.min(correlPerSet),])
chosen_questions


##########################################################
# Loop available recommendation algorithms
##########################################################
# Our approach for predicting 45 answers (item) of an user that actually answered only to 5, will be to a Recommendation Model, based in the library Recommederlab. Tha basic idea is to consider Users (those who answer) and Items (questions) and apply some of the methods of standard recommendation. However, there are some singularities in our respect to a typical movie/book recommenadtion case:
# - No sparcity: our matrix of answwers Users X Items has all cells filled, since we have data for all users answers all questions
# - Matrix is pretty "vertical" having more that 800000 Users (after initial cleanup) and 50 Item (columns)
# - Items have clear assumed correlative components, since questions are grouped by traits
# Thus, we will choose adequate algorithms based on singularities:
# Due to the shape of the matrix, we discard IBCF (Item Based collaborative Filtering) due to the risk in some cases fo not generating predictions for all item (could be fixed with average/median filling though loosing accuracy). Regarding no scarcity we also discard Popular method.
# In order to generate a rich approach for our prediction, we'll focus in a collaborative filtering method (UBCF) and in a matrix factorization method (ALS) - we choose it instead of SVD for its superior accuracy given singularities of our User x Item matrix:
# - UBCF (User Based Colaborative Filtering): this algorithm which tries to mimics word-of-mouth by analyzing rating data from many individuals. The assumption is that users with similar preferences will rate items similarly. Thus missing ratings for a user can be predicted by first finding a neighborhood of similar users and then aggregate the ratings of these users to form a prediction. The neighborhood is defined in terms of similarity between users, either by taking a given number of most similar users (k nearest neighbors) or all users within a given similarity threshold. Popular similarity measures for CF are the Pearson correlation coefficient and the Cosine similarity. These similarity measures are defined between two users ux and uy as 
#   ¡¡FORMULA!!
# and
#   ¡¡FORMULA!!
# where ~x = rx and ~y = ry represent the row vectors in R with the two users’ profile vectors. sd(·) is the standard deviation and k · k is the l^2-norm of a vector. For calculating similarity using rating data only the dimensions (items) are used which were rated by both users. Now the neighborhood for the active user N (a) SIMBOLO_PERTENECE U can be selected by either a threshold on the similarity or by taking the k nearest neighbors. Once the users in the neighborhood are found, their ratings are aggregated to form the predicted rating for the active user. The easiest form is to just average the ratings in the neighborhood.
#   ¡¡FORMULA!!
# - ALS (Alternating Least Squares): ALS is an iterative optimization process where we, for every iteration, try to arrive closer and closer to a factorized representation of our original data. We have our original matrix R of size u x i with our users, items and some type of feedback data. We then want to find a way to turn that into one matrix with users and hidden features of size u x f and one with items and hidden features of size f x i. In U and V we have weights for how each user/item relates to each feature. What we do is we calculate U and V so that their product approximates R as closely as possible: R ≈ U x V. By randomly assigning the values in U and V and using least squares iteratively we can arrive at what weights yield the best approximation of R. The least squares approach in it’s basic forms means fitting some line to the data, measuring the sum of squared distances from all points to the line and trying to get an optimal fit by minimising this value.
# we prepare methods to run

methods_choice <- list(
  # list("ALS", "ALS" = list(NULL)),
  list("UBCF", "user-based CF" = list(nn=50))
)

# This for-loop serves for exution of each chosen method to train, test and measure-accuracy
for (a in 1:length(methods_choice)) {
  ##########################################################
  # Train model (Recommenderlab package)
  ##########################################################
  chosenAlgorithm <- methods_choice[[a]][[1]]
  chosenAlgorithmParams <- methods_choice[[a]][[2]]  
  # Train
  recom <- Recommender(
    as(data.matrix(BF_train[,2:51]), "realRatingMatrix"), 
    method = chosenAlgorithm, 
    parameter=chosenAlgorithmParams)
  
  
  ##########################################################
  # Test (validate) model
  ##########################################################
  questionsList <- dictionary$Question
  names(questionsList) <- dictionary$ID
  # ==============================
  # Prepare known ratings (5 out of 50) to send to the model - take from VALIDATION
  ratings <- matrix(NA, nrow = totRowsValidation, ncol = 50)
  ratings[, chosen_questions[1]] <- BF_test[,2:51][,chosen_questions[1]]
  ratings[, chosen_questions[2]] <- BF_test[,2:51][,chosen_questions[2]]
  ratings[, chosen_questions[3]] <- BF_test[,2:51][,chosen_questions[3]]
  ratings[, chosen_questions[4]] <- BF_test[,2:51][,chosen_questions[4]]
  ratings[, chosen_questions[5]] <- BF_test[,2:51][,chosen_questions[5]]
  ratings <- as(ratings, "realRatingMatrix")
  # Predict
  # create (predict) recommendations (45) based on known 5 answers ('ratings') 
  pred <- predict(recom, ratings, n=45)
  # Predicted answers from the model (still "rough")
  matrixNamesPredicted <- matrix(unlist(getList(pred)), ncol = 45, byrow = TRUE)
  matrixScoresPredicted <- matrix(unlist(getRatings(pred)), ncol = 45, byrow = TRUE)
  # This matrix (matrixScoresPredicted) contain all predictions for validation dataset row by row (matrixScoresPredicted), BUT each row follows a different order, based on corresponding row of previous (matrixNamesPredicted) matrix
  # So, we need to rearrange all lines to any, but the same, column structure. We use list of first row as template for this arrangement:
  tmpPatternColumnsReference <- matrixNamesPredicted[1,]
  tmpPatternColumnsReference
  # This loop performs rearrange row by row
  for (r in c(1:totRowsValidation)) {
    matrixScoresPredicted[r,] <- matrixScoresPredicted[r,][match(tmpPatternColumnsReference, matrixNamesPredicted[r,])]
  }
  # We assign column names of first row (used as reference to new global matrix)
  colnames(matrixScoresPredicted) <- tmpPatternColumnsReference
  rm(matrixNamesPredicted)  # Remove this mixed matrix to prevent confusion, clarity in next steps
  # Inspect obtained matrix with sorted (align values per column) predictions
  head(matrixScoresPredicted)
  # ==============================
  # Join together predictions with 5 known answers
  # Let's collect together entered answers and predicted ratings to prepare results calculation
  realRatings <- BF_test[,2:51] # real nx50 answers in the validation dataset
  head(realRatings)
  enteredQuestions <- dictionary[chosen_questions[1:5],1] # names of 5 "known"
  enteredRatings <- as(ratings, "matrix")[, chosen_questions[1:5]]  # answers of n x 5 "known" questions
  colnames(enteredRatings) <- enteredQuestions
  # Build a matrix with the union of 5 real answers + 45 predicted answers for all validation raws
  dim(enteredRatings) # n x 5 questions entered
  dim(matrixScoresPredicted) # n x 45 questions predicted 
  # Ready to prepare a single matrix
  matrixAllRatings <- cbind(enteredRatings, matrixScoresPredicted)
  dim(matrixAllRatings) # n x 50 (all questions)
  rm(matrixScoresPredicted) # For clarity to prevent confusion
  colnames(matrixAllRatings)
  # Reorder "mixed" columns as in dataset
  matrixAllRatings <- matrixAllRatings[,colnames(BFdata)]
  colnames(matrixAllRatings)
  
  
  ##########################################################
  # Score real and predicted matrices
  ##########################################################
  # We have now both matrices (real data and predicted data) so we score them individually
  # Score adding last (predicted) row to validation data  
  scoresfile_real <- scoreFast(keys.list.allPositive, as(BF_test[,2:51], "matrix"))
  scoresfile_pred <- scoreFast(keys.list.allPositive, as(matrixAllRatings, "matrix"))
  # Put a suffix in predicted-based scores columns to distinguish from real-based
  colnames(scoresfile_pred) <- paste(colnames(scoresfile_pred), "_pred", sep = "")
  
  ##########################################################
  # Convert just calculated scores to ranking (percentile)
  # Results are presented in percentile for each user/trait. With all data + prediction scored together (n+1 rows matrix), calculate percentiles of the last row (results for the user) using eCDF
  # For real data percentiles
  O_score <- round(ecdf(scoresfile_real[,"openess-A"])(scoresfile_real[,"openess-A"])*100,0)
  C_score <- round(ecdf(scoresfile_real[,"conscienciousness-A"])(scoresfile_real[,"conscienciousness-A"])*100,0)
  E_score <- round(ecdf(scoresfile_real[,"extroversion-A"])(scoresfile_real[,"extroversion-A"])*100,0)
  A_score <- round(ecdf(scoresfile_real[,"agreeability-A"])(scoresfile_real[,"agreeability-A"])*100,0)
  N_score <- round(ecdf(scoresfile_real[,"natural_reactions-A"])(scoresfile_real[,"natural_reactions-A"])*100,0)
  traits_percentiles_real <- cbind(O_score, C_score, E_score, A_score, N_score)
  # For predicted data percentiles
  O_score_pred <- round(ecdf(scoresfile_real[,"openess-A"])(scoresfile_pred[,"openess-A_pred"])*100,0)
  C_score_pred <- round(ecdf(scoresfile_real[,"conscienciousness-A"])(scoresfile_pred[,"conscienciousness-A_pred"])*100,0)
  E_score_pred <- round(ecdf(scoresfile_real[,"extroversion-A"])(scoresfile_pred[,"extroversion-A_pred"])*100,0)
  A_score_pred <- round(ecdf(scoresfile_real[,"agreeability-A"])(scoresfile_pred[,"agreeability-A_pred"])*100,0)
  N_score_pred <- round(ecdf(scoresfile_real[,"natural_reactions-A"])(scoresfile_pred[,"natural_reactions-A_pred"])*100,0)
  traits_percentiles_predicted <- cbind(O_score_pred, C_score_pred, E_score_pred, A_score_pred, N_score_pred)
  
  ################################################
  # Here we have all resulting data
  names(chosenAlgorithm) = "Algorithm"
  # Other files generated during process
  head(traits_percentiles_real)
  head(traits_percentiles_predicted)
  
  ##########################################################
  # Compose high level result in a data frame
  # We need to calculate several accuracy indicators (by trait/general, by quarter/half correct prediction, and by most traits correctly predicted)
  # Detect quartile success in trait
  accuracyPerQuartile <- colMeans((1+floor(abs((traits_percentiles_real-1))/25)) == (1+floor(abs((traits_percentiles_predicted-1))/25)))
  accuracyPerQuartileMean <- as(cbind("All", mean(accuracyPerQuartile),"Hits quartile"),"matrix")
  accuracyPerQuartile <- cbind(as(accuracyPerQuartile,"matrix"), Accuracy_type = "Hits quartile")
  # Detect High-Low success in trait
  accuracyPerHalf <- colMeans((1+floor(abs((traits_percentiles_real-1))/50)) == (1+floor(abs((traits_percentiles_predicted-1))/50)))
  accuracyPerHalfMean <- as(cbind("All", mean(accuracyPerHalf),"Hits HighLow"),"matrix")
  accuracyPerHalf <- cbind(as(accuracyPerHalf,"matrix"), Accuracy_type = "Hits HighLow")
  # Detect High-Low successes are >= 3 in same test
  accuracySameHalf <- (1+floor(abs((traits_percentiles_real-1))/50)) == (1+floor(abs((traits_percentiles_predicted-1))/50))
  accuracySameHalf <- mean(rowSums(accuracySameHalf[,1:5]) >= 3)
  accuracySameHalf <- as(cbind("All", accuracySameHalf,"3+ hits HighLow"),"matrix")
  accuracySameHalf
  
  # Collect results already obtained in a temporary matrix
  tmpResultsPrediction <- rbind(
    accuracyPerQuartile, 
    accuracyPerHalf)
  tmpResultsPrediction <- cbind(
    "Trait" = rownames(tmpResultsPrediction), tmpResultsPrediction)
  colnames(tmpResultsPrediction)[2] <- "Score"
  rownames(tmpResultsPrediction) <- NULL
  tmpResultsPrediction <- tmpResultsPrediction %>% 
    rbind(accuracySameHalf, accuracyPerQuartileMean, accuracyPerHalfMean) %>% 
    cbind("Algorithm" = chosenAlgorithm)
  head (tmpResultsPrediction)
  # Create data frame
  df_resultsPrediction <- as.data.frame(tmpResultsPrediction)
  df_resultsPrediction <- df_resultsPrediction %>% 
    mutate(Score = as.double(as.character(df_resultsPrediction$Score)))
  head(df_resultsPrediction)
  rm(tmpResultsPrediction)
  # Store results
  analysis_results <- bind_rows(
    analysis_results, 
    df_resultsPrediction)
  analysis_results %>% knitr::kable(digits = 4)
  
}
# Ends FOR-LOOP of algorithms to model


## =============================================================================
## =============================================================================
## Show final results
## =============================================================================
## =============================================================================
#
# We can know compare how accuracies, as defined, get significantly improved with both ALS and UBCF methods algorithms respect to base "random" reference generated with the Montecarlo approach

# Accuracies by trait and aggregated
table_results <- analysis_results %>% 
  spread(Trait, Score, fill = "") %>% 
  select(1,2,4,8,5,6,3,7,4)
table_results %>% knitr::kable(digits = 4)

# Summary of accuracies
table_results %>% 
  group_by(Accuracy_type, Algorithm) %>% 
  summarise("Accuracy" = max(as.numeric(All))) %>% 
  spread(Algorithm, Accuracy, fill = "") %>% 
  mutate(
    Improvement_UBCF = 
      paste(100*round((as.numeric(UBCF)/as.numeric(Montecarlo)),2),"%",sep = ""), 
    Improvement_ALS = 
      paste(100*round((as.numeric(ALS)/as.numeric(Montecarlo)),2),"%",sep = "")    
  ) %>% 
  knitr::kable(digits = 4)

# Improvement number
refMC <- table_results %>% filter(Algorithm == "Montecarlo" & Accuracy_type == "3+ hits HighLow") %>% pull(All)
refALS <- table_results %>% filter(Algorithm == "ALS" & Accuracy_type == "3+ hits HighLow") %>% pull(All)
improvementNumber <- paste(100*round((as.numeric(refALS)/as.numeric(refMC)),2),"%",sep = "")
improvementNumber

view(table_results)




