# Regenerar "loadExtractionScope.R"
# Decidir si considero VOL de los stocks
# Fusionar con "initialization/load_extractionScope.R"

library(tidyverse)


# Create seed as a list of features specifying for each: the geography applying, related concepts/terms 

# Initialize with dummie row
seed_1 <- data.frame(
  featureCode = c(""), 
  geo_std = c(""), 
  concepts = I(list(c(""))), 
  isGoal = FALSE, 
  featureName = c(""), 
  relatedFeatures = I(list(c(""))))


seed_1 <- seed_1 %>% 
  rbind(list("CADJPY=X", "CAN", I(list(c("Truffeau", "Canadian"))), TRUE, "Canadian$-Yen", I(list(c("FIFA.Rank", "music.tempo"))))) %>% 
  rbind(list("EURCAD=X", "EUR", I(list(c("from:Merkel", "European"))), TRUE, "Canadian$-Euro", I(list(c("airTraffic.worldFlights", "FIFA.Rank")))))
# Remove dummie row
seed_1 = seed_1[-c(1),]


head(seed_1)
class(seed_1$relatedFeatures)



