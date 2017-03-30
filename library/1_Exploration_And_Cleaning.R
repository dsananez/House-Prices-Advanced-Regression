#Loading libraries and functions
source("../library/general.R")

#Loading the datasets
train <- read.csv("../input/train.csv", stringsAsFactors=FALSE)
test <- read.csv("../input/test.csv", stringsAsFactors=FALSE)

#Variables Exploration and Formatting

    #Character Variables List
    str(train[, sapply(train, class) == "character"])
    
    #MSZoning: Ordinal or Categorical
    P
    p <- ggplot(train, aes(x=MSZoning, y=SalePrice, color=MSZoning)) +
        geom_boxplot()
    p
    
  