#Loading libraries and functions
source("../library/general.R")

#Loading the datasets
train <- read.csv("../input/train.csv", stringsAsFactors=FALSE)
test <- read.csv("../input/test.csv", stringsAsFactors=FALSE)

#Variables Exploration and Formatting

    #Character Variables List
    str(train[, sapply(train, class) == "character"])
    
    #MSZoning Boxplot
    
    p <- ggplot(train, aes(x=MSZoning, y=SalePrice, color=MSZoning)) +
        geom_boxplot()
    p
    
    #MSZoning mean t-tests
    
    means <- tapply(train$SalePrice, train$MSZoning, mean)
    means <- sort(means)
    ttest <- NULL
    for (i in 1:(length(means)-1)){
        ttest[i] <- t.test(train$SalePrice[train$MSZoning == names(means[i])], train$SalePrice[train$MSZoning == names(means[i+1])], "l")$p.value
    }
    round(ttest, 5)
    
    #MSZoning Ordinal Variables
    
    train$MSZoning[train$MSZoning == "C (all)"] <- 1
    train$MSZoning[train$MSZoning == "RM" | train$MSZoning == "RH"] <- 2
    train$MSZoning[train$MSZoning == "RL"] <- 3
    train$MSZoning[train$MSZoning == "FV"] <- 4
    
    #MSZoning Ordinal Boxplot
    
    p2<- ggplot(train, aes(x=MSZoning, y=SalePrice, color=MSZoning)) +
        geom_boxplot()
    
    multiplot(p, p2)
    
    #MSZoning LM
    
    train$MSZoning <- as.integer(train$MSZoning)
    summary(lm(train$SalePrice~train$MSZoning))
    
    #Street Boxplot
    
    p <- ggplot(train, aes(x=Street, y=SalePrice, color=Street)) +
        geom_boxplot()
    p
    
    #Street mean t-tests
    
    means <- tapply(train$SalePrice, train$Street, mean)
    means <- sort(means)
    ttest <- NULL
    round(t.test(train$SalePrice[train$Street == names(means[1])], train$SalePrice[train$Street == names(means[2])], "l")$p.value, 5)
    
    #Street Ordinal Variables
    
    train$Street[train$Street == "Grvl"] <- 0
    train$Street[train$Street == "Pave"] <- 1

   #Street LM
    
    train$Street <- as.integer(train$Street)
    summary(lm(train$SalePrice~train$Street))
    
    #Alley Boxplot
    
    p <- ggplot(train, aes(x=Alley, y=SalePrice, color=Alley)) +
        geom_boxplot()
    p
    
    #Alley mean t-tests
    
    means <- tapply(train$SalePrice, train$Alley, mean)
    means <- sort(means)
    ttest <- NULL
    for (i in 1:(length(means)-1)){
        ttest[i] <- t.test(train$SalePrice[train$Alley == names(means[i])], train$SalePrice[train$Alley == names(means[i+1])], "l")$p.value
    }
    round(ttest, 5)
    
    #Alley Ordinal Variables
    
    train$Alley[train$Alley == "Grvl"] <- 0
    train$Alley[train$Alley == "Pave"] <- 1
    train$Alley[is.na(train$Alley)] <- 2

    #Alley LM

    train$Alley <- as.integer(train$Alley)
    summary(lm(train$SalePrice~train$Alley))
    
    #LotShape Boxplot
    
    p <- ggplot(train, aes(x=LotShape, y=SalePrice, color=LotShape)) +
        geom_boxplot()
    p
    
    #LotShape mean t-tests
    
    means <- tapply(train$SalePrice, train$LotShape, mean)
    means <- sort(means)
    ttest <- NULL
    for (i in 1:(length(means)-1)){
        ttest[i] <- t.test(train$SalePrice[train$LotShape == names(means[i])], train$SalePrice[train$LotShape == names(means[i+1])], "l")$p.value
    }
    round(ttest, 5)
    
    #LotShape Ordinal Variables
    
    train$LotShape[train$LotShape == "Reg"] <- 1
    train$LotShape[train$LotShape == "IR1" | train$LotShape == "IR2"] <- 2
    train$LotShape[train$LotShape == "IR3"] <- 3

    #LotShape Ordinal Boxplot
    
    p2<- ggplot(train, aes(x=LotShape, y=SalePrice, color=LotShape)) +
        geom_boxplot()
    
    multiplot(p, p2)
    
    #LotShape LM
    
    train$LotShape <- as.integer(train$LotShape)
    summary(lm(train$SalePrice~train$LotShape))
    
    #LandContour Boxplot
    
    p <- ggplot(train, aes(x=LandContour, y=SalePrice, color=LandContour)) +
        geom_boxplot()
    p
    
    #LandContour mean t-tests
    
    means <- tapply(train$SalePrice, train$LandContour, mean)
    means <- sort(means)
    ttest <- NULL
    for (i in 1:(length(means)-1)){
        ttest[i] <- t.test(train$SalePrice[train$LandContour == names(means[i])], train$SalePrice[train$LandContour == names(means[i+1])], "l")$p.value
    }
    round(ttest, 5)
    
    #LandContour Ordinal Variables
    
    train$LandContour[train$LandContour == "Bnk"] <- 1
    train$LandContour[train$LandContour == "Lvl"] <- 2
    train$LandContour[train$LandContour == "Low"] <- 3
    train$LandContour[train$LandContour == "HLS"] <- 4
    
    #LandContour Ordinal Boxplot
    
    p2<- ggplot(train, aes(x=LandContour, y=SalePrice, color=LandContour)) +
        geom_boxplot()
    
    multiplot(p, p2)
    
    #LandContour LM
    
    train$LandContour <- as.integer(train$LandContour)
    summary(lm(train$SalePrice~train$LandContour))

    #Utilities Boxplot
    
    p <- ggplot(train, aes(x=Utilities, y=SalePrice, color=Utilities)) +
        geom_boxplot()
    p
    
    #Utilities mean t-tests
    
    means <- tapply(train$SalePrice, train$Utilities, mean)
    means <- sort(means)
    ttest <- NULL
    for (i in 1:(length(means)-1)){
        ttest[i] <- t.test(train$SalePrice[train$Utilities == names(means[i])], train$SalePrice[train$Utilities == names(means[i+1])], "l")$p.value
    }
    round(ttest, 5)
    
    #Utilities Ordinal Variables
    
    train$Utilities[train$Utilities == "C (all)"] <- 1
    train$Utilities[train$Utilities == "RM" | train$Utilities == "RH"] <- 2
    train$Utilities[train$Utilities == "RL"] <- 3
    train$Utilities[train$Utilities == "FV"] <- 4
    
    #Utilities Ordinal Boxplot
    
    p2<- ggplot(train, aes(x=Utilities, y=SalePrice, color=Utilities)) +
        geom_boxplot()
    
    multiplot(p, p2)
    
    #Utilities LM
    
    train$Utilities <- as.integer(train$Utilities)
    summary(lm(train$SalePrice~train$Utilities))
    