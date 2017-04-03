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
    
    train$Utilities[train$Utilities != "AllPub"] <- 0
    train$Utilities[train$Utilities == "AllPub"] <- 1
    
    #Utilities Ordinal Boxplot
    
    p2<- ggplot(train, aes(x=Utilities, y=SalePrice, color=Utilities)) +
        geom_boxplot()
    
    multiplot(p, p2)
    
    #Utilities LM
    
    train$Utilities <- as.integer(train$Utilities)
    summary(lm(train$SalePrice~train$Utilities))
    
    #LotConfig Boxplot
    
    p <- ggplot(train, aes(x=LotConfig, y=SalePrice, color=LotConfig)) +
        geom_boxplot()
    p
    
    #LotConfig mean t-tests
    
    means <- tapply(train$SalePrice, train$LotConfig, mean)
    means <- sort(means)
    ttest <- NULL
    for (i in 1:(length(means)-1)){
        ttest[i] <- t.test(train$SalePrice[train$LotConfig == names(means[i])], train$SalePrice[train$LotConfig == names(means[i+1])], "l")$p.value
    }
    round(ttest, 5)
    
    #LotConfig Ordinal Variables
    
    train$LotConfig[train$LotConfig == "FR3" | train$LotConfig != "CulDSac"] <- 0
    train$LotConfig[train$LotConfig == "FR3" | train$LotConfig == "CulDSac"] <- 1
    
    #LotConfig Ordinal Boxplot
    
    p2<- ggplot(train, aes(x=LotConfig, y=SalePrice, color=LotConfig)) +
        geom_boxplot()
    
    multiplot(p, p2)
    
    #LotConfig LM
    
    train$LotConfig <- as.integer(train$LotConfig)
    summary(lm(train$SalePrice~train$LotConfig))
    
    #LandSlope Boxplot
    
    p <- ggplot(train, aes(x=LandSlope, y=SalePrice, color=LandSlope)) +
        geom_boxplot()
    p
    
    #LandSlope mean t-tests
    
    means <- tapply(train$SalePrice, train$LandSlope, mean)
    means <- sort(means)
    ttest <- NULL
    for (i in 1:(length(means)-1)){
        ttest[i] <- t.test(train$SalePrice[train$LandSlope == names(means[i])], train$SalePrice[train$LandSlope == names(means[i+1])], "l")$p.value
    }
    round(ttest, 5)
    
    #LandSlope Ordinal Variables
    
    train$LandSlope[train$LandSlope != "Gtl"] <- 1
    train$LandSlope[train$LandSlope == "Gtl"] <- 0
    
    #LandSlope Ordinal Boxplot
    
    p2<- ggplot(train, aes(x=LandSlope, y=SalePrice, color=LandSlope)) +
        geom_boxplot()
    
    multiplot(p, p2)
    
    #LandSlope LM
    
    train$LandSlope <- as.integer(train$LandSlope)
    summary(lm(train$SalePrice~train$LandSlope))
    
    #Neighborhood Boxplot
    
    p <- ggplot(train, aes(x=Neighborhood, y=SalePrice, color=Neighborhood)) +
        geom_boxplot()
    p
    
    #Neighborhood mean t-tests
    
    means <- tapply(train$SalePrice, train$Neighborhood, mean)
    means <- sort(means)
    ttest <- NULL
    for (i in 1:(length(means)-1)){
        ttest[i] <- t.test(train$SalePrice[train$Neighborhood == names(means[i])], train$SalePrice[train$Neighborhood == names(means[i+1])], "l")$p.value
    }
    round(ttest, 5)
    
    #Neighborhood Ordinal Variables
    
    nbhdprice <- summarize(group_by(train, Neighborhood),
                           mean(SalePrice, na.rm=T))
    neigh0 <- filter(nbhdprice, nbhdprice$`mean(SalePrice, na.rm = T)` < 110000)
    neigh1 <- filter(nbhdprice, nbhdprice$`mean(SalePrice, na.rm = T)` < 150000 &
                                nbhdprice$`mean(SalePrice, na.rm = T)` >= 110000)
    neigh2 <- filter(nbhdprice, nbhdprice$`mean(SalePrice, na.rm = T)` < 165000 &
                                nbhdprice$`mean(SalePrice, na.rm = T)` >= 150000)
    neigh3 <- filter(nbhdprice, nbhdprice$`mean(SalePrice, na.rm = T)` < 275000 &
                                           nbhdprice$`mean(SalePrice, na.rm = T)` >= 165000 )
    neigh4 <-  filter(nbhdprice, nbhdprice$`mean(SalePrice, na.rm = T)` >= 275000)
    
    train$Neighborhood[train$Neighborhood %in% neigh0$Neighborhood] <- 1
    train$Neighborhood[train$Neighborhood %in% neigh1$Neighborhood] <- 2
    train$Neighborhood[train$Neighborhood %in% neigh2$Neighborhood] <- 3
    train$Neighborhood[train$Neighborhood %in% neigh3$Neighborhood] <- 4
    train$Neighborhood[train$Neighborhood %in% neigh4$Neighborhood] <- 5

    #Neighborhood Ordinal Boxplot
    
    p2<- ggplot(train, aes(x=Neighborhood, y=SalePrice, color=Neighborhood)) +
        geom_boxplot()
    
    multiplot(p, p2)
    
    #Neighborhood LM
    
    train$Neighborhood <- as.integer(train$Neighborhood)
    summary(lm(train$SalePrice~train$Neighborhood))
    
    #Condition1 Boxplot
    
    p <- ggplot(train, aes(x=Condition1, y=SalePrice, color=Condition1)) +
        geom_boxplot()
    p
    
    #Condition1 mean t-tests
    
    means <- tapply(train$SalePrice, train$Condition1, mean)
    means <- sort(means)
    ttest <- NULL
    for (i in 1:(length(means)-1)){
        ttest[i] <- t.test(train$SalePrice[train$Condition1 == names(means[i])], train$SalePrice[train$Condition1 == names(means[i+1])], "l")$p.value
    }
    round(ttest, 5)
    
    #Condition1 Ordinal Variables
    
    train$Condition1[!train$Condition1 %in% c("Feedr", "Artery", "RRAe")] <- 1
    train$Condition1[train$Condition1 %in% c("Feedr", "Artery", "RRAe")] <- 0
  
    
    #Condition1 Ordinal Boxplot
    
    p2<- ggplot(train, aes(x=Condition1, y=SalePrice, color=Condition1)) +
        geom_boxplot()
    
    multiplot(p, p2)
    
    #Condition1 LM
    
    train$Condition1 <- as.integer(train$Condition1)
    summary(lm(train$SalePrice~train$Condition1))
    
    #Condition2 Boxplot
    
    p <- ggplot(train, aes(x=Condition2, y=SalePrice, color=Condition2)) +
        geom_boxplot()
    p
    
    #Condition2 mean t-tests
    
    means <- tapply(train$SalePrice, train$Condition2, mean)
    means <- sort(means)
    ttest <- NULL
    for (i in 1:(length(means)-1)){
        ttest[i] <- t.test(train$SalePrice[train$Condition2 == names(means[i])], train$SalePrice[train$Condition2 == names(means[i+1])], "l")$p.value
    }
    round(ttest, 5)
    
    #Condition2 Ordinal Variables
    
    train$Condition2[!train$Condition2 %in% c("Feedr", "Artery", "RRAe")] <- 1
    train$Condition2[train$Condition2 %in% c("Feedr", "Artery", "RRAe")] <- 0
    
    
    #Condition2 Ordinal Boxplot
    
    p2<- ggplot(train, aes(x=Condition2, y=SalePrice, color=Condition2)) +
        geom_boxplot()
    
    multiplot(p, p2)
    
    #Condition2 LM
    
    train$Condition2 <- as.integer(train$Condition2)
    summary(lm(train$SalePrice~train$Condition2))
    
    #BldgType Boxplot
    
    p <- ggplot(train, aes(x=BldgType, y=SalePrice, color=BldgType)) +
        geom_boxplot()
    p
    
    #BldgType mean t-tests
    
    means <- tapply(train$SalePrice, train$BldgType, mean)
    means <- sort(means)
    ttest <- NULL
    for (i in 1:(length(means)-1)){
        ttest[i] <- t.test(train$SalePrice[train$BldgType == names(means[i])], train$SalePrice[train$BldgType == names(means[i+1])], "l")$p.value
    }
    round(ttest, 5)
    
    #BldgType Ordinal Variables
    
    train$BldgType[!train$BldgType %in% c("TwnhsE", "1Fam")] <- 0
    train$BldgType[train$BldgType %in% c("TwnhsE", "1Fam")] <- 1
    
    #BldgType Ordinal Boxplot
    
    p2<- ggplot(train, aes(x=BldgType, y=SalePrice, color=BldgType)) +
        geom_boxplot()
    
    multiplot(p, p2)
    
    #BldgType LM
    
    train$BldgType <- as.integer(train$BldgType)
    summary(lm(train$SalePrice~train$BldgType))
    
    #HouseStyle Boxplot
    
    p <- ggplot(train, aes(x=HouseStyle, y=SalePrice, color=HouseStyle)) +
        geom_boxplot()
    p
    
    #HouseStyle mean t-tests
    
    means <- tapply(train$SalePrice, train$HouseStyle, mean)
    means <- sort(means)
    ttest <- NULL
    for (i in 1:(length(means)-1)){
        ttest[i] <- t.test(train$SalePrice[train$HouseStyle == names(means[i])], train$SalePrice[train$HouseStyle == names(means[i+1])], "l")$p.value
    }
    round(ttest, 5)
    
    #HouseStyle Ordinal Variables
   
    train$HouseStyle[train$HouseStyle == "1.5Unf"] <- 1
    train$HouseStyle[train$HouseStyle %in% c("SFoyer", "1.5Fin", "2.5Unf", "SLvl", "1Story")] <- 2
    train$HouseStyle[train$HouseStyle %in% c("2.5Fin", "2Story")] <- 3

    #HouseStyle Ordinal Boxplot
    
    p2<- ggplot(train, aes(x=HouseStyle, y=SalePrice, color=HouseStyle)) +
        geom_boxplot()
    
    multiplot(p, p2)
    
    #HouseStyle LM
    
    train$HouseStyle <- as.integer(train$HouseStyle)
    summary(lm(train$SalePrice~train$HouseStyle))
    
    #RoofStyle Boxplot
    
    p <- ggplot(train, aes(x=RoofStyle, y=SalePrice, color=RoofStyle)) +
        geom_boxplot()
    p
    
    #RoofStyle mean t-tests
    
    means <- tapply(train$SalePrice, train$RoofStyle, mean)
    means <- sort(means)
    ttest <- NULL
    for (i in 1:(length(means)-1)){
        ttest[i] <- t.test(train$SalePrice[train$RoofStyle == names(means[i])], train$SalePrice[train$RoofStyle == names(means[i+1])], "l")$p.value
    }
    round(ttest, 5)
    
    #RoofStyle Ordinal Variables
    
    train$RoofStyle[train$RoofStyle == "Gambrel"] <- 1
    train$RoofStyle[train$RoofStyle %in% c("Mansard", "Gable")] <- 2
    train$RoofStyle[train$RoofStyle %in% c("Hip", "Shed", "Flat")] <- 3
    
    #RoofStyle Ordinal Boxplot
    
    p2<- ggplot(train, aes(x=RoofStyle, y=SalePrice, color=RoofStyle)) +
        geom_boxplot()
    
    multiplot(p, p2)
    
    #RoofStyle LM
    
    train$RoofStyle <- as.integer(train$RoofStyle)
    summary(lm(train$SalePrice~train$RoofStyle))
    
    #RoofMatl Boxplot
    
    p <- ggplot(train, aes(x=RoofMatl, y=SalePrice, color=RoofMatl)) +
        geom_boxplot()
    p
    
    #RoofMatl mean t-tests
    
    means <- tapply(train$SalePrice, train$RoofMatl, mean)
    means <- sort(means)
    ttest <- NULL
    for (i in 1:(length(means)-1)){
        ttest[i] <- t.test(train$SalePrice[train$RoofMatl == names(means[i])], train$SalePrice[train$RoofMatl == names(means[i+1])], "l")$p.value
    }
    round(ttest, 5)
    
    #RoofMatl Ordinal Variables
    
    train$RoofMatl[train$RoofMatl %in% c("Roll", "ClyTile", "CompShg", "Metal")] <- 1
    train$RoofMatl[train$RoofMatl == "Tar&Grv"] <- 2
    train$RoofMatl[train$RoofMatl %in% c("WdShake", "Membran")] <- 3
    train$RoofMatl[train$RoofMatl == "WdShngl"] <- 4
    
    #RoofMatl Ordinal Boxplot
    
    p2<- ggplot(train, aes(x=RoofMatl, y=SalePrice, color=RoofMatl)) +
        geom_boxplot()
    
    multiplot(p, p2)
    
    #RoofMatl LM
    
    train$RoofMatl <- as.integer(train$RoofMatl)
    summary(lm(train$SalePrice~train$RoofMatl))
    
    #Exterior1st Boxplot
    
    p <- ggplot(train, aes(x=Exterior1st, y=SalePrice, color=Exterior1st)) +
        geom_boxplot()
    p
    
    #Exterior1st mean t-tests
    
    means <- tapply(train$SalePrice, train$Exterior1st, mean)
    means <- sort(means)
    ttest <- NULL
    for (i in 1:(length(means)-1)){
        ttest[i] <- t.test(train$SalePrice[train$Exterior1st == names(means[i])], train$SalePrice[train$Exterior1st == names(means[i+1])], "l")$p.value
    }
    round(ttest, 5)
    
    #Exterior1st Ordinal Variables
    
    train$Exterior1st[train$Exterior1st %in% c("BrkComm", "AsphShn", "CBlock", "AsbShng")] <- 1
    train$Exterior1st[train$Exterior1st %in% c("MetalSd", "Wd Sdng", "WdShing")] <- 2
    train$Exterior1st[train$Exterior1st %in% c("Stucco", "HdBoard")] <- 3
    train$Exterior1st[train$Exterior1st == "Plywood"] <- 4
    train$Exterior1st[train$Exterior1st == "BrkFace"] <- 5
    train$Exterior1st[train$Exterior1st == "VinylSd"] <- 6
    train$Exterior1st[train$Exterior1st == "CemntBd"] <- 7    
    train$Exterior1st[train$Exterior1st %in% c("Stone", "ImStucc")] <- 8
    
    #Exterior1st Ordinal Boxplot
    
    p2<- ggplot(train, aes(x=Exterior1st, y=SalePrice, color=Exterior1st)) +
        geom_boxplot()
    
    multiplot(p, p2)
    
    #Exterior1st LM
    
    train$Exterior1st <- as.integer(train$Exterior1st)
    summary(lm(train$SalePrice~train$Exterior1st))
    
    #Exterior2nd Boxplot
    
    p <- ggplot(train, aes(x=Exterior2nd, y=SalePrice, color=Exterior2nd)) +
        geom_boxplot()
    p
    
    #Exterior2nd mean t-tests
    
    means <- tapply(train$SalePrice, train$Exterior2nd, mean)
    means <- sort(means)
    ttest <- NULL
    for (i in 1:(length(means)-1)){
        ttest[i] <- t.test(train$SalePrice[train$Exterior2nd == names(means[i])], train$SalePrice[train$Exterior2nd == names(means[i+1])], "l")$p.value
    }
    round(ttest, 5)
    
    #Exterior2nd Ordinal Variables
    
    train$Exterior2nd[train$Exterior2nd %in% c("Brk Cmn", "CBlock", "AsbShng")] <- 1
    train$Exterior2nd[train$Exterior2nd %in% c("AsphShn", "MetalSd", "Wd Sdng")] <- 2
    train$Exterior2nd[train$Exterior2nd %in% c("Stucco", "Stone", "Wd Shng")] <- 3
    train$Exterior2nd[train$Exterior2nd %in% c("HdBoard", "Plywood")] <- 4
    train$Exterior2nd[train$Exterior2nd == "BrkFace"] <- 5
    train$Exterior2nd[train$Exterior2nd == "VinylSd"] <- 6
    train$Exterior2nd[train$Exterior2nd == "CmentBd"] <- 7    
    train$Exterior2nd[train$Exterior2nd %in% c("Other", "ImStucc")] <- 8
    
    #Exterior2nd Ordinal Boxplot
    
    p2<- ggplot(train, aes(x=Exterior2nd, y=SalePrice, color=Exterior2nd)) +
        geom_boxplot()
    
    multiplot(p, p2)
    
    #Exterior2nd LM
    
    train$Exterior2nd <- as.integer(train$Exterior2nd)
    summary(lm(train$SalePrice~train$Exterior2nd))
    
    