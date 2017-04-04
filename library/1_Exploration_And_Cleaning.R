#Loading libraries and functions
source("../library/general.R")

#Loading the datasets
train <- read.csv("../input/train.csv", stringsAsFactors=FALSE)
test <- read.csv("../input/test.csv", stringsAsFactors=FALSE)

#Variables Exploration and Formatting

    #Character Variables List
    str(train[, !sapply(train, class) == "integer"])
    
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
    
    train$LotConfig[train$LotConfig != "CulDSac"] <- 0
    train$LotConfig[train$LotConfig == "CulDSac"] <- 1
    
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
    
    #MasVnrType Boxplot
    
    p <- ggplot(train, aes(x=MasVnrType, y=SalePrice, color=MasVnrType)) +
        geom_boxplot()
    p
    
    #MasVnrType mean t-tests
    
    means <- tapply(train$SalePrice, train$MasVnrType, mean)
    means <- sort(means)
    ttest <- NULL
    for (i in 1:(length(means)-1)){
        ttest[i] <- t.test(train$SalePrice[train$MasVnrType == names(means[i])], train$SalePrice[train$MasVnrType == names(means[i+1])], "l")$p.value
    }
    round(ttest, 5)
    
    #MasVnrType Ordinal Variables
    
    train$MasVnrType[train$MasVnrType %in% c("BrkCmn", "None")] <- 1
    train$MasVnrType[train$MasVnrType == "BrkFace"] <- 2
    train$MasVnrType[train$MasVnrType == "Stone"] <- 3

    #MasVnrType Ordinal Boxplot
    
    p2<- ggplot(train, aes(x=MasVnrType, y=SalePrice, color=MasVnrType)) +
        geom_boxplot()
    
    multiplot(p, p2)
    
    #MasVnrType LM
    
    train$MasVnrType <- as.integer(train$MasVnrType)
    summary(lm(train$SalePrice~train$MasVnrType))
    
    #ExterQual Boxplot
    
    p <- ggplot(train, aes(x=ExterQual, y=SalePrice, color=ExterQual)) +
        geom_boxplot()
    p
    
    #ExterQual mean t-tests
    
    means <- tapply(train$SalePrice, train$ExterQual, mean)
    means <- sort(means)
    ttest <- NULL
    for (i in 1:(length(means)-1)){
        ttest[i] <- t.test(train$SalePrice[train$ExterQual == names(means[i])], train$SalePrice[train$ExterQual == names(means[i+1])], "l")$p.value
    }
    round(ttest, 5)
    
    #ExterQual Ordinal Variables
    
    train$ExterQual[train$ExterQual == "Po"] <- 1
    train$ExterQual[train$ExterQual == "Fa"] <- 2
    train$ExterQual[train$ExterQual == "TA"] <- 3
    train$ExterQual[train$ExterQual == "Gd"] <- 4
    train$ExterQual[train$ExterQual == "Ex"] <- 5
    
    #ExterQual Ordinal Boxplot
    
    p2<- ggplot(train, aes(x=ExterQual, y=SalePrice, color=ExterQual)) +
        geom_boxplot()
    
    multiplot(p, p2)
    
    #ExterQual LM
    
    train$ExterQual <- as.integer(train$ExterQual)
    summary(lm(train$SalePrice~train$ExterQual))
    
    #ExterCond Boxplot
    
    p <- ggplot(train, aes(x=ExterCond, y=SalePrice, color=ExterCond)) +
        geom_boxplot()
    p
    
    #ExterCond mean t-tests
    
    means <- tapply(train$SalePrice, train$ExterCond, mean)
    means <- sort(means)
    means <- means[-1]
    ttest <- NULL
    for (i in 1:(length(means)-1)){
        ttest[i] <- t.test(train$SalePrice[train$ExterCond == names(means[i])], train$SalePrice[train$ExterCond == names(means[i+1])], "l")$p.value
    }
    round(ttest, 5)
    
    #ExterCond Ordinal Variables
    
    train$ExterCond[train$ExterCond == "Po"] <- 1
    train$ExterCond[train$ExterCond == "Fa"] <- 2
    train$ExterCond[train$ExterCond == "TA"] <- 3
    train$ExterCond[train$ExterCond == "Gd"] <- 4
    train$ExterCond[train$ExterCond == "Ex"] <- 5
    
    #ExterCond Ordinal Boxplot
    
    p2<- ggplot(train, aes(x=ExterCond, y=SalePrice, color=ExterCond)) +
        geom_boxplot()
    
    multiplot(p, p2)
    
    #ExterCond LM
    
    train$ExterCond <- as.integer(train$ExterCond)
    summary(lm(train$SalePrice~train$ExterCond))
    
    #Foundation Boxplot
    
    p <- ggplot(train, aes(x=Foundation, y=SalePrice, color=Foundation)) +
        geom_boxplot()
    p
    
    #Foundation mean t-tests
    
    means <- tapply(train$SalePrice, train$Foundation, mean)
    means <- sort(means)
    ttest <- NULL
    for (i in 1:(length(means)-1)){
        ttest[i] <- t.test(train$SalePrice[train$Foundation == names(means[i])], train$SalePrice[train$Foundation == names(means[i+1])], "l")$p.value
    }
    round(ttest, 5)
    
    #Foundation Ordinal Variables
    
    train$Foundation[train$Foundation == "Slab"] <- 1
    train$Foundation[train$Foundation == "BrkTil"] <- 2
    train$Foundation[train$Foundation == "CBlock"] <- 3
    train$Foundation[train$Foundation == "Stone" | train$Foundation == "Wood"] <- 4
    train$Foundation[train$Foundation == "PConc"] <- 5
    
    #Foundation Ordinal Boxplot
    
    p2<- ggplot(train, aes(x=Foundation, y=SalePrice, color=Foundation)) +
        geom_boxplot()
    
    multiplot(p, p2)
    
    #Foundation LM
    
    train$Foundation <- as.integer(train$Foundation)
    summary(lm(train$SalePrice~train$Foundation))
    
    #BsmtQual Boxplot
    
    p <- ggplot(train, aes(x=BsmtQual, y=SalePrice, color=BsmtQual)) +
        geom_boxplot()
    p
    
    #BsmtQual mean t-tests
    
    means <- tapply(train$SalePrice, train$BsmtQual, mean)
    means <- sort(means)
    ttest <- NULL
    for (i in 1:(length(means)-1)){
        ttest[i] <- t.test(train$SalePrice[train$BsmtQual == names(means[i])], train$SalePrice[train$BsmtQual == names(means[i+1])], "l")$p.value
    }
    round(ttest, 5)
    
    #BsmtQual Ordinal Variables
    
    train$BsmtQual[train$BsmtQual == "Po"] <- 1
    train$BsmtQual[train$BsmtQual == "Fa"] <- 2
    train$BsmtQual[train$BsmtQual == "TA"] <- 3
    train$BsmtQual[train$BsmtQual == "Gd"] <- 4
    train$BsmtQual[train$BsmtQual == "Ex"] <- 5
    
    #BsmtQual Ordinal Boxplot
    
    p2<- ggplot(train, aes(x=BsmtQual, y=SalePrice, color=BsmtQual)) +
        geom_boxplot()
    
    multiplot(p, p2)
    
    #BsmtQual LM
    
    train$BsmtQual <- as.integer(train$BsmtQual)
    summary(lm(train$SalePrice~train$BsmtQual))
    
    #BsmtCond Boxplot
    
    p <- ggplot(train, aes(x=BsmtCond, y=SalePrice, color=BsmtCond)) +
        geom_boxplot()
    p
    
    #BsmtCond mean t-tests
    
    means <- tapply(train$SalePrice, train$BsmtCond, mean)
    means <- sort(means)
    ttest <- NULL
    for (i in 1:(length(means)-1)){
        ttest[i] <- t.test(train$SalePrice[train$BsmtCond == names(means[i])], train$SalePrice[train$BsmtCond == names(means[i+1])], "l")$p.value
    }
    round(ttest, 5)
    
    #BsmtCond Ordinal Variables
    
    train$BsmtCond[train$BsmtCond == "Po"] <- 1
    train$BsmtCond[train$BsmtCond == "Fa"] <- 2
    train$BsmtCond[train$BsmtCond == "TA"] <- 3
    train$BsmtCond[train$BsmtCond == "Gd"] <- 4
    train$BsmtCond[train$BsmtCond == "Gd"] <- 5
    
    #BsmtCond Ordinal Boxplot
    
    p2<- ggplot(train, aes(x=BsmtCond, y=SalePrice, color=BsmtCond)) +
        geom_boxplot()
    
    multiplot(p, p2)
    
    #BsmtCond LM
    
    train$BsmtCond <- as.integer(train$BsmtCond)
    summary(lm(train$SalePrice~train$BsmtCond))
    
    #BsmtExposure Boxplot
    
    p <- ggplot(train, aes(x=BsmtExposure, y=SalePrice, color=BsmtExposure)) +
        geom_boxplot()
    p
    
    #BsmtExposure mean t-tests
    
    means <- tapply(train$SalePrice, train$BsmtExposure, mean)
    means <- sort(means)
    ttest <- NULL
    for (i in 1:(length(means)-1)){
        ttest[i] <- t.test(train$SalePrice[train$BsmtExposure == names(means[i])], train$SalePrice[train$BsmtExposure == names(means[i+1])], "l")$p.value
    }
    round(ttest, 5)
    
    #BsmtExposure Ordinal Variables
    
    train$BsmtExposure[train$BsmtExposure == "No"] <- 1
    train$BsmtExposure[train$BsmtExposure == "Mn"] <- 2
    train$BsmtExposure[train$BsmtExposure == "Av"] <- 3
    train$BsmtExposure[train$BsmtExposure == "Gd"] <- 4
    
    #BsmtExposure Ordinal Boxplot
    
    p2<- ggplot(train, aes(x=BsmtExposure, y=SalePrice, color=BsmtExposure)) +
        geom_boxplot()
    
    multiplot(p, p2)
    
    #BsmtExposure LM
    
    train$BsmtExposure <- as.integer(train$BsmtExposure)
    summary(lm(train$SalePrice~train$BsmtExposure))
    
    #BsmtFinType1 Boxplot
    
    p <- ggplot(train, aes(x=BsmtFinType1, y=SalePrice, color=BsmtFinType1)) +
        geom_boxplot()
    p
    
    #BsmtFinType1 mean t-tests
    
    means <- tapply(train$SalePrice, train$BsmtFinType1, mean)
    means <- sort(means)
    ttest <- NULL
    for (i in 1:(length(means)-1)){
        ttest[i] <- t.test(train$SalePrice[train$BsmtFinType1 == names(means[i])], train$SalePrice[train$BsmtFinType1 == names(means[i+1])], "l")$p.value
    }
    round(ttest, 5)
    
    #BsmtFinType1 Ordinal Variables
    
  
    train$BsmtFinType1[train$BsmtFinType1 == "Rec" | train$BsmtFinType1 == "BLQ" | train$BsmtFinType1 == "LwQ"] <- 1
    train$BsmtFinType1[train$BsmtFinType1 == "ALQ"] <- 2
    train$BsmtFinType1[train$BsmtFinType1 == "Unf"] <- 3
    train$BsmtFinType1[train$BsmtFinType1 == "GLQ"] <- 4 
    #BsmtFinType1 Ordinal Boxplot
    
    p2<- ggplot(train, aes(x=BsmtFinType1, y=SalePrice, color=BsmtFinType1)) +
        geom_boxplot()
    
    multiplot(p, p2)
    
    #BsmtFinType1 LM
    
    train$BsmtFinType1 <- as.integer(train$BsmtFinType1)
    summary(lm(train$SalePrice~train$BsmtFinType1))
    
    #BsmtFinType2 Boxplot
    
    p <- ggplot(train, aes(x=BsmtFinType2, y=SalePrice, color=BsmtFinType2)) +
        geom_boxplot()
    p
    
    #BsmtFinType2 mean t-tests
    
    means <- tapply(train$SalePrice, train$BsmtFinType2, mean)
    means <- sort(means)
    ttest <- NULL
    for (i in 1:(length(means)-1)){
        ttest[i] <- t.test(train$SalePrice[train$BsmtFinType2 == names(means[i])], train$SalePrice[train$BsmtFinType2 == names(means[i+1])], "l")$p.value
    }
    round(ttest, 5)
    
    #BsmtFinType2 Ordinal Variables
    
    train$BsmtFinType2[train$BsmtFinType2 == "Rec" | train$BsmtFinType2 == "BLQ" | train$BsmtFinType2 == "LwQ"] <- 1
    train$BsmtFinType2[train$BsmtFinType2 == "ALQ"] <- 2
    train$BsmtFinType2[train$BsmtFinType2 == "Unf"] <- 3
    train$BsmtFinType2[train$BsmtFinType2 == "GLQ"] <- 4 
    
    #BsmtFinType2 Ordinal Boxplot
    
    p2<- ggplot(train, aes(x=BsmtFinType2, y=SalePrice, color=BsmtFinType2)) +
        geom_boxplot()
    
    multiplot(p, p2)
    
    #BsmtFinType2 LM
    
    train$BsmtFinType2 <- as.integer(train$BsmtFinType2)
    summary(lm(train$SalePrice~train$BsmtFinType2))
    
    #Heating Boxplot
    
    p <- ggplot(train, aes(x=Heating, y=SalePrice, color=Heating)) +
        geom_boxplot()
    p
    
    #Heating mean t-tests
    
    means <- tapply(train$SalePrice, train$Heating, mean)
    means <- sort(means)
    ttest <- NULL
    for (i in 1:(length(means)-1)){
        ttest[i] <- t.test(train$SalePrice[train$Heating == names(means[i])], train$SalePrice[train$Heating == names(means[i+1])], "l")$p.value
    }
    round(ttest, 5)
    
    #Heating Ordinal Variables
    
    train$Heating[train$Heating == "Floor" | train$Heating == "Grav" | train$Heating == "Wall"] <- 1
    train$Heating[train$Heating == "OthW"] <- 2
    train$Heating[train$Heating == "GasW"] <- 3
    train$Heating[train$Heating == "GasA"] <- 4
    
    #Heating Ordinal Boxplot
    
    p2<- ggplot(train, aes(x=Heating, y=SalePrice, color=Heating)) +
        geom_boxplot()
    
    multiplot(p, p2)
    
    #Heating LM
    
    train$Heating <- as.integer(train$Heating)
    summary(lm(train$SalePrice~train$Heating))
    
    #HeatingQC Boxplot
    
    p <- ggplot(train, aes(x=HeatingQC, y=SalePrice, color=HeatingQC)) +
        geom_boxplot()
    p
    
    #HeatingQC mean t-tests
    
    means <- tapply(train$SalePrice, train$HeatingQC, mean)
    means <- sort(means)
    means <- means[-1]
    ttest <- NULL
    for (i in 1:(length(means)-1)){
        ttest[i] <- t.test(train$SalePrice[train$HeatingQC == names(means[i])], train$SalePrice[train$HeatingQC == names(means[i+1])], "l")$p.value
    }
    round(ttest, 5)
    
    #HeatingQC Ordinal Variables
    
    train$HeatingQC[train$HeatingQC == "Po"] <- 1
    train$HeatingQC[train$HeatingQC == "Fa"] <- 2
    train$HeatingQC[train$HeatingQC == "TA"] <- 3
    train$HeatingQC[train$HeatingQC == "Gd"] <- 4
    train$HeatingQC[train$HeatingQC == "Ex"] <- 5
        
    #HeatingQC Ordinal Boxplot
    
    p2<- ggplot(train, aes(x=HeatingQC, y=SalePrice, color=HeatingQC)) +
        geom_boxplot()
    
    multiplot(p, p2)
    
    #HeatingQC LM
    
    train$HeatingQC <- as.integer(train$HeatingQC)
    summary(lm(train$SalePrice~train$HeatingQC))
    
    #CentralAir Boxplot
    
    p <- ggplot(train, aes(x=CentralAir, y=SalePrice, color=CentralAir)) +
        geom_boxplot()
    p
    
    #CentralAir mean t-tests
    
    means <- tapply(train$SalePrice, train$CentralAir, mean)
    means <- sort(means)
    ttest <- NULL
    for (i in 1:(length(means)-1)){
        ttest[i] <- t.test(train$SalePrice[train$CentralAir == names(means[i])], train$SalePrice[train$CentralAir == names(means[i+1])], "l")$p.value
    }
    round(ttest, 5)
    
    #CentralAir Ordinal Variables
    
    train$CentralAir[!train$CentralAir == "Y"] <- 0
    train$CentralAir[train$CentralAir == "Y"] <- 1
    
    #CentralAir Ordinal Boxplot
    
    p2<- ggplot(train, aes(x=CentralAir, y=SalePrice, color=CentralAir)) +
        geom_boxplot()
    
    multiplot(p, p2)
    
    #CentralAir LM
    
    train$CentralAir <- as.integer(train$CentralAir)
    summary(lm(train$SalePrice~train$CentralAir))
    
    #Electrical Boxplot
    
    p <- ggplot(train, aes(x=Electrical, y=SalePrice, color=Electrical)) +
        geom_boxplot()
    p
    
    #Electrical mean t-tests
    
    means <- tapply(train$SalePrice, train$Electrical, mean)
    means <- sort(means)
    means <- means[-1]
    ttest <- NULL
    for (i in 1:(length(means)-1)){
        ttest[i] <- t.test(train$SalePrice[train$Electrical == names(means[i])], train$SalePrice[train$Electrical == names(means[i+1])], "l")$p.value
    }
    round(ttest, 5)
    
    #Electrical Ordinal Variables
    
    train$Electrical[train$Electrical == "Mix"] <- 1
    train$Electrical[train$Electrical == "FuseP" | train$Electrical == "FuseF"] <- 2
    train$Electrical[train$Electrical == "FuseA"] <- 3
    train$Electrical[train$Electrical == "SBrkr"] <- 4
    
    #Electrical Ordinal Boxplot
    
    p2<- ggplot(train, aes(x=Electrical, y=SalePrice, color=Electrical)) +
        geom_boxplot()
    
    multiplot(p, p2)
    
    #Electrical LM
    
    train$Electrical <- as.integer(train$Electrical)
    summary(lm(train$SalePrice~train$Electrical))
    
    #KitchenQual Boxplot
    
    p <- ggplot(train, aes(x=KitchenQual, y=SalePrice, color=KitchenQual)) +
        geom_boxplot()
    p
    
    #KitchenQual mean t-tests
    
    means <- tapply(train$SalePrice, train$KitchenQual, mean)
    means <- sort(means)
    ttest <- NULL
    for (i in 1:(length(means)-1)){
        ttest[i] <- t.test(train$SalePrice[train$KitchenQual == names(means[i])], train$SalePrice[train$KitchenQual == names(means[i+1])], "l")$p.value
    }
    round(ttest, 5)
    
    #KitchenQual Ordinal Variables
    
    train$KitchenQual[train$KitchenQual == "Po"] <- 1
    train$KitchenQual[train$KitchenQual == "Fa"] <- 2
    train$KitchenQual[train$KitchenQual == "TA"] <- 3
    train$KitchenQual[train$KitchenQual == "Gd"] <- 4
    train$KitchenQual[train$KitchenQual == "Ex"] <- 5
    
    #KitchenQual Ordinal Boxplot
    
    p2<- ggplot(train, aes(x=KitchenQual, y=SalePrice, color=KitchenQual)) +
        geom_boxplot()
    
    multiplot(p, p2)
    
    #KitchenQual LM
    
    train$KitchenQual <- as.integer(train$KitchenQual)
    summary(lm(train$SalePrice~train$KitchenQual))
    
    #Functional Boxplot
    
    p <- ggplot(train, aes(x=Functional, y=SalePrice, color=Functional)) +
        geom_boxplot()
    p
    
    #Functional mean t-tests
    
    means <- tapply(train$SalePrice, train$Functional, mean)
    means <- sort(means)
    means <- means[-2]
    ttest <- NULL
    for (i in 1:(length(means)-1)){
        ttest[i] <- t.test(train$SalePrice[train$Functional == names(means[i])], train$SalePrice[train$Functional == names(means[i+1])], "l")$p.value
    }
    round(ttest, 5)
    
    #Functional Ordinal Variables
    
    train$Functional[!train$Functional %in% c("Maj2", "Sev")] <- 1
    train$Functional[train$Functional %in% c("Maj2", "Sev")] <- 0
    
    #Functional Ordinal Boxplot
    
    p2<- ggplot(train, aes(x=Functional, y=SalePrice, color=Functional)) +
        geom_boxplot()
    
    multiplot(p, p2)
    
    #Functional LM
    
    train$Functional <- as.integer(train$Functional)
    summary(lm(train$SalePrice~train$Functional))
    
    #FireplaceQu Boxplot
    
    p <- ggplot(train, aes(x=FireplaceQu, y=SalePrice, color=FireplaceQu)) +
        geom_boxplot()
    p
    
    #FireplaceQu NA's
    
    train$FireplaceQu[is.na(train$FireplaceQu)] <- "No"
    
    #FireplaceQu mean t-tests
    
    means <- tapply(train$SalePrice, train$FireplaceQu, mean)
    means <- sort(means)
    ttest <- NULL
    for (i in 1:(length(means)-1)){
        ttest[i] <- t.test(train$SalePrice[train$FireplaceQu == names(means[i])], train$SalePrice[train$FireplaceQu == names(means[i+1])], "l")$p.value
    }
    round(ttest, 5)
    
    #FireplaceQu Ordinal Variables
    
    train$FireplaceQu[train$FireplaceQu == "Po"] <- 1
    train$FireplaceQu[train$FireplaceQu == "No"] <- 2
    train$FireplaceQu[train$FireplaceQu == "Fa"] <- 3
    train$FireplaceQu[train$FireplaceQu == "TA"] <- 4
    train$FireplaceQu[train$FireplaceQu == "Gd"] <- 5
    train$FireplaceQu[train$FireplaceQu == "Ex"] <- 6
    
    #FireplaceQu Ordinal Boxplot
    
    p2<- ggplot(train, aes(x=FireplaceQu, y=SalePrice, color=FireplaceQu)) +
        geom_boxplot()
    
    multiplot(p, p2)
    
    #FireplaceQu LM
    
    train$FireplaceQu <- as.integer(train$FireplaceQu)
    summary(lm(train$SalePrice~train$FireplaceQu))
    
    #GarageType Boxplot
    
    p <- ggplot(train, aes(x=GarageType, y=SalePrice, color=GarageType)) +
        geom_boxplot()
    p
    
    #GarageType NA's
    
    train$GarageType[is.na(train$GarageType)] <- "No"
    
    #GarageType mean t-tests
    
    means <- tapply(train$SalePrice, train$GarageType, mean)
    means <- sort(means)
    ttest <- NULL
    for (i in 1:(length(means)-1)){
        ttest[i] <- t.test(train$SalePrice[train$GarageType == names(means[i])], train$SalePrice[train$GarageType == names(means[i+1])], "l")$p.value
    }
    round(ttest, 5)
    
    #GarageType Ordinal Variables
    
    train$GarageType[train$GarageType == "No" | train$GarageType == "CarPort"] <- 1
    train$GarageType[train$GarageType %in% c("Detchd","2Types","Basment")] <- 2
    train$GarageType[train$GarageType == "Attchd"] <- 3
    train$GarageType[train$GarageType == "BuiltIn"] <- 4
    
    #GarageType Ordinal Boxplot
    
    p2<- ggplot(train, aes(x=GarageType, y=SalePrice, color=GarageType)) +
        geom_boxplot()
    
    multiplot(p, p2)
    
    #GarageType LM
    
    train$GarageType <- as.integer(train$GarageType)
    summary(lm(train$SalePrice~train$GarageType))
    
    #GarageFinish Boxplot
    
    p <- ggplot(train, aes(x=GarageFinish, y=SalePrice, color=GarageFinish)) +
        geom_boxplot()
    p
    
    #GarageFinish NA's
    
    train$GarageFinish[is.na(train$GarageFinish)] <- "No"
    
    #GarageFinish mean t-tests
    
    means <- tapply(train$SalePrice, train$GarageFinish, mean)
    means <- sort(means)
    ttest <- NULL
    for (i in 1:(length(means)-1)){
        ttest[i] <- t.test(train$SalePrice[train$GarageFinish == names(means[i])], train$SalePrice[train$GarageFinish == names(means[i+1])], "l")$p.value
    }
    round(ttest, 5)
    
    #GarageFinish Ordinal Variables
    
    train$GarageFinish[train$GarageFinish == "No"] <- 1
    train$GarageFinish[train$GarageFinish == "Unf"] <- 2
    train$GarageFinish[train$GarageFinish == "RFn"] <- 3
    train$GarageFinish[train$GarageFinish == "Fin"] <- 4
    
    #GarageFinish Ordinal Boxplot
    
    p2<- ggplot(train, aes(x=GarageFinish, y=SalePrice, color=GarageFinish)) +
        geom_boxplot()
    
    multiplot(p, p2)
    
    #GarageFinish LM
    
    train$GarageFinish <- as.integer(train$GarageFinish)
    summary(lm(train$SalePrice~train$GarageFinish))
    
    #GarageQual Boxplot
    
    p <- ggplot(train, aes(x=GarageQual, y=SalePrice, color=GarageQual)) +
        geom_boxplot()
    p
    
    #GarageQual NA's
    
    train$GarageQual[is.na(train$GarageQual)] <- "No"
    
    #GarageQual mean t-tests
    
    means <- tapply(train$SalePrice, train$GarageQual, mean)
    means <- sort(means)
    ttest <- NULL
    for (i in 1:(length(means)-1)){
        ttest[i] <- t.test(train$SalePrice[train$GarageQual == names(means[i])], train$SalePrice[train$GarageQual == names(means[i+1])], "l")$p.value
    }
    round(ttest, 5)
    
    #GarageQual Ordinal Variables
    
    train$GarageQual[train$GarageQual == "No" | train$GarageQual == "Po"] <- 1
    train$GarageQual[train$GarageQual == "Fa"] <- 2
    train$GarageQual[train$GarageQual == "TA"] <- 3
    train$GarageQual[train$GarageQual == "Gd"] <- 4
    train$GarageQual[train$GarageQual == "Ex"] <- 5
    
    #GarageQual Ordinal Boxplot
    
    p2<- ggplot(train, aes(x=GarageQual, y=SalePrice, color=GarageQual)) +
        geom_boxplot()
    
    multiplot(p, p2)
    
    #GarageQual LM
    
    train$GarageQual <- as.integer(train$GarageQual)
    summary(lm(train$SalePrice~train$GarageQual))
    
    #GarageCond NA's
    
    train$GarageCond[is.na(train$GarageCond)] <- "No"
    
    #GarageCond Boxplot
    
    p <- ggplot(train, aes(x=GarageCond, y=SalePrice, color=GarageCond)) +
        geom_boxplot()
    p
    
    #GarageCond mean t-tests
    
    means <- tapply(train$SalePrice, train$GarageCond, mean)
    means <- sort(means)
    means <- means[-4]
    ttest <- NULL
    for (i in 1:(length(means)-1)){
        ttest[i] <- t.test(train$SalePrice[train$GarageCond == names(means[i])], train$SalePrice[train$GarageCond == names(means[i+1])], "l")$p.value
    }
    round(ttest, 5)
    
    #GarageCond Ordinal Variables
    
    train$GarageCond[!train$GarageCond %in% c("Gd", "TA")] <- 0
    train$GarageCond[train$GarageCond %in% c("Gd", "TA")] <- 1

    #GarageCond Ordinal Boxplot
    
    p2<- ggplot(train, aes(x=GarageCond, y=SalePrice, color=GarageCond)) +
        geom_boxplot()
    
    multiplot(p, p2)
    
    #GarageCond LM
    
    train$GarageCond <- as.integer(train$GarageCond)
    summary(lm(train$SalePrice~train$GarageCond))
    
    #PavedDrive Boxplot
    
    p <- ggplot(train, aes(x=PavedDrive, y=SalePrice, color=PavedDrive)) +
        geom_boxplot()
    p
    
    #PavedDrive mean t-tests
    
    means <- tapply(train$SalePrice, train$PavedDrive, mean)
    means <- sort(means)
    ttest <- NULL
    for (i in 1:(length(means)-1)){
        ttest[i] <- t.test(train$SalePrice[train$PavedDrive == names(means[i])], train$SalePrice[train$PavedDrive == names(means[i+1])], "l")$p.value
    }
    round(ttest, 5)
    
    #PavedDrive Ordinal Variables
    
    train$PavedDrive[train$PavedDrive == "N"] <- 1
    train$PavedDrive[train$PavedDrive == "P"] <- 2
    train$PavedDrive[train$PavedDrive == "Y"] <- 3

    #PavedDrive Ordinal Boxplot
    
    p2<- ggplot(train, aes(x=PavedDrive, y=SalePrice, color=PavedDrive)) +
        geom_boxplot()
    
    multiplot(p, p2)
    
    #PavedDrive LM
    
    train$PavedDrive <- as.integer(train$PavedDrive)
    summary(lm(train$SalePrice~train$PavedDrive))
    
    #PoolQC Boxplot
    
    p <- ggplot(train, aes(x=PoolQC, y=SalePrice, color=PoolQC)) +
        geom_boxplot()
    p
    
    #PoolQC NA's
    
    train$PoolQC[is.na(train$PoolQC)] <- "No"
    
    #PoolQC mean t-tests
    
    means <- tapply(train$SalePrice, train$PoolQC, mean)
    means <- sort(means)
    ttest <- NULL
    for (i in 1:(length(means)-1)){
        ttest[i] <- t.test(train$SalePrice[train$PoolQC == names(means[i])], train$SalePrice[train$PoolQC == names(means[i+1])], "l")$p.value
    }
    round(ttest, 5)
    
    #PoolQC Ordinal Variables
    
    train$PoolQC[train$PoolQC != "Ex"] <- 0
    train$PoolQC[train$PoolQC == "Ex"] <- 1
    
    #PoolQC Ordinal Boxplot
    
    p2<- ggplot(train, aes(x=PoolQC, y=SalePrice, color=PoolQC)) +
        geom_boxplot()
    
    multiplot(p, p2)
    
    #PoolQC LM
    
    train$PoolQC <- as.integer(train$PoolQC)
    summary(lm(train$SalePrice~train$PoolQC))
    
    #Fence Boxplot
    
    p <- ggplot(train, aes(x=Fence, y=SalePrice, color=Fence)) +
        geom_boxplot()
    p
    
    #Fence NA's
    
    train$Fence[is.na(train$Fence)] <- "No"
    
    #Fence mean t-tests
    
    means <- tapply(train$SalePrice, train$Fence, mean)
    means <- sort(means)
    ttest <- NULL
    for (i in 1:(length(means)-1)){
        ttest[i] <- t.test(train$SalePrice[train$Fence == names(means[i])], train$SalePrice[train$Fence == names(means[i+1])], "l")$p.value
    }
    round(ttest, 5)
    
    #Fence Ordinal Variables
    
    train$Fence[train$Fence == "MnWw" | train$Fence == "GdWo" | train$Fence == "MnPrv"] <- 0
    train$Fence[train$Fence == "GdPrv" | train$Fence == "No"] <- 1
    
    #Fence Ordinal Boxplot
    
    p2<- ggplot(train, aes(x=Fence, y=SalePrice, color=Fence)) +
        geom_boxplot()
    
    multiplot(p, p2)
    
    #Fence LM
    
    train$Fence <- as.integer(train$Fence)
    summary(lm(train$SalePrice~train$Fence))
    
    #MiscFeature Boxplot
    
    p <- ggplot(train, aes(x=MiscFeature, y=SalePrice, color=MiscFeature)) +
        geom_boxplot()
    p
    
    #MiscFeature mean t-tests
    
    means <- tapply(train$SalePrice, train$MiscFeature, mean)
    means <- sort(means)
    means <- means[-4]
    ttest <- NULL
    for (i in 1:(length(means)-1)){
        ttest[i] <- t.test(train$SalePrice[train$MiscFeature == names(means[i])], train$SalePrice[train$MiscFeature == names(means[i+1])], "l")$p.value
    }
    round(ttest, 5)
    
    #MiscFeature Ordinal Variables
    
    train$MiscFeature <- 0
    
    #MiscFeature Ordinal Boxplot
    
    p2<- ggplot(train, aes(x=MiscFeature, y=SalePrice, color=MiscFeature)) +
        geom_boxplot()
    
    multiplot(p, p2)
    
    #MiscFeature LM
    
    train$MiscFeature <- as.integer(train$MiscFeature)
    summary(lm(train$SalePrice~train$MiscFeature))
    
    #SaleType Boxplot
    
    p <- ggplot(train, aes(x=SaleType, y=SalePrice, color=SaleType)) +
        geom_boxplot()
    p
    
    #SaleType mean t-tests
    
    means <- tapply(train$SalePrice, train$SaleType, mean)
    means <- sort(means)
    ttest <- NULL
    for (i in 1:(length(means)-1)){
        ttest[i] <- t.test(train$SalePrice[train$SaleType == names(means[i])], train$SalePrice[train$SaleType == names(means[i+1])], "l")$p.value
    }
    round(ttest, 5)
    
    #SaleType Ordinal Variables
    
    train$SaleType[!train$SaleType %in% c("Oth", "ConLD", "ConLw", "COD")] <- 1
    train$SaleType[train$SaleType %in% c("Oth", "ConLD", "ConLw", "COD")] <- 0

    
    #SaleType Ordinal Boxplot
    
    p2<- ggplot(train, aes(x=SaleType, y=SalePrice, color=SaleType)) +
        geom_boxplot()
    
    multiplot(p, p2)
    
    #SaleType LM
    
    train$SaleType <- as.integer(train$SaleType)
    summary(lm(train$SalePrice~train$SaleType))
    
    #SaleCondition Boxplot
    
    p <- ggplot(train, aes(x=SaleCondition, y=SalePrice, color=SaleCondition)) +
        geom_boxplot()
    p
    
    #SaleCondition mean t-tests
    
    means <- tapply(train$SalePrice, train$SaleCondition, mean)
    means <- sort(means)
    ttest <- NULL
    for (i in 1:(length(means)-1)){
        ttest[i] <- t.test(train$SalePrice[train$SaleCondition == names(means[i])], train$SalePrice[train$SaleCondition == names(means[i+1])], "l")$p.value
    }
    round(ttest, 5)
    
    #SaleCondition Ordinal Variables
    
    train$SaleCondition[train$SaleCondition == "AdjLand"] <- 1
    train$SaleCondition[train$SaleCondition %in% c("Family","Abnorml", "Alloca", "Normal")] <- 2
    train$SaleCondition[train$SaleCondition == "Partial"] <- 3
    
    #SaleCondition Ordinal Boxplot
    
    p2<- ggplot(train, aes(x=SaleCondition, y=SalePrice, color=SaleCondition)) +
        geom_boxplot()
    
    multiplot(p, p2)
    
    #SaleCondition LM
    
    train$SaleCondition <- as.integer(train$SaleCondition)
    summary(lm(train$SalePrice~train$SaleCondition))
    
#Plots
    
correlations <- cor(train, use="everything")
corrplot(correlations, method="circle", type="lower",  sig.level = 0.01, insig = "blank")

scatterplot(SalePrice ~ YearBuilt, data=train,  xlab="Year Built", ylab="Sale Price", grid=FALSE)
    
scatterplot(SalePrice ~ YrSold, data=train,  xlab="Year Sold", ylab="Sale Price", grid=FALSE)    

scatterplot(SalePrice ~ X1stFlrSF, data=train,  xlab="Square Footage Floor 1", ylab="Sale Price", grid=FALSE)

#Prep Model

outcome <- train$SalePrice

partition <- createDataPartition(y=outcome,
                                 p=.5,
                                 list=F)
training <- train[partition,]
testing <- train[-partition,]

#Linear Model

lm_model_15 <- lm(SalePrice ~ ., data=training)
summary(lm_model_15)

lm_model_15 <- lm(SalePrice ~ MSSubClass+LotFrontage+LotArea+
                      LandContour+Alley+LotConfig+
                      Neighborhood+Condition1+
                      HouseStyle+OverallQual+
                      OverallCond+RoofMatl+MasVnrArea+ExterQual+ExterCond+
                      BsmtQual+BsmtExposure+BsmtFinSF1+X1stFlrSF+X2ndFlrSF+
                      +KitchenAbvGr+KitchenQual+TotRmsAbvGrd+Fireplaces+
                      GarageCars+GarageQual+ScreenPorch+PoolArea+PoolQC+
                      SaleCondition, data=training)
summary(lm_model_15)

lm_model_15 <- lm(SalePrice ~ MSSubClass+LotFrontage+LotArea+
                      LotConfig+
                      Neighborhood+Condition1+
                      HouseStyle+OverallQual+
                      OverallCond+MasVnrArea+
                      BsmtQual+BsmtExposure+BsmtFinSF1+X1stFlrSF+X2ndFlrSF+
                      +KitchenQual+TotRmsAbvGrd+Fireplaces+
                      GarageCars+ScreenPorch+PoolArea+PoolQC+
                      SaleCondition, data=training)

summary(lm_model_15)

#Predictions

prediction <- predict(lm_model_15, testing, type="response")
model_output <- cbind(testing, prediction)
model_output <- model_output[!is.na(prediction),]

model_output$log_prediction <- log(model_output$prediction)
model_output$log_SalePrice <- log(model_output$SalePrice)

#Test with RMSE

rmse(model_output$log_SalePrice,model_output$log_prediction)

#Random Forests

model_1 <- randomForest(SalePrice ~ ., data=training)


# Predict using the test set
prediction <- predict(model_1, testing)
model_output <- cbind(testing, prediction)


model_output$log_prediction <- log(model_output$prediction)
model_output$log_SalePrice <- log(model_output$SalePrice)

#Test with RMSE

rmse(model_output$log_SalePrice,model_output$log_prediction)
    