library(caret)
library(plyr)
library(ROCR)
library(ClustOfVar)
library(reshape)
library(scales)
library(data.table)
library(shiny)
library(shinydashboard)
library(googleVis)
library(sqldf)

#############################################################################
# MISCELLENEOUS FUNCTIONS
#############################################################################
Rank <- function(x) {
  # Rank function ignore NA
  #
  # Args:
  #   x: vector whose rank function would be applied
  r <- rank(x) / sum(!is.na(x))
  r[is.na(x)] <- NA
  return(r)
}
#' Mode function
#' @param x: Vector
#' @param na.rm: Remove NA; default = FALSE
#' @return Mode of x
#' @export
Mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

#############################################################################
# VARS
#############################################################################
#' Create Variable Summary Table
#'
#' Create Variable Summary Table with general rules to drop
#'   certain variables with low value to model / explore. Act as a primary
#'   variable selection for the rest of the package functions
#' @param df Dataframe
#' @param sv Vector of values user defined and can be detected if unusually
#' high proportion of the variable has the values; default = c(9999, 99999).
#' @param maxCatLevel Maximum categorical levels to be excluded in Shiny Dashboard
#' output; default = 50.
#' @param maxNumPct Maximum percent of one single value of the variable to be
#' exlcuded in Shiny Dashboard.
#' @return Dataframe of variable summary table.
#' Variable Reduction Rules:
#'   1. Categorical and more than maxCatLevel (default) 50
#'   2. Only one value
#'   3. Numeric variable & one value more than (default) 99%
#'   4. Missing value more than (default) 99%
#'   5. Zero value more than 99.5%
#'   6. Outlier (over 5 standard deviation) more than 99.5%
#'   7. Special value more than 99.5%
#' @export
Vars <- function(df,
                 sv = c(9999, 99999),
                 maxCatLevel = 50,
                 maxNumPct = .99) {
  if (!is.data.frame(df)){  # Check if data.frame
    return("Data is not a data.frame")
  } else {
    Vars <- data.frame()
    # Loop through all columns
    for (i in names(df)) {
      # Create temp column for easier coding
      df$X <- df[,i]
      VarType <- class(df$X)
      # Frequency table by column values
      t <- data.frame()
      t <- as.data.frame(ftable(df$X))
      if (VarType %in% c('integer', 'numeric')) {
        t$Var1 <- as.numeric(levels(t$Var1))
      }
      # Additional stats
      N <- nrow(df)
      NwValues <- nrow(subset(df, is.na(X)==FALSE, X))
      NMiss <- N - NwValues
      NMissPct <- NMiss / N
      NZero <- as.numeric(subset(t, Var1 == 0, select = Freq))
      NZero <- ifelse(is.na(NZero), 0, NZero)
      NZeroPct <- NZero / N
      NSV <- ifelse(is.na(t$Freq[t$Var1 %in% sv][1]), 0,
                    t$Freq[t$Var1 %in% sv][1])
      if (VarType %in% c('integer', 'numeric')) {
        mean <- mean(df$X, na.rm = TRUE)
        min <- min(df$X, na.rm = TRUE)
        max <- max(df$X, na.rm = TRUE)
        SDev <- sd(df$X, na.rm = TRUE)
      } else {
        mean <- NA
        min <- NA
        max <- NA
        SDev <- NA
      }
      low <- mean - 5 * SDev
      high <- mean + 5 * SDev
      #t <- t[complete.cases(t),]
      if (VarType %in% c('integer', 'numeric')) {
        NOutlier <- sum(t$Freq[t$Var1 < low | t$Var1 > high])
      }
      else {
        NOutlier <- NA
      }
      MaxPct <- max(t$Freq)/N
      NValues <- nrow(subset(t, is.na(Var1) == FALSE, select = Freq))
      # Variable Selection Rules
      if (VarType %in% c('character', 'factor', 'Date') &
          NValues > maxCatLevel) {
        Drop = 'Y'
      } else if (NValues == 1) {
        Drop = 'Y'
      } else if (VarType %in% c('integer', 'numeric') & MaxPct > maxNumPct) {
        Drop = 'Y'
      } else if (NMissPct > maxNumPct) {
        Drop = 'Y'
      } else if (NZero/N > .995) {
        Drop = 'Y'
      } else if (NSV/N > .995){
        Drop = 'Y'
      } else if (VarType %in% c('integer', 'numeric') & NOutlier/N > .995) {
        Drop = 'Y'
      } else {
        Drop = 'N'
      }
      # Final Vars Table
      Vars_temp <- data.frame(var = i, VarType, N, NValues, MaxPct, NMiss,
                              NMissPct, NZero, NZeroPct, NSV, NOutlier, mean,
                              min, max, SDev, Drop)
      Vars <- rbind(Vars, Vars_temp)
      Vars[is.na(Vars)] <- 0
    }
    return(Vars)
  }
}

#############################################################################
# CHISQ
#############################################################################
#'Chisq - chi-square and c-stats on non-binned basis
#'@param df Dataframe
#'@param Y Binary event variable (0, 1) name. Integer (\code{Int}) is required.
#'@param Vars Dataframe of the variable summary table from function Vars().
#'@return Dataframe of variable chi-square summary.
#'@export
Chisq <- function(df, Y, Vars) {
  # Check data frame and formats
  if (!is.data.frame(df)){ # Check if data.frame
    return("Data is not a data.frame")
  } else if (is.numeric(Y)){ # Check if target variable is numeric
    return("Characteristic name not string")
  } else if (grepl("[.]",Y)){ # Check if there is a dot
    return("Name of a characteristic must not have a dot [.]")
  } else if (max(df[,Y],na.rm=T)!=1){
    return("Maximum not 1")
  } else if (min(df[,Y],na.rm=T)!=0){
    return("Minimum not 0")
  } else {
    SelVar <- as.list(t(subset(Vars, Drop == 'N', select = var)))
    Chisq <- data.frame()
    for (i in SelVar) {
      df$YY <- df[[Y]]
      df$var <- df[[i]]

      # Chi-sqaure test and p-value
      freq <- table(df[[Y]], df$var)
      chi <- suppressWarnings(chisq.test(freq))
      ChiSq <- as.numeric(chi[1])
      ChiSqP <- as.numeric(chi[3])

      # C, DevC50
      m<-suppressWarnings(glm(YY~var,data=df,family=binomial()))
      df$score<-predict(m,type='response',df)
      pred<-prediction(df$score,df$YY)
      C <- attr(performance(pred, "auc"), 'y.values')[[1]]
      DevC50 <- abs(C-0.5)
      perf <- performance(pred,"tpr","fpr")

      # Combine all tests
      Chisq_temp <- data.frame(var = i, ChiSq, ChiSqP, C, DevC50)
      Chisq <- rbind(Chisq, Chisq_temp)
      Chisq.sort <- Chisq[with(Chisq, order(-DevC50)), ]
    }
    Chisq <- merge(Chisq, Chisq.sort, by.x = "var", by.y = "var", all.x = TRUE)
    return(Chisq.sort)
  }
}

#############################################################################
# UNIVARS
#############################################################################
#' Univariate Analysis
#' @param df Dataframe
#' @param Vars Dataframe of variable summary table from function Vars()
#' @return Dataframe of univariate summary
#' @export
Univars <- function(df, Vars) {
  # Check data frame and formats
  if (!is.data.frame(df)){ # Check if data.frame
    return("Data is not a data.frame")
  } else {
    SelVar <- as.list(t(
      subset(Vars, Drop == 'N' & VarType %in% c('integer', 'numeric'),
             select = var)))
    Univars <- data.frame()
    for (i in SelVar) {
      df$var <- df[[i]]
      NumObs <- nrow(df)
      NNonMiss <- nrow(subset(df, is.na(var)==FALSE, var))
      Mean <- mean(df$var, na.rm = TRUE)
      Median <- mean(df$var, na.rm = TRUE)
      Max <- max(df$var, na.rm = TRUE)
      Min <- min(df$var, na.rm = TRUE)
      Mode <- Mode(df$var, na.rm = TRUE)
      P1 <- quantile(df$var, .01, na.rm = TRUE)
      P5 <- quantile(df$var, .05, na.rm = TRUE)
      #P10 <- quantile(df$var, .10, na.rm = TRUE)
      #P15 <- quantile(df$var, .15, na.rm = TRUE)
      #P20 <- quantile(df$var, .20, na.rm = TRUE)
      P25 <- quantile(df$var, .25, na.rm = TRUE)
      #P30 <- quantile(df$var, .30, na.rm = TRUE)
      #P35 <- quantile(df$var, .35, na.rm = TRUE)
      #P40 <- quantile(df$var, .40, na.rm = TRUE)
      #P45 <- quantile(df$var, .45, na.rm = TRUE)
      #P50 <- quantile(df$var, .50, na.rm = TRUE)
      #P55 <- quantile(df$var, .55, na.rm = TRUE)
      #P60 <- quantile(df$var, .60, na.rm = TRUE)
      #P65 <- quantile(df$var, .65, na.rm = TRUE)
      #P70 <- quantile(df$var, .70, na.rm = TRUE)
      P75 <- quantile(df$var, .75, na.rm = TRUE)
      #P80 <- quantile(df$var, .80, na.rm = TRUE)
      #P85 <- quantile(df$var, .85, na.rm = TRUE)
      P90 <- quantile(df$var, .90, na.rm = TRUE)
      P95 <- quantile(df$var, .95, na.rm = TRUE)
      P99 <- quantile(df$var, .99, na.rm = TRUE)
      #P100 <- quantile(df$var, 1, na.rm = TRUE)
      # Combine all tests
      Univars_temp <- data.frame(var = i, NumObs, NNonMiss, Mean, Median, Mode,
                                 Max, Min, P1, P5, P25, P75, P90, P95, P99)
      Univars <- rbind(Univars, Univars_temp)
    }
    return(Univars)
  }
}

#############################################################################
# FREQ
#############################################################################
#' Categorical Variable Frequency Table
#' @param df Dataframe
#' @param Vars Dataframe of Variable Summary Table from function Vars()
#' @return Dataframe of frequency table
#' @export
Freq <- function(df, Vars) {
  # Check data frame and formats
  if (!is.data.frame(df)){ # Check if data.frame
    return("Data is not a data.frame")
  } else {
    SelVar <- as.list(t(
      subset(Vars, Drop == 'N' & !(VarType %in% c('integer', 'numeric')),
             select = var)))
    Freq <- data.frame()
    for (i in SelVar) {
      df$var <- df[[i]]

      t1 <- ddply(df, .(var), summarise, Freq = length(var))
      t1$VarName <- i
      totalcount <- nrow(df)
      t1$Percent <- t1$Freq / totalcount
      t1 <- t1[c(3, 1, 2, 4)]
      t1 <- rename(t1, c("var" = "VarValue"))

      # Combine all tests
      Freq <- rbind(Freq, t1)
    }
    return(Freq)
  }
}

#############################################################################
# STATS
#############################################################################
# Numeric Variable Stats through testing
stats_num <- function(df, Y, Vars, groups = 100) {
  stats_num <- data.frame()
  SelVar <- as.list(t(
    subset(Vars, Drop == 'N' & VarType %in% c('integer', 'numeric'),
           select = var)))
  totalevent <- sum(df$Y)
  totalnonevent <- nrow(df)-totalevent
  for (i in SelVar) {
    df$var <- df[[i]]
    df$YY <- df[[Y]]

    df$bin <- round(Rank(df$var) *groups)
    t1 <- ddply(df, .(bin), summarise, event = sum(YY), count = length(YY))

    # Information Value
    t1$totalevent <- sum(df$YY)
    t1$totalnonevent <- nrow(df)-sum(df$YY)
    t1$nonevent <- t1$count - t1$event
    t1$pctevent <- ifelse(t1$totalevent==0, 0, t1$event/t1$totalevent)
    t1$pctnonevent <- ifelse(t1$totalnonevent==0, 0,
                             t1$nonevent/t1$totalnonevent)
    t1$woe <- ifelse(t1$pctnonevent==0 | t1$pctevent==0, 0,
                     log(t1$pctevent/t1$pctnonevent))
    t1 <- within(t1, iv <- woe * (pctevent-pctnonevent))
    iv <- sum(t1$iv)
    bins <- nrow(t1)

    # Chi-sqaure test and p-value
    freq <- table(df$YY, df$bin)
    chi <- suppressWarnings(chisq.test(freq))
    chisq <- as.numeric(chi[1])
    pvalue <- as.numeric(chi[3])

    # ROC, Gini and KS
    m<-suppressWarnings(glm(YY~var,data=df,family=binomial()))
    df$score<-predict(m,type='response',df)
    pred<-prediction(df$score,df$YY)
    perf <- performance(pred,"tpr","fpr")
    c <- attr(performance(pred, "auc"), 'y.values')[[1]]
    c50 <- abs(c-0.5)
    gini <- c50*2

    pred<-prediction(df$score,df$YY)
    perf <- performance(pred,"tpr","fpr")
    ks <- max(attr(perf,'y.values')[[1]]-attr(perf,'x.values')[[1]])
    df.sort <- df[with(df, order(score)),]
    df.sort$cnt <- 1
    df.sort$cnt <- with(df.sort, cumsum(cnt))
    df.sort$YYY <- ifelse(is.na(df.sort$var), NA, df.sort$YY)
    df.sort$YYYY <- ifelse(df.sort$YY==1, 0, 1)
    noneventcnt <- sum(df.sort$YYYY, na.rm=TRUE)
    df.sort$pred <- ifelse(df.sort$cnt <noneventcnt , 0, 1)
    misclassrate <- mean(ifelse(df.sort$pred==df.sort$YY, 0, 1), na.rm=TRUE)

    # Combine all tests
    stats_num_temp <- data.frame(var = i, bins, c, c50, gini, ks, chisq, pvalue,
                                 misclassrate, iv)
    stats_num <- rbind(stats_num, stats_num_temp)
  }
  return(stats_num)
}
# Character variable stats testing
stats_char <- function(df, Y, Vars, groups = 100) {
  stats_char <- data.frame()
  SelVar <- as.list(t(
    subset(Vars, Drop == 'N' & !(VarType %in% c('integer', 'numeric')),
           select = var)))
  totalevent <- sum(df$Y)
  totalnonevent <- nrow(df)-totalevent
  for (i in SelVar) {
    df$var <- df[[i]]
    df$YY <- df[[Y]]

    t1 <- ddply(df, .(var), summarise, mean = mean(YY), event = sum(YY),
                count = length(YY))
    t1$bin <- round(Rank(t1$mean) *groups)
    df2 <- merge(df, t1, by.x = "var", by.y = "var", all.x = TRUE)

    # Information Value
    t1$totalevent <- sum(df$YY)
    t1$totalnonevent <- nrow(df)-sum(df$YY)
    t1$nonevent <- t1$count - t1$event
    t1$pctevent <- ifelse(t1$totalevent==0, 0, t1$event/t1$totalevent)
    t1$pctnonevent <- ifelse(t1$totalnonevent==0, 0,
                             t1$nonevent/t1$totalnonevent)
    t1$woe <- ifelse(t1$pctnonevent==0 | t1$pctevent==0, 0,
                     log(t1$pctevent/t1$pctnonevent))
    t1 <- within(t1, iv <- woe * (pctevent-pctnonevent))
    iv <- sum(t1$iv)
    bins <- nrow(t1)

    # Chi-sqaure test and p-value
    freq <- table(df2$YY, df2$bin)
    chi <- suppressWarnings(chisq.test(freq))
    chisq <- as.numeric(chi[1])
    pvalue <- as.numeric(chi[3])

    # ROC, Gini and KS
    m<-suppressWarnings(glm(YY~factor(var),data=df2,family=binomial()))
    df2$score<-predict(m,type='response',df2)
    pred<-prediction(df2$score,df2$YY)
    perf <- performance(pred,"tpr","fpr")
    c <- attr(performance(pred, "auc"), 'y.values')[[1]]
    c50 <- abs(c-0.5)
    gini <- c50*2

    pred<-prediction(df2$score,df2$YY)
    perf <- performance(pred,"tpr","fpr")
    ks <- max(attr(perf,'y.values')[[1]]-attr(perf,'x.values')[[1]])
    df2.sort <- df2[with(df2, order(score)),]
    df2.sort$cnt <- 1
    df2.sort$cnt <- with(df2.sort, cumsum(cnt))
    df2.sort$YYY <- ifelse(is.na(df2.sort$var), NA, df2.sort$YY)
    df2.sort$YYYY <- ifelse(df2.sort$YY==1, 0, 1)
    noneventcnt <- sum(df2.sort$YYYY, na.rm=TRUE)
    df2.sort$pred <- ifelse(df2.sort$cnt <noneventcnt , 0, 1)
    misclassrate <- mean(ifelse(df2.sort$pred==df2.sort$YY, 0, 1), na.rm=TRUE)

    m<-suppressWarnings(glm(YY~var,data=df,family=binomial()))
    df$score<-predict(m,type='response',df)
    pred<-prediction(df$score,df$YY)
    C <- attr(performance(pred, "auc"), 'y.values')[[1]]
    DevC50 <- abs(C-0.5)
    perf <- performance(pred,"tpr","fpr")
    # Combine all tests
    stats_char_temp <- data.frame(var = i, bins, c, c50, gini, ks, chisq,
                                  pvalue, misclassrate, iv)
    stats_char <- rbind(stats_char, stats_char_temp)
  }
  return(stats_char)
}
# Combine numeric and character results
#' Statistical Analysis Summary
#' @param df Dataframe
#' @param Y Binary event variable (0, 1) name. Integer (\code{Int}) is required.
#' @param Vars Dataframe of Variable Summary Table from function Vars()
#' @return Dataframe of statistical summary
#' @export
Stats <- function(df, Y, Vars) {
  # Check data frame and formats
  if (!is.data.frame(df)){ # Check if data.frame
    return("Data is not a data.frame")
  } else if (is.numeric(Y)){ # Check if target variable is numeric
    return("Characteristic name not string")
  } else if (grepl("[.]",Y)){ # Check if there is a dot
    return("Name of a characteristic must not have a dot [.]")
  } else if (max(df[,Y],na.rm=T)!=1){
    return("Maximum not 1")
  } else if (min(df[,Y],na.rm=T)!=0){
    return("Minimum not 0")
  } else {
    stats_num <- stats_num(df, Y, Vars)
    stats_char <- stats_char(df, Y, Vars)
    stats <- rbind(stats_num, stats_char)
    stats.sort <- stats[with(stats, order(-c50)),]
    return(stats.sort)
  }
}

#############################################################################
# VARCLUS
#############################################################################
#' Variable Clustering Analysis
#' @param df Dataframe
#' @param Y Binary event variable (0, 1) name. Integer (\code{Int}) is required.
#' @param maxEigenValue Eigen value cutoff, defualt = 0.7.
#' @return Dataframe of Variable Cluster Analysis results.
#' @export
Varclus <- function(df, Y, Vars, maxEigenValue = 0.7) {
  # Check data frame and formats
  if (!is.data.frame(df)){ # Check if data.frame
    return("Data is not a data.frame")
  } else if (is.numeric(Y)){ # Check if target variable is numeric
    return("Characteristic name not string")
  } else if (grepl("[.]",Y)){ # Check if there is a dot
    return("Name of a characteristic must not have a dot [.]")
  } else if (max(df[,Y],na.rm=T)!=1){
    return("Maximum not 1")
  } else if (min(df[,Y],na.rm=T)!=0){
    return("Minimum not 0")
  } else {
    # selected variable only
    SelVar <- as.vector(t(subset(Vars, Drop == 'N', select = var)))
    df1 <- subset(df, select = c(SelVar))

    # Drop dependent variable
    df1 <- df1[, !(names(df1) %in% Y)]
    SelVar <- names(df1)

    # transform to matrix for hclustvar function
    df1 <- data.matrix(df1)

    # ClustOfVar package for Tree based Variable Clustering
    # Use maxEigenValue to stop at semi-optimal number of clusters
    tree <- hclustvar(df1)
    NumClust <- sum(tree$height > maxEigenValue) + 1
    P <- cutreevar(tree, k = NumClust, matsim = TRUE)
    t <- P$var
    scores <- as.data.frame(P$scores)
    tdf <- rename(melt.list(t), c("X1" = "var", "value" = "RSquareOwnCluster",
                                  "L1" = "cluster"))
    tdf <- subset(tdf, select = -c(X2))
    tdf <- tdf[, c(3, 1, 2)]

    # Algoritm to mimic SAS Proc Varclus with closest cluster R-squared value to
    # select best variable within cluster
    RSquare <- data.frame()
    SelVar <- as.vector(t(subset(Vars, Drop == 'N', select = var)))
    df1 <- subset(df, select = c(SelVar))
    df1 <- df1[, !(names(df1) %in% Y)]
    SelVar <- names(df1)
    for (i in SelVar) {
      df2 <- df1[[i]]
      cluster <- as.numeric(substr(tdf[which(tdf$var == i), 1], 8, 10000))
      scores_temp <- scores[,-c(cluster)]
      m <- cbind(df2, scores_temp)

      m <- data.matrix(m)
      m <- as.data.frame(cor(m, use = "na.or.complete"))
      RSquareClosestCluster <- max((m[which(m$df2 !=1),1])^2)
      RSquare_temp <- data.frame(var2 = i, RSquareClosestCluster)
      RSquare <- rbind(RSquare, RSquare_temp)
    }

    # generate final dataset and calculate 1-Rsquared Ratio
    tdf <- subset(cbind(tdf, RSquare), select = -c(var2))
    tdf$RSquareRatio <- (1-tdf$RSquareOwnCluster) /
      (1-tdf$RSquareClosestCluster)
    tdf$cluster <- as.numeric(substr(tdf$cluster, 8, 10000))
    tdf <- tdf[with(tdf, order(cluster, RSquareRatio)),]
    return(tdf)
  }
}

#############################################################################
# RUNALL
#############################################################################
#' Building a set of variable explore tools mainly for binary prediction.
#'
#' Output into Shiny Dashboard for user interaction, especially numeric
#' variable binning and WOE summary with SQL codes available for transformation.
#'
#' @param df Dataframe
#' @param Y Binary event variable (0, 1) name. Integer (\code{Int}) is required.
#' @param groups Integer variable for default grouping in numeric variable.
#' Default is 20 groups / bins
#' @return Generate a Shiny Dashboard object containing 5 tabs: Variable
#' Summary, Variable Clustering, Log-Odds Charts for numeric variables
#' interactive binning, Log-Odds Charts for categorical variables, and export
#' results to CSV file format
#' @export
varexplore <- function(df, Y, groups = 20) {
  #Create Summaries
  Vars <- Vars(df)
  Univars <- Univars(df, Vars)
  Freq <- Freq(df, Vars)
  Chisq <- Chisq(df, Y, Vars)
  Stats <- Stats(df, Y, Vars)
  Varclus <- Varclus(df, Y, Vars)
  SelVar <- t(subset(Vars, Drop == 'N' & VarType %in% c('integer', 'numeric'),
                     select = var))
  SelVarC <- t(subset(Vars, Drop == 'N' & VarType %in% c('character', 'factor',
                                                         'Date'), select = var))
  SelVarA <- t(subset(Vars, Drop == 'N', select = var))
  df1 <- df
  df1$YY <- df[[Y]]
  server <- function(input, output, session) {
    # Reactive
    dataUpload<-reactive({
      # Selected Variable and Numeric Only for interactive binning
      SelVarc <- t(subset(Vars, Drop == 'N' & VarType %in% c('character',
                                                             'factor', 'Date'),
                          select = var))
      df <- subset(df, select = c(SelVarC))
      updateSelectInput(session, "product", choices = names(df))
      df$YY <- df1$YY
      return(df)
    })
    dataUpload2<-reactive({
      # Selected Variable and Numeric Only for interactive binning
      SelVar <- t(subset(Vars, Drop == 'N' & VarType %in% c('integer',
                                                            'numeric'),
                         select = var))
      df <- subset(df, select = c(SelVar))
      updateSelectInput(session, "product2", choices = names(df))
      df$YY <- df1$YY
      return(df)
    })
    dataUpload3<-reactive({
      # Selected Variable and Numeric Only for histogram
      SelVar <- t(subset(Vars, Drop == 'N', select = var))
      df <- subset(df, select = c(SelVar))
      updateSelectInput(session, "product3", choices = names(df))
      df$YY <- df1$YY
      return(df)
    })

    # Render
    output$viewVars <- renderGvis(
      {gvisTable(Vars,
                 formats = list('MaxPct'='#.#%', 'NMissPct'='#.#%',
                                'NZeroPct'='#.#%',
                                'N'='#,###.##', 'NValues'='#,###.##',
                                'NMiss'='#,###.##', 'NZero'='#,###.##',
                                'NSV'='#,###.##', 'NOutlier'='#,###.##',
                                'mean'='#,###.##', 'min'='#,###.##',
                                'max'='#,###.##', 'SDev'='#,###.##'))})
    output$viewUnivars <- renderGvis(
      {gvisTable(Univars,
                 formats = list(NumObs = '#,###.##', NNonMiss = '#,###.##',
                                Mean = '#,###.##', Median = '#,###.##',
                                Mode = '#,###.##', Max = '#,###.##',
                                Min = '#,###.##', P1 = '#,###.##',
                                P5 = '#,###.##', P25 = '#,###.##',
                                P75 = '#,###.##', P90 = '#,###.##',
                                P95 = '#,###.##', P99 = '#,###.##'))})
    output$viewChisq <- renderGvis(
      {gvisTable(Chisq,
                 formats = list(ChiSq = '#,###.##', ChiSqP = '#.####',
                                C = '#.####', DevC50 = '#.####'))})
    output$viewStats <- renderGvis(
      {gvisTable(Stats,
                 formats = list(c = '#.####', c50 = '#.####', gini = '#.####',
                                ks = '#.####', chisq = '#,###.##',
                                pvalue = '#.####', misclassrate = '#.####',
                                iv = '#.####'))})
    output$viewFreq <- renderGvis(
      {gvisTable(Freq)})
    output$viewVarclus <- renderGvis(
      {gvisTable(Varclus,
                 formats = list(RSquareOwnCluster = '#.####',
                                RSquareClosestCluster = '#.####',
                                RSquareRatio = '#.####'))})
    # Categorical Variable Logodds Charts
    output$viewLogodds <- renderGvis({
      # Reactive
      df <- dataUpload()
      df$var <- df[, paste0(input$product)]
      # Create sumarized table
      t1 <- data.frame()
      t1 <- ddply(df, .(var), summarise, event = sum(YY), count = length(YY))
      t1$mix <- t1$count / nrow(df)
      t1$totalevent <- sum(df$YY)
      t1$totalnonevent <- nrow(df)-sum(df$YY)
      t1$nonevent <- t1$count - t1$event
      t1$mix <- t1$count / nrow(df)
      t1$pctevent <- ifelse(t1$totalevent==0, 0, t1$event/t1$totalevent)
      t1$pctnonevent <- ifelse(t1$totalnonevent==0, 0,
                               t1$nonevent/t1$totalnonevent)
      t1$eventrate <- t1$event / (t1$nonevent+t1$event)
      t1$woe <- ifelse(t1$pctnonevent==0 | t1$pctevent==0, 0,
                       log(t1$pctevent/t1$pctnonevent))
      t1 <- within(t1, iv <- woe * (pctevent-pctnonevent))
      t1 <- subset(t1, select = c(var, count, mix, eventrate, woe, iv))
      t1 <- t1[with(t1, order(woe)),]
      #googleVis Combo chart
      gvisComboChart(t1, xvar="var", yvar=c("woe", "mix"),
                     options=list(curveType="function",
                                  pointSize=7,
                                  seriesType="bars",
                                  series="[{type:'line',
                                  targetAxisIndex:0,
                                  color:'black',
                                  lineWidth: 0.1,
                                  opacity: 0.1},
                                  {type:'bars',
                                  targetAxisIndex:1,
                                  color:'grey'}]",
                                  trendlines="{0: {type: 'polynomial',
                                  color: 'green',
                                  opacity: 0.3}}",
                                  vAxes="[{title:'WOE',
                                  format:'#,###.##',
                                  titleTextStyle: {color: 'black'},
                                  textStyle:{color: 'black'},
                                  textPosition: 'out'},
                                  {title:'% of Total',
                                  format:'#,###%',
                                  titleTextStyle: {color: 'grey'},
                                  textStyle:{color: 'grey'},
                                  textPosition: 'out',
                                  minValue:0}]",
                                  hAxes="[{title:'Cutoff Point',
                                  textPosition: 'out'}]",
                                  height=500
                     ),
                     chartid="twoaxiscombochart"
      )})
    # Interactive Binning WOE Table
    output$viewWoe <- renderGvis({
      # Reactive
      df <- dataUpload()
      df$var <- df[, paste0(input$product)]
      # Create sumarized table
      t1 <- data.frame()
      t1 <- ddply(df, .(var), summarise, event = sum(YY), count = length(YY))
      t1$mix <- t1$count / nrow(df)
      t1$totalevent <- sum(df$YY)
      t1$totalnonevent <- nrow(df)-sum(df$YY)
      t1$nonevent <- t1$count - t1$event
      t1$mix <- t1$count / nrow(df)
      t1$pctevent <- ifelse(t1$totalevent==0, 0, t1$event/t1$totalevent)
      t1$pctnonevent <- ifelse(t1$totalnonevent==0, 0,
                               t1$nonevent/t1$totalnonevent)
      t1$eventrate <- t1$event / (t1$nonevent+t1$event)
      t1$woe <- ifelse(t1$pctnonevent==0 | t1$pctevent==0, 0,
                       log(t1$pctevent/t1$pctnonevent))
      t1 <- within(t1, iv <- woe * (pctevent-pctnonevent))
      t1 <- subset(t1, select = c(var, count, mix, eventrate, woe, iv))
      t1 <- t1[with(t1, order(woe)),]
      t1 <- rename(t1, c("var" = "Category",
                         "count" = "Counts", "mix" = "Volume_Mix",
                         "eventrate" = "Event_Pct",
                         "woe" = "WOE", "iv" = "IV"))
      t <- sqldf("select 'Total' as Category,
                 sum(Counts) as Counts,
                 sum(Volume_Mix) as Volume_Mix,
                 sum(Event_Pct*Volume_Mix) as Event_Pct,
                 NULL as WOE,
                 sum(IV) as IV from t1")
      t1 <- rbind(t1, t)
      gvisTable(t1,
                formats = list('Volume_Mix'='#.#%','Event_Pct'='#.##%',
                               'Counts'='#,###',
                               'WOE'='#.####', 'IV'='#.####'),
                options = list(width=600))
    })
    # Print R code
    output$code <- renderPrint({
      # Reactive
      df <- dataUpload()
      df$var <- df[, paste0(input$product)]
      # Create sumarized table
      t1 <- data.frame()
      t1 <- ddply(df, .(var), summarise, event = sum(YY), count = length(YY))
      t1$mix <- t1$count / nrow(df)
      t1$totalevent <- sum(df$YY)
      t1$totalnonevent <- nrow(df)-sum(df$YY)
      t1$nonevent <- t1$count - t1$event
      t1$mix <- t1$count / nrow(df)
      t1$pctevent <- ifelse(t1$totalevent==0, 0, t1$event/t1$totalevent)
      t1$pctnonevent <- ifelse(t1$totalnonevent==0, 0,
                               t1$nonevent/t1$totalnonevent)
      t1$eventrate <- t1$event / (t1$nonevent+t1$event)
      t1$woe <- ifelse(t1$pctnonevent==0 | t1$pctevent==0, 0,
                       log(t1$pctevent/t1$pctnonevent))
      t1 <- within(t1, iv <- woe * (pctevent-pctnonevent))
      t1 <- subset(t1, select = c(var, count, mix, eventrate, woe, iv))
      t1 <- t1[with(t1, order(woe)),]
      lines=nrow(t1)
      sqlcodetable=as.list(matrix(ncol=0,nrow=0))
      sqlcodetable=rbind("select case")
      for (k in 1:lines){
        sqlcodetable=rbind(sqlcodetable,paste0("when ",
                                               input$product,
                                               "='",
                                               t1[k,1],
                                               "' then ",
                                               t1[k,5]))
      }
      sqlcodetable=rbind(sqlcodetable,paste0(" end as ", input$product, "_B"))
      return(gsub(",","",toString(sqlcodetable)))
    })
    # Interactive Binning Chart
    output$viewLogodds2 <- renderGvis({
      # Reactive
      df <- dataUpload2()
      df$var <- df[, paste0(input$product2)]
      selBin <- as.numeric(input$selBin)
      # Create sumarized table
      df$bin <- round(Rank(df$var) *groups)
      t1 <- data.frame()
      t1 <- ddply(df, .(bin), summarise, event = sum(YY), count = length(YY))
      t2 <- ddply(df, .(bin), summarise, minVal = min(var), maxVal = max(var))
      t2 <- subset(t2, select = c(minVal,maxVal))
      t1 <- cbind(t1, t2)
      t1$bin <- ifelse(is.na(t1$bin), -1, t1$bin)
      t1$mix <- t1$count / nrow(df)
      t1$totalevent <- sum(df$YY)
      t1$totalnonevent <- nrow(df)-sum(df$YY)
      t1$nonevent <- t1$count - t1$event
      t1 <- t1[with(t1, order(bin)),]

      t1$sel <- ifelse(t1$bin %in% selBin, 1, 0)
      t1$sel2 <- with(t1, cumsum(sel))
      setDT(t1)
      setkey(t1, sel2)
      t1 <- t1[with(t1, order(sel2, sel)),]
      t1[, event := cumsum(event), by=sel2]
      t1[, nonevent := cumsum(nonevent), by=sel2]
      t1[, count := cumsum(count), by=sel2]
      t1$mix <- t1$count / nrow(df)
      t1$pctevent <- ifelse(t1$totalevent==0, 0, t1$event/t1$totalevent)
      t1$pctnonevent <- ifelse(t1$totalnonevent==0, 0,
                               t1$nonevent/t1$totalnonevent)
      t1$eventrate <- t1$event / (t1$nonevent+t1$event)
      t1$woe <- ifelse(t1$pctnonevent==0 | t1$pctevent==0, 0,
                       log(t1$pctevent/t1$pctnonevent))
      t1 <- within(t1, iv <- woe * (pctevent-pctnonevent))
      t1 <- within(t1, cutpoint <- ifelse(bin == -1, "Missing",
                                          paste0("<= ", maxVal)))
      t1 <- subset(t1, sel == 1)
      t1 <- subset(t1, select = c(bin, minVal, cutpoint, count, mix, eventrate,
                                  woe, iv))
      t1 <- t1[with(t1, order(bin)),]
      #googleVis Combo chart
      gvisComboChart(t1, xvar="cutpoint", yvar=c("woe", "mix"),
                     options=list(curveType="function",
                                  pointSize=7,
                                  seriesType="bars",
                                  series="[{type:'line',
                                  targetAxisIndex:0,
                                  color:'black',
                                  lineWidth: 0.1,
                                  opacity: 0.1},
                                  {type:'bars',
                                  targetAxisIndex:1,
                                  color:'grey'}]",
                                  trendlines="{0: {type: 'polynomial',
                                  color: 'green',
                                  opacity: 0.3}}",
                                  vAxes="[{title:'WOE',
                                  format:'#,###.##',
                                  titleTextStyle: {color: 'black'},
                                  textStyle:{color: 'black'},
                                  textPosition: 'out'},
                                  {title:'% of Total',
                                  format:'#.#%',
                                  titleTextStyle: {color: 'grey'},
                                  textStyle:{color: 'grey'},
                                  textPosition: 'out',
                                  minValue:0}]",
                                  hAxes="[{title:'Cutoff Point',
                                  textPosition: 'out'}]",
                                  height=500
                     ),
                     chartid="twoaxiscombochart"
      )})
    # Interactive Binning WOE Table
    output$viewWoe2 <- renderGvis({
      # Reactive
      df <- dataUpload2()
      df$var <- df[, paste0(input$product2)]
      selBin <- as.numeric(input$selBin)
      # Create sumarized table
      df$bin <- round(Rank(df$var) *groups)
      t1 <- data.frame()
      t1 <- ddply(df, .(bin), summarise, event = sum(YY), count = length(YY))
      t2 <- ddply(df, .(bin), summarise, minVal = min(var), maxVal = max(var))
      t2 <- subset(t2, select = c(minVal,maxVal))
      t1 <- cbind(t1, t2)
      t1$bin <- ifelse(is.na(t1$bin), -1, t1$bin)
      t1$mix <- t1$count / nrow(df)
      t1$totalevent <- sum(df$YY)
      t1$totalnonevent <- nrow(df)-sum(df$YY)
      t1$nonevent <- t1$count - t1$event
      t1 <- t1[with(t1, order(bin)),]

      t1$sel <- ifelse(t1$bin %in% selBin, 1, 0)
      t1$sel2 <- with(t1, cumsum(sel))
      setDT(t1)
      setkey(t1, sel2)
      t1 <- t1[with(t1, order(sel2, sel)),]
      t1[, event := cumsum(event), by=sel2]
      t1[, nonevent := cumsum(nonevent), by=sel2]
      t1[, count := cumsum(count), by=sel2]
      t1$mix <- t1$count / nrow(df)
      t1$pctevent <- ifelse(t1$totalevent==0, 0, t1$event/t1$totalevent)
      t1$pctnonevent <- ifelse(t1$totalnonevent==0, 0, t1$nonevent/t1$totalnonevent)
      t1$woe <- ifelse(t1$pctnonevent==0 | t1$pctevent==0, 0,
                       log(t1$pctevent/t1$pctnonevent))
      t1$eventrate <- t1$event / (t1$nonevent+t1$event)
      t1 <- within(t1, iv <- woe * (pctevent-pctnonevent))
      t1 <- within(t1, cutpoint <- ifelse(bin == -1, "Missing",
                                          paste0("<= ", maxVal)))
      t1 <- subset(t1, sel == 1)
      t1 <- subset(t1, select = c(bin, minVal, cutpoint, count, mix, eventrate,
                                  woe, iv))
      t1 <- t1[with(t1, order(bin)),]
      t1 <- rename(t1, c("bin" = "Bin", "minVal" = "Min_Value",
                         "cutpoint" = "Cutoff_Point",
                         "count" = "Counts", "mix" = "Volume_Mix",
                         "eventrate" = "Event_Pct",
                         "woe" = "WOE", "iv" = "IV"))
      t <- sqldf("select NULL as Bin,
                 NULL as Min_Value,
                 'Total' as Cutoff_Point,
                 sum(Counts) as Counts,
                 sum(Volume_Mix) as Volume_Mix,
                 sum(Event_Pct*Volume_Mix) as Event_Pct,
                 NULL as WOE,
                 sum(IV) as IV from t1")
      t1 <- rbind(t1, t)
      gvisTable(t1,
                formats = list('Volume_Mix'='#.#%','Event_Pct'='#.##%',
                               'Counts'='#,###', 'Min_Value'='#,###',
                               'WOE'='#.####', 'IV'='#.####'),
                options = list(width=600))
    })
    # Print R code
    output$code2 <- renderPrint({
      # Reactive
      df <- dataUpload2()
      df$var <- df[, paste0(input$product2)]
      selBin <- as.numeric(input$selBin)
      # Create sumarized table
      df$bin <- round(Rank(df$var) *groups)
      t1 <- data.frame()
      t1 <- ddply(df, .(bin), summarise, event = sum(YY), count = length(YY))
      t2 <- ddply(df, .(bin), summarise, minVal = min(var), maxVal = max(var))
      t2 <- subset(t2, select = c(minVal,maxVal))
      t1 <- cbind(t1, t2)
      t1$bin <- ifelse(is.na(t1$bin), -1, t1$bin)
      t1$mix <- t1$count / nrow(df)
      t1$totalevent <- sum(df$YY)
      t1$totalnonevent <- nrow(df)-sum(df$YY)
      t1$nonevent <- t1$count - t1$event
      t1 <- t1[with(t1, order(bin)),]

      t1$sel <- ifelse(t1$bin %in% selBin, 1, 0)
      t1$sel2 <- with(t1, cumsum(sel))
      setDT(t1)
      setkey(t1, sel2)
      t1 <- t1[with(t1, order(sel2, sel)),]
      t1[, event := cumsum(event), by=sel2]
      t1[, nonevent := cumsum(nonevent), by=sel2]
      t1[, count := cumsum(count), by=sel2]
      t1$mix <- t1$count / nrow(df)
      t1$pctevent <- ifelse(t1$totalevent==0, 0, t1$event/t1$totalevent)
      t1$pctnonevent <- ifelse(t1$totalnonevent==0, 0,
                               t1$nonevent/t1$totalnonevent)
      t1$woe <- ifelse(t1$pctnonevent==0 | t1$pctevent==0, 0,
                       log(t1$pctevent/t1$pctnonevent))
      t1 <- within(t1, iv <- woe * (pctevent-pctnonevent))
      t1 <- within(t1, cutpoint <- ifelse(bin == -1, " is Null",
                                          paste0("<= ", maxVal)))
      t1 <- subset(t1, sel == 1)
      t1 <- subset(t1, select = c(bin, minVal, cutpoint, count, mix, pctevent,
                                  woe, iv))
      t1 <- t1[with(t1, order(bin)),]
      t1 <- as.data.frame(t1)

      lines=nrow(t1)
      sqlcodetable=as.list(matrix(ncol=0,nrow=0))
      sqlcodetable=rbind("select case")
      for (k in 1:lines){
        sqlcodetable=rbind(sqlcodetable,paste0("when ",
                                               input$product2,
                                               t1[k,3],
                                               " ",
                                               "then ",
                                               t1[k,7]))
      }
      sqlcodetable=rbind(sqlcodetable,paste0(" end as ", input$product2, "_B"))
      return(gsub(",","",toString(sqlcodetable)))
    })
    # Histogram
    output$main_plot <- renderPlot({
      # Reactive
      df <- dataUpload3()
      df$var <- df[, paste0(input$product3)]
      df <- subset(df, select = c(var))
      VarType <- class(df$var)
      if (VarType %in% c('integer', 'numeric')) {
        # Create Histogram
        hist(df$var,
             probability = TRUE,
             breaks = as.numeric(input$n_breaks),
             main = "Numeric Variable Distribution")

        if (input$individual_obs) {
          rug(df$var)
        }

        if (input$density) {
          dens <- density(df$var,
                          adjust = input$bw_adjust)
          lines(dens, col = "blue")
        }
      }
      else barplot(prop.table(table(df$var)))
    })
    # Download Data
    datasetInput <- reactive({
      switch(input$dataset,
             "Variable Summary" = Vars,
             "Univariate Analysis" = Univars,
             "Chi-Square" = Chisq,
             "Stats Analysis" = Stats,
             "Variable Clustering" = Varclus)
    })

    output$table <- renderTable({
      datasetInput()
    })

    output$downloadData <- downloadHandler(
      filename = function() { paste(input$dataset, '.csv', sep='') },
      content = function(file) {
        write.csv(datasetInput(), file)
      }
    )
                                  }

  ui <- dashboardPage(
    dashboardHeader(title = "Modeling Tools"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Stats Explore", tabName = "dashboard1",
                 icon = icon("table")),
        menuItem("Variable Clustering", tabName = "dashboard8",
                 icon = icon("table")),
        menuItem("Log-Odds Charts (Numeric)", tabName = "dashboard9",
                 icon = icon("line-chart")),
        menuItem("Log-Odds Charts (Categorical)", tabName = "dashboard2",
                 icon = icon("bar-chart")),
        menuItem("Export", tabName = "dashboard10",
                 icon = icon("download"))
      )
    ),
    dashboardBody(
      tabItems(
        # First tab content
        tabItem(tabName = "dashboard1",
                fluidRow(tabBox(
                  width = 10,
                  tabPanel("Variable Summary", "", htmlOutput("viewVars"),
                           p(""),
                           p("Column Legend: "),
                           p(class = "text-muted",
                             "N = Number of observations"),
                           p(class = "text-muted",
                             "NValues = Distinct values counts"),
                           p(class = "text-muted",
                             "MaxPct = Percent of value with highest frequncy"),
                           p(class = "text-muted",
                             "NMiss = Missing counts"),
                           p(class = "text-muted",
                             "NMissPct = Missing percent of total"),
                           p(class = "text-muted",
                             "NZero = Zero counts"),
                           p(class = "text-muted",
                             "NZeroPct = Zero percent of total"),
                           p(class = "text-muted",
                             "NSV = Number of Special Value"),
                           p(class = "text-muted",
                             "NOutlier = Number of Outliers (over 5 stdev)")),
                  tabPanel("Univariate Summary", "",
                           htmlOutput("viewUnivars")),
                  tabPanel("Chi-Square Summary", "",
                           htmlOutput("viewChisq")),
                  tabPanel("Stats Summary (Binned)", "",
                           htmlOutput("viewStats"))
                ))),
        tabItem(tabName = "dashboard8",
                fluidRow(box(width = 10, htmlOutput("viewVarclus")))),
        tabItem(tabName = "dashboard9",
                fluidRow(box(width = 3,
                             selectInput("product2", "Select Variable: ",
                                         names(subset(df, select = c(SelVar))))),
                         box(width = 7,
                             selectInput('selBin', 'Remove Bins: ',
                                         seq(-1, groups),
                                         selected = seq(-1, groups),
                                         multiple = TRUE, selectize = TRUE))),
                fluidRow(box(width = 5,
                             title = "Log-Odds Chart",
                             status = "primary",
                             solidHeader = TRUE,
                             htmlOutput("viewLogodds2")),
                         box(width = 5,
                             title = "WOE Summary",
                             status = "primary",
                             solidHeader = TRUE,
                             htmlOutput("viewWoe2"))),
                fluidRow(box(width = 10,
                             title = "WOE Transformation SQL",
                             status = "primary",
                             solidHeader = TRUE,
                             verbatimTextOutput('code2')))
        ),
        tabItem(tabName = "dashboard2",
                fluidRow(box(width = 3,
                             selectInput("product",
                                         "Select Variable: ",
                                         names(subset(df,
                                                      select = c(SelVarC)))))),
                fluidRow(box(width = 5,
                             title = "Log-Odds Chart",
                             status = "primary",
                             solidHeader = TRUE,
                             htmlOutput("viewLogodds")),
                         box(width = 5,
                             title = "WOE Summary",
                             status = "primary",
                             solidHeader = TRUE,
                             htmlOutput("viewWoe"))),
                fluidRow(box(width = 10,
                             title = "WOE Transformation SQL",
                             status = "primary",
                             solidHeader = TRUE,
                             verbatimTextOutput('code')))
        ),
        tabItem(tabName = "dashboard10",
                pageWithSidebar(
                  headerPanel('Export to CSV'),
                  sidebarPanel(
                    selectInput("dataset", "Choose a dataset:",
                                choices = c("Variable Summary",
                                            "Univariate Analysis",
                                            "Chi-Square",
                                            "Stats Analysis",
                                            "Variable Clustering")),
                    downloadButton('downloadData', 'Download')
                  ),
                  mainPanel(
                    tableOutput('table')
                  )
                ))
      ),
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
      )
    )
  )
  shinyApp(ui = ui, server = server)
  }
