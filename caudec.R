


caudec <-  function(Y,W,G1,G2,Q,X,data,alpha=0.05,weight=NULL,k=500,t=0.05,algorithm) {
  
  if (is.null(weight)) {
    data$weight=rep(1, nrow(data))
    weight <- "weight"
  }
  
  core <- function(data) {
    
    # estimate the nuisance functions within each group, so that the final estimates are independent across groups
    G1_index <- data[,G1]==1
    G2_index <- data[,G2]==1
    
    mainsample_G1 <- sample(nrow(data[G1_index,]), floor(nrow(data[G1_index,])/2), replace=F)
    auxsample_G1 <- setdiff(1:nrow(data[G1_index,]), mainsample_G1)
    mainsample_G2 <- sample(nrow(data[G2_index,]), floor(nrow(data[G2_index,])/2), replace=F)
    auxsample_G2 <- setdiff(1:nrow(data[G2_index,]), mainsample_G2)
    
    YgivenX.Pred_W0 <- YgivenX.Pred_W1 <- WgivenX.Pred <- rep(NA, nrow(data))
    
    if (algorithm=="nnet") {
      message <- capture.output( YgivenX.Model.Aux_G1 <- train(as.formula(paste(Y, paste(W,paste(X,collapse="+"),sep="+"), sep="~")), data=data[G1_index,][mainsample_G1,], method="nnet", 
                                                               preProc=c("center","scale"), trControl=trainControl(method="none"), linout=TRUE, 
                                                               tuneGrid=expand.grid(size=2,decay=0.02)) )
      message <- capture.output( YgivenX.Model.Main_G1 <- train(as.formula(paste(Y, paste(W,paste(X,collapse="+"),sep="+"), sep="~")), data=data[G1_index,][auxsample_G1,], method="nnet",
                                                                preProc=c("center","scale"), trControl=trainControl(method="none"), linout=TRUE, 
                                                                tuneGrid=expand.grid(size=2,decay=0.02)) )
      message <- capture.output( YgivenX.Model.Aux_G2 <- train(as.formula(paste(Y, paste(W,paste(X,collapse="+"),sep="+"), sep="~")), data=data[G2_index,][mainsample_G2,], method="nnet", 
                                                               preProc=c("center","scale"), trControl=trainControl(method="none"), linout=TRUE, 
                                                               tuneGrid=expand.grid(size=2,decay=0.02)) )
      message <- capture.output( YgivenX.Model.Main_G2 <- train(as.formula(paste(Y, paste(W,paste(X,collapse="+"),sep="+"), sep="~")), data=data[G2_index,][auxsample_G2,], method="nnet",
                                                                preProc=c("center","scale"), trControl=trainControl(method="none"), linout=TRUE, 
                                                                tuneGrid=expand.grid(size=2,decay=0.02)) )
    }
    if (algorithm=="ranger") {
      message <- capture.output( YgivenX.Model.Aux_G1 <- train(as.formula(paste(Y, paste(W,paste(X,collapse="+"),sep="+"), sep="~")), data=data[G1_index,][mainsample_G1,], method="ranger", 
                                                               trControl=trainControl(method="cv"),
                                                               tuneGrid=expand.grid(mtry=floor(sqrt(length(X))),splitrule="variance",min.node.size=c(5,10,100))) )
      message <- capture.output( YgivenX.Model.Main_G1 <- train(as.formula(paste(Y, paste(W,paste(X,collapse="+"),sep="+"), sep="~")), data=data[G1_index,][auxsample_G1,], method="ranger",
                                                                trControl=trainControl(method="cv"),
                                                                tuneGrid=expand.grid(mtry=floor(sqrt(length(X))),splitrule="variance",min.node.size=c(5,10,100))) )
      message <- capture.output( YgivenX.Model.Aux_G2 <- train(as.formula(paste(Y, paste(W,paste(X,collapse="+"),sep="+"), sep="~")), data=data[G2_index,][mainsample_G2,], method="ranger", 
                                                               trControl=trainControl(method="cv"),
                                                               tuneGrid=expand.grid(mtry=floor(sqrt(length(X))),splitrule="variance",min.node.size=c(5,10,100))) )
      message <- capture.output( YgivenX.Model.Main_G2 <- train(as.formula(paste(Y, paste(W,paste(X,collapse="+"),sep="+"), sep="~")), data=data[G2_index,][auxsample_G2,], method="ranger",
                                                                trControl=trainControl(method="cv"),
                                                                tuneGrid=expand.grid(mtry=floor(sqrt(length(X))),splitrule="variance",min.node.size=c(5,10,100))) )
    }
    
    
    pred_data <- data
    pred_data[,colnames(pred_data)%in%W] <- 0
    YgivenX.Pred_W0[G1_index][mainsample_G1] <- predict(YgivenX.Model.Aux_G1, newdata = pred_data[G1_index,][mainsample_G1,])
    YgivenX.Pred_W0[G1_index][auxsample_G1] <- predict(YgivenX.Model.Main_G1, newdata = pred_data[G1_index,][auxsample_G1,])
    YgivenX.Pred_W0[G2_index][mainsample_G2] <- predict(YgivenX.Model.Aux_G2, newdata = pred_data[G2_index,][mainsample_G2,])
    YgivenX.Pred_W0[G2_index][auxsample_G2] <- predict(YgivenX.Model.Main_G2, newdata = pred_data[G2_index,][auxsample_G2,])
    
    pred_data <- data
    pred_data[,colnames(pred_data)%in%W] <- 1
    YgivenX.Pred_W1[G1_index][mainsample_G1] <- predict(YgivenX.Model.Aux_G1, newdata = pred_data[G1_index,][mainsample_G1,])
    YgivenX.Pred_W1[G1_index][auxsample_G1] <- predict(YgivenX.Model.Main_G1, newdata = pred_data[G1_index,][auxsample_G1,])
    YgivenX.Pred_W1[G2_index][mainsample_G2] <- predict(YgivenX.Model.Aux_G2, newdata = pred_data[G2_index,][mainsample_G2,])
    YgivenX.Pred_W1[G2_index][auxsample_G2] <- predict(YgivenX.Model.Main_G2, newdata = pred_data[G2_index,][auxsample_G2,])
    
    
    data[,W] <- as.factor(data[,W])
    levels(data[,W]) <- c("W0","W1")  # necessary for caret implementation of ranger
    
    if (algorithm=="nnet") {
      message <- capture.output( WgivenX.Model.Aux_G1 <- train(as.formula(paste(W, paste(X,collapse="+"), sep="~")), data=data[G1_index,][auxsample_G1,], method="nnet", 
                                                               preProc=c("center","scale"), trControl=trainControl(method="none"), linout=FALSE, 
                                                               tuneGrid=expand.grid(size=2,decay=0.02)) )
      message <- capture.output( WgivenX.Model.Main_G1 <- train(as.formula(paste(W, paste(X,collapse="+"), sep="~")), data=data[G1_index,][mainsample_G1,], method="nnet",
                                                                preProc=c("center","scale"), trControl=trainControl(method="none"), linout=FALSE,
                                                                tuneGrid=expand.grid(size=2,decay=0.02)) )
      message <- capture.output( WgivenX.Model.Aux_G2 <- train(as.formula(paste(W, paste(X,collapse="+"), sep="~")), data=data[G2_index,][auxsample_G2,], method="nnet", 
                                                               preProc=c("center","scale"), trControl=trainControl(method="none"), linout=FALSE, 
                                                               tuneGrid=expand.grid(size=2,decay=0.02)) )
      message <- capture.output( WgivenX.Model.Main_G2 <- train(as.formula(paste(W, paste(X,collapse="+"), sep="~")), data=data[G2_index,][mainsample_G2,], method="nnet",
                                                                preProc=c("center","scale"), trControl=trainControl(method="none"), linout=FALSE,
                                                                tuneGrid=expand.grid(size=2,decay=0.02)) )
    }
    if (algorithm=="ranger") {
      message <- capture.output( WgivenX.Model.Aux_G1 <- train(as.formula(paste(W, paste(X,collapse="+"), sep="~")), data=data[G1_index,][auxsample_G1,], method="ranger", 
                                                               trControl=trainControl(method="cv", classProbs=TRUE),
                                                               tuneGrid=expand.grid(mtry=floor(sqrt(length(X))),splitrule="gini",min.node.size=c(1,10,100))) )
      message <- capture.output( WgivenX.Model.Main_G1 <- train(as.formula(paste(W, paste(X,collapse="+"), sep="~")), data=data[G1_index,][mainsample_G1,], method="ranger",
                                                                trControl=trainControl(method="cv", classProbs=TRUE),
                                                                tuneGrid=expand.grid(mtry=floor(sqrt(length(X))),splitrule="gini",min.node.size=c(1,10,100))) )
      message <- capture.output( WgivenX.Model.Aux_G2 <- train(as.formula(paste(W, paste(X,collapse="+"), sep="~")), data=data[G2_index,][auxsample_G2,], method="ranger", 
                                                               trControl=trainControl(method="cv", classProbs=TRUE),
                                                               tuneGrid=expand.grid(mtry=floor(sqrt(length(X))),splitrule="gini",min.node.size=c(1,10,100))) )
      message <- capture.output( WgivenX.Model.Main_G2 <- train(as.formula(paste(W, paste(X,collapse="+"), sep="~")), data=data[G2_index,][mainsample_G2,], method="ranger",
                                                                trControl=trainControl(method="cv", classProbs=TRUE),
                                                                tuneGrid=expand.grid(mtry=floor(sqrt(length(X))),splitrule="gini",min.node.size=c(1,10,100))) )
    }
    
    WgivenX.Pred[G1_index][mainsample_G1] <- predict(WgivenX.Model.Aux_G1, newdata=data[G1_index,][mainsample_G1,], type="prob")[,2]
    WgivenX.Pred[G1_index][auxsample_G1] <- predict(WgivenX.Model.Main_G1, newdata=data[G1_index,][auxsample_G1,], type="prob")[,2]
    WgivenX.Pred[G2_index][mainsample_G2] <- predict(WgivenX.Model.Aux_G2, newdata=data[G2_index,][mainsample_G2,], type="prob")[,2]
    WgivenX.Pred[G2_index][auxsample_G2] <- predict(WgivenX.Model.Main_G2, newdata=data[G2_index,][auxsample_G2,], type="prob")[,2]
    
    data[,W] <- as.numeric(data[,W])-1
    
    WgivenX.Pred[WgivenX.Pred<=t] <- t
    WgivenX.Pred[WgivenX.Pred>=1-t] <- 1-t
    
    Y0_i <- ATT_i <- ATE_i <- wht <- rep(NA, nrow(data))
    
    wht[G1_index] <- data[,weight][G1_index]/mean(data[,weight][G1_index])
    wht[G2_index] <- data[,weight][G2_index]/mean(data[,weight][G2_index])
    
    Y0_i[G1_index] <- ( YgivenX.Pred_W0 + (1-data[,W])*(data[,Y]-YgivenX.Pred_W0)/(1-WgivenX.Pred) )[G1_index]*wht[G1_index]
    Y0_i[G2_index] <- ( YgivenX.Pred_W0 + (1-data[,W])*(data[,Y]-YgivenX.Pred_W0)/(1-WgivenX.Pred) )[G2_index]*wht[G2_index]
    
    ATT_i[G1_index] <- ( (data[,W]-(1-data[,W])*WgivenX.Pred/(1-WgivenX.Pred))*(data[,Y]-YgivenX.Pred_W0) )[G1_index]/mean(data[,W][G1_index]*wht[G1_index])*wht[G1_index]
    ATT_i[G2_index] <- ( (data[,W]-(1-data[,W])*WgivenX.Pred/(1-WgivenX.Pred))*(data[,Y]-YgivenX.Pred_W0) )[G2_index]/mean(data[,W][G2_index]*wht[G2_index])*wht[G2_index]
    
    ATE_i[G1_index] <- ( YgivenX.Pred_W1 + data[,W]*(data[,Y]-YgivenX.Pred_W1)/WgivenX.Pred - ( YgivenX.Pred_W0 + (1-data[,W])*(data[,Y]-YgivenX.Pred_W0)/(1-WgivenX.Pred) ) )[G1_index]*wht[G1_index]
    ATE_i[G2_index] <- ( YgivenX.Pred_W1 + data[,W]*(data[,Y]-YgivenX.Pred_W1)/WgivenX.Pred - ( YgivenX.Pred_W0 + (1-data[,W])*(data[,Y]-YgivenX.Pred_W0)/(1-WgivenX.Pred) ) )[G2_index]*wht[G2_index]
    
    total <- mean(data[,Y][G1_index]*wht[G1_index])-mean(data[,Y][G2_index]*wht[G2_index])
    baseline <- mean(Y0_i[G1_index])-mean(Y0_i[G2_index])
    prevalence <- mean(ATE_i[G2_index])*(mean(data[,W][G1_index]*wht[G1_index])-mean(data[,W][G2_index]*wht[G2_index]))
    effect <- mean(data[,W][G1_index]*wht[G1_index])*(mean(ATE_i[G1_index])-mean(ATE_i[G2_index]))
    selection <- (mean(ATT_i[G1_index])-mean(ATE_i[G1_index]))*mean(data[,W][G1_index]*wht[G1_index])-
      (mean(ATT_i[G2_index])-mean(ATE_i[G2_index]))*mean(data[,W][G2_index]*wht[G2_index])
    
    ### conditional prevalence ###
    data_cond <- cbind(ATE_i, data[,W], data[,Q])
    data_cond <- as.data.frame(data_cond)
    colnames(data_cond)[1:2] <- c("tau","W")
    Q_names <- colnames(data_cond)[3:ncol(data_cond)]
    data_cond$W <- as.factor(data_cond$W)
    levels(data_cond$W) <- c("W0","W1")  # necessary for caret implementation of ranger
    
    TaugivenQ.Pred_G1_G1 <- TaugivenQ.Pred_G2_G1 <- WgivenQ.Pred_G1_G1 <- WgivenQ.Pred_G2_G1 <- rep(NA, sum(G1_index))
    TaugivenQ.Pred_G1_G2 <- TaugivenQ.Pred_G2_G2 <- WgivenQ.Pred_G1_G2 <- WgivenQ.Pred_G2_G2 <- rep(NA, sum(G2_index))
    
    if (algorithm=="nnet") {
      message <- capture.output( TaugivenQ.Model_G1 <- train(as.formula(paste("tau", paste(Q_names,sep="+"), sep="~")), data=data_cond[G1_index,], method="nnet", 
                                                             preProc=c("center","scale"), trControl=trainControl(method="none"), linout=TRUE, 
                                                             tuneGrid=expand.grid(size=2,decay=0.02)) )
      
      message <- capture.output( TaugivenQ.Model_G2 <- train(as.formula(paste("tau", paste(Q_names,sep="+"), sep="~")), data=data_cond[G2_index,], method="nnet", 
                                                             preProc=c("center","scale"), trControl=trainControl(method="none"), linout=TRUE, 
                                                             tuneGrid=expand.grid(size=2,decay=0.02)) )
    }
    if (algorithm=="ranger") {
      message <- capture.output( TaugivenQ.Model_G1 <- train(as.formula(paste("tau", paste(Q_names,sep="+"), sep="~")), data=data_cond[G1_index,], method="ranger", 
                                                             trControl=trainControl(method="cv"),
                                                             tuneGrid=expand.grid(mtry=floor(sqrt(length(Q_names))),splitrule="variance",min.node.size=c(5,10,100))) )
      
      message <- capture.output( TaugivenQ.Model_G2 <- train(as.formula(paste("tau", paste(Q_names,sep="+"), sep="~")), data=data_cond[G2_index,], method="ranger", 
                                                             trControl=trainControl(method="cv"),
                                                             tuneGrid=expand.grid(mtry=floor(sqrt(length(Q_names))),splitrule="variance",min.node.size=c(5,10,100))) )
    }
    
    
    TaugivenQ.Pred_G1_G1 <- predict(TaugivenQ.Model_G1, newdata = data_cond[G1_index,])
    TaugivenQ.Pred_G2_G1 <- predict(TaugivenQ.Model_G2, newdata = data_cond[G1_index,])
    TaugivenQ.Pred_G1_G2 <- predict(TaugivenQ.Model_G1, newdata = data_cond[G2_index,])
    TaugivenQ.Pred_G2_G2 <- predict(TaugivenQ.Model_G2, newdata = data_cond[G2_index,])
    
    #plotLowess(data_cond[G2_index,]$tau ~ data_cond[G2_index,]$V3)
    #plot(data_cond[G2_index,]$V3, TaugivenQ.Pred_G2_G2)
    
    if (algorithm=="nnet") {  
      message <- capture.output( WaugivenQ.Model_G1 <- train(as.formula(paste("W", paste(Q_names,sep="+"), sep="~")), data=data_cond[G1_index,], method="nnet", 
                                                             preProc=c("center","scale"), trControl=trainControl(method="none"), linout=FALSE, 
                                                             tuneGrid=expand.grid(size=2,decay=0.02)), weights=wht[G1_index] )
      
      message <- capture.output( WaugivenQ.Model_G2 <- train(as.formula(paste("W", paste(Q_names,sep="+"), sep="~")), data=data_cond[G2_index,], method="nnet", 
                                                             preProc=c("center","scale"), trControl=trainControl(method="none"), linout=FALSE, 
                                                             tuneGrid=expand.grid(size=2,decay=0.02)), weights=wht[G2_index] )
    }
    if (algorithm=="ranger") {
      message <- capture.output( WaugivenQ.Model_G1 <- train(as.formula(paste("W", paste(Q_names,sep="+"), sep="~")), data=data_cond[G1_index,], method="ranger", 
                                                             trControl=trainControl(method="cv", classProbs=TRUE),  
                                                             tuneGrid=expand.grid(mtry=floor(sqrt(length(Q_names))),splitrule="gini",min.node.size=c(1,10,100))), weights=wht[G1_index] )
      
      message <- capture.output( WaugivenQ.Model_G2 <- train(as.formula(paste("W", paste(Q_names,sep="+"), sep="~")), data=data_cond[G2_index,], method="ranger", 
                                                             trControl=trainControl(method="cv", classProbs=TRUE),
                                                             tuneGrid=expand.grid(mtry=floor(sqrt(length(Q_names))),splitrule="gini",min.node.size=c(1,10,100))), weights=wht[G2_index] )
    }
    
    
    WgivenQ.Pred_G1_G1 <- predict(WaugivenQ.Model_G1, newdata = data_cond[G1_index,], type="prob")[,2]
    WgivenQ.Pred_G2_G1 <- predict(WaugivenQ.Model_G2, newdata = data_cond[G1_index,], type="prob")[,2]
    WgivenQ.Pred_G1_G2 <- predict(WaugivenQ.Model_G1, newdata = data_cond[G2_index,], type="prob")[,2]
    WgivenQ.Pred_G2_G2 <- predict(WaugivenQ.Model_G2, newdata = data_cond[G2_index,], type="prob")[,2]
    
    cond_prevalence <- mean((WgivenQ.Pred_G1_G2-WgivenQ.Pred_G2_G2)*TaugivenQ.Pred_G2_G2*wht[G2_index])
    cond_effect <- mean((TaugivenQ.Pred_G1_G1-TaugivenQ.Pred_G2_G1)*WgivenQ.Pred_G1_G1*wht[G1_index])
    Q_dist <- mean(WgivenQ.Pred_G1_G1*TaugivenQ.Pred_G2_G1*wht[G1_index]) - mean(WgivenQ.Pred_G1_G2*TaugivenQ.Pred_G2_G2*wht[G2_index])
    cond_selection <- total-baseline-cond_prevalence-cond_effect-Q_dist
    ###  ###
    
    output <- c(
      mean(data[,Y][G1_index]*wht[G1_index]),
      mean(data[,Y][G2_index]*wht[G2_index]),
      mean(Y0_i[G1_index]),
      mean(Y0_i[G2_index]),
      mean(data[,W][G1_index]*wht[G1_index]),
      mean(data[,W][G2_index]*wht[G2_index]),
      mean(ATE_i[G1_index]),
      mean(ATE_i[G2_index]),
      mean(ATT_i[G1_index]),
      mean(ATT_i[G2_index]),
      total,
      baseline,
      mean(data[,W][G1_index]*wht[G1_index])-mean(data[,W][G2_index]*wht[G2_index]),
      mean(ATE_i[G1_index])-mean(ATE_i[G2_index]),
      mean(ATT_i[G1_index])-mean(ATT_i[G2_index]),
      total,
      baseline,
      prevalence,
      effect,
      selection,
      cond_prevalence,
      cond_effect,
      cond_selection,
      Q_dist
    )
    
    return(output)
  }
  
  output_point <- core(data=data)
  
  data_boot <- replicate(k, data[sample(1:nrow(data), nrow(data), replace=TRUE),], simplify=FALSE)
  output_boot <- lapply(data_boot, function (x) core(data=x))
  output_boot <- as.data.frame(output_boot)
  
  output_se <- sqrt( (1/k)*rowSums(  (output_boot-rowMeans(output_boot))^2  ) )
  
  return(list(output_point, output_se))
  
}

