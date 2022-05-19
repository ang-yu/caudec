


caudec <-  function(Y,W,G1,G2,Q,X,data,alpha=0.05,weight=NULL,k=500,t=0.05) {
  
  if (is.null(weight)) {
    data$weight=rep(1, nrow(data))
    weight <- "weight"
  }
  
  # estimate the nuisance functions within each group, so that the final estimates are independent across groups
  G1_index <- data[,G1]==1
  G2_index <- data[,G2]==1

  mainsample_G1 <- sample(nrow(data[G1_index,]), floor(nrow(data[G1_index,])/2), replace=F)
  auxsample_G1 <- setdiff(1:nrow(data[G1_index,]), mainsample_G1)
  mainsample_G2 <- sample(nrow(data[G2_index,]), floor(nrow(data[G2_index,])/2), replace=F)
  auxsample_G2 <- setdiff(1:nrow(data[G2_index,]), mainsample_G2)
  
  YgivenX.Pred_W0 <- YgivenX.Pred_W1 <- WgivenX.Pred <- rep(NA, nrow(data))
  
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
  
  message <- capture.output( TaugivenQ.Model.Main_G1 <- train(as.formula(paste("tau", paste(Q_names,sep="+"), sep="~")), data=data_cond[G1_index,][mainsample_G1,], method="nnet", 
                                                              preProc=c("center","scale"), trControl=trainControl(method="none"), linout=TRUE, 
                                                              tuneGrid=expand.grid(size=2,decay=0.02)) )
  
  message <- capture.output( TaugivenQ.Model.Aux_G1 <- train(as.formula(paste("tau", paste(Q_names,sep="+"), sep="~")), data=data_cond[G1_index,][auxsample_G1,], method="nnet", 
                                                              preProc=c("center","scale"), trControl=trainControl(method="none"), linout=TRUE, 
                                                              tuneGrid=expand.grid(size=2,decay=0.02)) )
  
  message <- capture.output( TaugivenQ.Model.Aux_G2 <- train(as.formula(paste("tau", paste(Q_names,sep="+"), sep="~")), data=data_cond[G2_index,][mainsample_G2,], method="nnet", 
                                                              preProc=c("center","scale"), trControl=trainControl(method="none"), linout=TRUE, 
                                                              tuneGrid=expand.grid(size=2,decay=0.02)) )
  
  message <- capture.output( TaugivenQ.Model.Main_G2 <- train(as.formula(paste("tau", paste(Q_names,sep="+"), sep="~")), data=data_cond[G2_index,][auxsample_G2,], method="nnet", 
                                                              preProc=c("center","scale"), trControl=trainControl(method="none"), linout=TRUE, 
                                                              tuneGrid=expand.grid(size=2,decay=0.02)) )
  
  TaugivenQ.Pred_G1_G1 <- rep(NA, sum(G1_index))
  
  pred_data <- data_cond
  TaugivenQ.Pred_G1_G1[mainsample_G1] <- predict(YgivenX.Model.Aux_G1, newdata = pred_data[G1_index,][mainsample_G1,])

  rep(NA, nrow(data))
  
  YgivenX.Pred_W1[G1_index][mainsample_G1] <- predict(YgivenX.Model.Aux_G1, newdata = pred_data[G1_index,][mainsample_G1,])
  YgivenX.Pred_W1[G1_index][auxsample_G1] <- predict(YgivenX.Model.Main_G1, newdata = pred_data[G1_index,][auxsample_G1,])
  YgivenX.Pred_W1[G2_index][mainsample_G2] <- predict(YgivenX.Model.Aux_G2, newdata = pred_data[G2_index,][mainsample_G2,])
  YgivenX.Pred_W1[G2_index][auxsample_G2] <- predict(YgivenX.Model.Main_G2, newdata = pred_data[G2_index,][auxsample_G2,])
  
  

  ATE_i[G2_index][mainsample_G2] 
  
  ATE_i[G2_index][auxsample_G2] 
  
  ###  ###

  ### conditional prevalence ###
  #data$Q_cut <- cut(data[,Q], breaks=quantile(data[,Q], seq(0,1,0.2)),include.lowest=TRUE)
  data$Q_cut <- cut(data[,Q], breaks=quantile(data[,Q], seq(0,1,0.1)),include.lowest=TRUE)
  #data$Q_cut <- cut(data[,Q], breaks=quantile(data[,Q], seq(0,1,0.02)),include.lowest=TRUE)
  
  G1givenQ.Model <- svyglm(as.formula(paste(G1, paste("Q_cut",collapse="+"), sep="~")), family=gaussian(), 
                           design=svydesign(id=rownames(data), data=data, weights=data[,weight]))
  G1givenQ.Pred <- as.vector(predict(G1givenQ.Model))
  
  wht1 <- data[,weight]/mean(data[,weight])
  r <- G1givenQ.Pred/(1-G1givenQ.Pred)*mean(data[,G2]*wht1)/mean(data[,G1]*wht1)  # the weight
  # mean(r[G2_index]*wht[G2_index])  # when the G1~Q model is saturated lm or glm, this mean is always 1. This is nice because E(r(Q)|G2=1)=1. 

  c_prevalence <- (1/2)*(mean(ATE_i[G1_index])+mean(ATE_i[G2_index]))*(mean(data[,W][G1_index]*wht[G1_index])-mean((r*data[,W])[G2_index]*wht[G2_index]))

  boot_out <- rep(NA, k)
  for (i in 1: k) {
  data_boot <- data[sample(1:nrow(data), nrow(data), replace = TRUE),]
  G1givenQ.Model <- svyglm(as.formula(paste(G1, paste("Q_cut",collapse="+"), sep="~")), family=gaussian(), 
                           design=svydesign(id=rownames(data_boot), data=data_boot, weights=data_boot[,weight]))
  G1givenQ.Pred <- as.vector(predict(G1givenQ.Model))
  
  r <- G1givenQ.Pred/(1-G1givenQ.Pred)*weighted.mean(data_boot[,G2], data_boot[,weight])/weighted.mean(data_boot[,G1],data_boot[,weight])
  boot_out[i] <- weighted.mean(data_boot[,W][data_boot[,G1]==1], data_boot[,weight][data_boot[,G1]==1])-
    weighted.mean((r*data_boot[,W])[data_boot[,G2]==1], data_boot[,weight][data_boot[,G2]==1])
  }

  se.c_prevalence <- 1/sqrt(nrow(data)/2)*(1/2)*(mean(ATE_i[G1_index])+mean(ATE_i[G2_index]))*sd(boot_out)
  ###  ###
  
  output <- list(
    underlying=c(mean(data[,Y][G1_index]*wht[G1_index]),
                 mean(data[,Y][G2_index]*wht[G2_index]),
                 mean(Y0_i[G1_index]),
                 mean(Y0_i[G2_index]),
                 mean(data[,W][G1_index]*wht[G1_index]),
                 mean(data[,W][G2_index]*wht[G2_index]),
                 mean(ATE_i[G1_index]),
                 mean(ATE_i[G2_index]),
                 mean(ATT_i[G1_index]),
                 mean(ATT_i[G2_index])),
    underlying_diff=c(total,
                      baseline,
                      mean(data[,W][G1_index]*wht[G1_index])-mean(data[,W][G2_index]*wht[G2_index]),
                      mean(ATE_i[G1_index])-mean(ATE_i[G2_index]),
                      mean(ATT_i[G1_index])-mean(ATT_i[G2_index])),
    decomp=c(total,
             baseline,
             prevalence,
             effect,
             selection,
    )
  )
  
  colnames(output$underlying) <- colnames(output$underlying_diff) <- colnames(output$decomp) <- c("average","se","p_value","lowerCI","upperCI")
  row.names(output$underlying) <- c("Y_G1","Y_G2","Y0_G1","Y0_G2","W_G1","W_G2","ATE_G1","ATE_G2","ATT_G1","ATT_G2")
  row.names(output$underlying_diff) <- c("Y","Y0","W","ATE","ATT")
  row.names(output$decomp) <- c("Total","Baseline","Prevalence","Effect","Selection","Conditional prevalence")

  return(output)
}

