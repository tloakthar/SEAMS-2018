# This script checks the performance of
# logistic regression classifiers 

# The script is used to generate plots in
# Fig. 8 of the SEAMS submission.

# Author: thein.tun@open.ac.uk
# Last Updated: 1st March 2018

rm(list=ls())

#library(e1071)
#library(klaR)
# setwd("~/repos/github/relais2015/r-scripts/")

for (n_users in c(6,9)) {
  sr_fname <- paste("data/share_request",n_users,"300",sep = "-")
  sr_fname <- paste(sr_fname,"rds",sep = ".")
  
  sl_fname <- paste("data/share_label",n_users,"300",sep = "-")
  sl_fname <- paste(sl_fname,"rds",sep = ".")
  
  requests_data <- readRDS(sr_fname)
  labels_data <- readRDS(sl_fname)
  
  # Identifiy rows for attack and non-attack
  # get the class balance so that sample has same balance
  attack_cases <- rep(1:length(labels_data[labels_data == 1]))
  nattack_cases <-
    rep(length(labels_data[labels_data == 1]) + 1:length(labels_data[labels_data ==
                                                                       0]))
  
  #define the number of instances in each of the sizes
  sizes <- c(100,200,300)
  
  for (k in c(1:length(sizes))) {
    #define the number of attack instances to consider
    # in the sample size
    num_pos_inst <-
      round(length(attack_cases) * sizes[k] / length(labels_data))
    a_index <- sample(attack_cases,num_pos_inst,replace = FALSE)
    na_index <-
      sample(nattack_cases,sizes[k] - length(a_index),replace = FALSE) # keep the same number of total number of instances
    class_rat <- length(na_index) / length(a_index)
    rindex <- sort(c(a_index,na_index))
    #share_requests<-requests_data[rindex, ]
    #share_labels<-labels_data[rindex]
    
    attack_positive <- matrix(NA,num_pos_inst,10)
    colnames(attack_positive) <- c(paste("runs# =", 1:10))
    rownames(attack_positive) <-
      c(paste("#post ins", 1:num_pos_inst))
    attack_negative <- attack_positive
    
    for (i in 1:num_pos_inst) {#postive instances
      for (j in 1:10) {
        sample_a_index <- sort(sample(a_index,i,replace = FALSE))
        sample_na_index <-
          sort(sample(na_index,i * class_rat,replace = FALSE))
        sample_rindex <- c(sample_a_index,sample_na_index)
        
        train.data <- requests_data[sample_rindex,]
        train.lab <- labels_data[sample_rindex]
        test.data <- requests_data
        test.lab <- labels_data
        
        train.requests<-cbind(train.data,train.lab)
        colnames(train.requests)[which(names(train.requests) == "train.lab")] <- "attacked"
        train.requests<-train.requests[,apply(train.requests, 2, var, na.rm=TRUE) != 0]
        
        test.requests<-cbind(test.data,test.lab)
        colnames(test.requests)[which(names(test.requests) == "test.lab")] <- "attacked"
        test.requests<-test.requests[,colnames(train.requests)]
        
        classifier.a <- glm(attacked~.,family = "binomial",data = train.requests,control=glm.control(maxit=50))
        classification.result.a <-
          predict(classifier.a,newdata = test.requests, type = "response")
        
        attack_positive[i,j] <-
          table(round(classification.result.a),test.lab)[2,2] / length(attack_cases)
        attack_negative[i,j] <-
          table(round(classification.result.a),test.lab)[1,1] / length(nattack_cases)
      } #end of j
    } # end of i
    result <-rbind(rowMeans(attack_positive),rowMeans(attack_negative))
    
    out_fname <- paste("figs/logreg-perf",n_users,sizes[k],sep = "-")
    out_fname <- paste(out_fname,"pdf",sep = ".")
    pdf(out_fname)
    par(mar=c(5,5,1,1))
    matplot(t(result), type = "l", cex.lab=2, cex.axis=1.6,xlab = "#attack instance", ylab = "Accuracy of classification")
    
    legend(
      'bottomright', inset = .05, cex=1.3, legend = c("attack","non-attack"), 
      pch = "--", horiz = FALSE, col = 1:5
    )
    dev.off()
  } # end of k
} #end of nusers
