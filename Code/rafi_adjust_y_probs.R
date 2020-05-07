# from Rafi 

my.predictions<-lapply(1:nrow(model.summary), function(i){
  mod.i<-my.models[[i]]
  x.i<-my.newdfs2
  bsvar<-model.summary$BiostimVar[i]
  print(paste(i, bsvar))
  x.i$BiostimVar<-bsvar
  x.i$Biostim<-my.newdfs2[,bsvar]
  print(max(x.i$Biostim))
  
  x.i$Response<-model.summary$Response[i]
  x.i$BIgoal<-model.summary$BIgoal[i]
  x.predictions<-predict(mod.i, newdata=x.i, type="link", se.fit=T)
  
  x.i<-cbind(x.i, linkfit=x.predictions$fit, linkse=x.predictions$se.fit)
  x.i$linkfit.l95<-x.i$linkfit-(1.96*x.i$linkse)
  x.i$linkfit.u95<-x.i$linkfit+(1.96*x.i$linkse)
  x.i$pMeetObjective<-mod.i$family$linkinv(x.i$linkfit)
  x.i$pMeetObjective.l95<-mod.i$family$linkinv(x.i$linkfit.l95)
  x.i$pMeetObjective.u95<-mod.i$family$linkinv(x.i$linkfit.u95)
  x.i$pMeetObjective_rel<-x.i$pMeetObjective/max(x.i$pMeetObjective)
  x.i$pMeetObjective_rel.l95<-x.i$pMeetObjective.l95/max(x.i$pMeetObjective)
  x.i$pMeetObjective_rel.u95<-x.i$pMeetObjective.u95/max(x.i$pMeetObjective)
  x.i$pMeetObjective_rel.u95[x.i$pMeetObjective_rel.u95>1]<-1
  x.i
  
})

# https://blog.methodsconsultants.com/posts/bias-adjustment-for-rare-events-logistic-regression-in-r/

# At the same time, the authors do note that, even with a bias-corrected logit 
# estimator, predicted probabilities calculated the usual way (using the inverse 
# logit link function) remain too small. 

library(magrittr)

fit <- glm(as.factor(y) ~ x1, data = train, family = "binomial")

# extract coefs and coefs matrix
coefs <- fit %>% 
  coef() %>% 
  as.numeric()
V <- vcov(fit)

# function for adjusted predictions
logisticPred <- function(X, coef) {
  X %>%
    na.omit() %>%
    mutate(int = 1) %>%
    select(int, everything()) %>%
    as.matrix(.) %*% coef %>%
    as.vector() %>%
    (function(x) 1 / (1 + exp(-x)))
}

pred <- X_train %>%
  logisticPred(coefs)

test_pred <- X_test %>%
  logisticPred(coefs)

X_matrix <- X_train %>%
  mutate(bias = 1) %>%
  select(bias, everything()) %>%
  as.matrix()

X_test_matrix <- X_test %>%
  mutate(bias = 1) %>%
  select(bias, everything()) %>%
  as.matrix()

W <- diag(pred * (1 - pred))

Q <- X_matrix %*% solve(t(X_matrix) %*% W %*% X_matrix) %*% t(X_matrix)

e <- 0.5 * diag(Q) * (2 * pred - 1)

bias <- (solve(t(X_matrix) %*% W %*% X_matrix) %*% t(X_matrix) %*% W %*% e) %>%
  as.numeric()

unbiased_coefs <- coefs - bias

updated_var <- (nrow(X_train) / (nrow(X_train) + ncol(X_train) + 1)) ^ 2 * V







