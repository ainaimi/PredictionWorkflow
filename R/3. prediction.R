#' List necessary packages here
packages <- c("splines","data.table","parallel","SuperLearner","caret",
              "doParallel","doRNG","tidyverse","VIM","here","pROC")

for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package, repos='http://lib.stat.cmu.edu/R/CRAN',dependencies=TRUE)
  }
}

for (package in packages) {
  library(package, character.only=T)
}

#' Format graphs in ggplot
thm <- theme_classic() +
  theme(
    legend.position = "top",
    legend.title=element_blank(),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = "transparent", colour = NA)
  )
theme_set(thm)

#' Read in data and explore

a <- read_csv(here("data","analytic_data.csv")) %>% mutate(dmomrace = as.factor(dmomrace),
                                                           dmomedu2 = as.factor(dmomedu2),
                                                           mhxpara = if_else(mhxpara>10,10,mhxpara),
                                                           mhxpara = as.factor(mhxpara),
                                                           dmommari = as.factor(dmommari))

glimpse(a)

a %>% count(mhxpara)

#' Create hold-out sample for honest validation using caret package function
set.seed(123)
train <- createDataPartition(a$ptb32sp, p=0.8, list=FALSE)
training <- a[train, ]
testing <- a[-train,]

training
testing

#' Check if training and testing add up to total

(nrow(training) + nrow(testing) == nrow(a))

#' Modeling
#' Using glm logit to predict
mod_fit <- glm(ptb32sp ~ .,data=training,family=binomial(logit))
summary(mod_fit)

#' This is the risk score from a logit model
testing$logit_predict <- predict(mod_fit,newdata=testing,type="response")

##' NEED TO PICK ALPHA APPROPRIATELY

testing$logit_class <- as.numeric(testing$logit_predict > runif(nrow(testing)))

mean(as.numeric(testing$logit_predict))

a<-roc(testing$ptb32sp, testing$logit_predict, direction="auto")
a$auc
B<-data.frame(sens=a$sensitivities,spec=a$specificities)
head(B)

## SUPERLEARNER
#dsamp <- downSample(x,as.factor(y))

#y <- as.numeric(dsamp$Class) - 1
y <- testing$ptb32sp
x <- subset(testing,select=c(-ptb32sp,-logit_predict,-logit_class))

xgboost_learner = create.Learner("SL.xgboost",tune=list(minobspernode = 50,max_depth = c(4),shrinkage=c(.01,.1),ntrees = c(200)),env=globalenv())
glmnet_learner = create.Learner("SL.glmnet",tune=list(alpha = seq(0,1,.5)),env=globalenv())
ranger_learner = create.Learner("SL.ranger",tune=list(num.trees=c(200),mtry=c(2,3),min.node.size=c(50)),env=globalenv())

sl.lib <- c(ranger_learner$names,
            xgboost_learner$names,
            glmnet_learner$names,
            "SL.rpartPrune",
            "SL.mean",
            "SL.glm",
            "SL.glm.interaction",
            "SL.bayesglm")

sl.lib <- ranger_learner$names

fitY <- SuperLearner(Y=y,X=x,family="binomial",
                     method="method.AUC",
                     SL.library=sl.lib,
                     cvControl=list(V=10,stratifyCV=T)) # V should be 10 or 20

#' Note: for rare binary outcomes, consider using the stratifyCV option to
#'		maintain roughly the same # of outcomes per fold
#' View the output: 'Risk' column returns the CV estimates of (1-AUC)
#;		'Coef' column gives the weights for the final SuperLearner (meta-learner)
fitY

#' Obtain the predicted probability of the outcome from SL
#' This is the risk score from the SL algorithm

newX <- subset(testing,select = names(x))
testing$SL_pred <- predict(fitY, newdata=newX, onlySL=T)$pred

##' NEED TO PICK ALPHA APPROPRIATELY

testing$SL_class <- as.numeric(testing$SL_pred > runif(nrow(testing)))
p <- data.frame(y=testing$ptb32sp, y_pred=testing$SL_pred)
head(p)

# Use the roc() function to obtain measures of performance for binary classification
a <- roc(p$y, p$y_pred, direction="auto")
a$auc
# To plot the ROC curve, we need the sensitivity and specificity
C<-data.frame(sens=a$sensitivities,spec=a$specificities)


A <- rbind(tibble(sens=B$sens,spec=B$spec,Model="Logistic"),
           tibble(sens=C$sens,spec=C$spec,Model="Super Learner"))

#pdf("./figures/logit_vs_stacked.pdf",width=5,height=5)
ggplot() +
  # geom_step(data=B, aes(1-spec,sens),color="red",size=.25) +
  # geom_step(data=C, aes(1-spec,sens),color="blue",size=.25) +
  geom_step(data=A,aes(1-spec,sens,color=Model)) +
  theme_light() + theme(panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank()) +
  labs(x = "1 - Specificity",y = "Sensitivity") +
  theme(legend.position = c(0.8, 0.2)) +
  geom_abline(intercept=0,slope=1,col="gray") +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  scale_color_manual(values=c("red", "blue"))
#dev.off()
