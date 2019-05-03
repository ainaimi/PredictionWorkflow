packages <- c("missForest","splines","polspline","pROC",
              "data.table","parallel","SuperLearner",
              "doParallel","doRNG","cmprsk","caret",
              "tidyverse","VIM","caret","here")

for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package, repos='http://lib.stat.cmu.edu/R/CRAN',dependencies=TRUE)
  }
}

for (package in packages) {
  library(package, character.only=T)
}

thm <- theme_classic() +
  theme(
    legend.position = "top",
    legend.title=element_blank(),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = "transparent", colour = NA)
  )
theme_set(thm)


# Load Data and Select Variables ----------------------------------------------------------------
library(foreign)
m0 <- read.dta(file="./completeMOMI1.dta")
head(m0)
nrow(m0)
names(m0)

keepers <- c("recordid","rank_pregid","delyear","ddadage","ddadeduc","delwksgt","dmomadm","dmomhgt",
             "dmomage","dmomeduc","dmommari","dmomrace","doutborn","dzip",
             "smoke","grgrav","grage","abortion","teen2","preterm","dfc",
             "grga","grbw","primip","grmomwt","married","grapgar1","grapgar5",
             "race2","singlton")

momi <- m0[,keepers]
names(momi)
length(momi$rank_pregid)
length(unique(momi$rank_pregid))
momi$delwksgt <- as.numeric(momi$delwksgt)
momi$ddadeduc <- as.numeric(momi$ddadeduc)
momi$dmomhgt <- as.numeric(momi$dmomhgt)
momi$dmomeduc <- as.numeric(momi$dmomeduc)

# Quantiles ---------------------------------------------------------------
deciles <- function(x) {quantile(x,(1:10)/10,na.rm=T)}
apply(momi,2,deciles)

############################ not working
histogramer <- function(variable,filter_val){
  var <- enquo(variable)
  momi %>% filter(!!var<=filter_val) %>%  ggplot(.) + geom_histogram(aes(!!var))
}
histogramer(variable=delwksgt,filter_val=45)
###########################

head(momi)
tail(momi)
names(momi)
table(as.numeric(momi$delwksgt>42))

momi <- momi %>% filter(delwksgt<=42&delwksgt>=20)


# Plot Variables ----------------------------------------------------------
momi %>% filter(delwksgt<=45) %>%  ggplot(.) + geom_histogram(aes(delwksgt))
momi %>% filter(ddadage<99) %>%  ggplot(.) + geom_histogram(aes(ddadage))
momi %>% filter(ddadeduc<99) %>%  ggplot(.) + geom_histogram(aes(ddadeduc))
momi %>% filter(dmomhgt<999) %>%  ggplot(.) + geom_histogram(aes(dmomhgt))
momi %>% filter(dmomage<90) %>%  ggplot(.) + geom_histogram(aes(dmomage))
momi %>% filter(dmomeduc<99) %>%  ggplot(.) + geom_histogram(aes(dmomeduc))

momi <- momi %>% filter(!duplicated(rank_pregid)&singlton==1&complete.cases(.)) %>%
  mutate(ptb=as.numeric(delwksgt<37),race2=ifelse(race2=="",0,race2))

str(momi$dfc)

momi <- momi %>% mutate(dmomrace=ifelse(dmomrace=="","0",dmomrace),
                        dmomrace=ifelse(dmomrace=="99","1",dmomrace),
                        dmomrace=ifelse(!(dmomrace %in% c("0","1")),"0",dmomrace),
                        dmomrace=as.factor(dmomrace),
                        primip=as.numeric(ifelse(primip=="","0",primip)),
                        smoke=as.numeric(ifelse(smoke=="","0",smoke)),
                        dmomeduc=as.factor(dmomeduc),
                        teen2=as.numeric(ifelse(teen2=="","0",teen2)),
                        married=as.numeric(ifelse(married=="",0,married)),
                        dfc=as.factor(ifelse(momi$dfc %in% c("0","A","K","T","V","P","O","J","E"),"AAA",dfc)))

names(momi)
table(momi$married)
table(momi$singlton)

Train <- createDataPartition(momi$ptb, p=0.8, list=FALSE)
training <- momi[Train, ]
testing <- subset(momi,select=c(delyear,dmomrace,dmomage,dmomeduc,dmomhgt,ptb,primip,smoke,teen2,married,dfc,dzip))[-Train,]

aa <- data.frame(dzip=unique((training$dzip)),zip_code=1:length(unique(training$dzip)))
training <- merge(training,aa,by="dzip")
testing <- merge(testing,aa,by="dzip")

mean(training$ptb)
table(training$race2)
table(training$ptb,as.factor(training$ptb))
names(training)
str(training)

mod_fit <- glm(ptb ~ delyear+dmomrace+dmomage+dmomeduc+dmomhgt+primip+teen2+married+dfc,data=training,family=binomial(logit))
summary(mod_fit)

nrow(training)
nrow(testing)

testing$logit_predict <- predict(mod_fit,newdata=testing,type="response")
testing$logit_class <- as.numeric(testing$logit_predict > runif(nrow(testing)))

mean(as.numeric(testing$logit_predict))

a<-roc(testing$ptb, testing$logit_predict, direction="auto")
a$auc
B<-data.frame(sens=a$sensitivities,spec=a$specificities)
head(B)

## SUPERLEARNER
#dsamp <- downSample(x,as.factor(y))

#y <- as.numeric(dsamp$Class) - 1
y <- testing$ptb
x <- subset(testing,select=c(delyear,dmomrace,dmomage,dmomeduc,dmomhgt,primip,smoke,zip_code,teen2,married))

names(momi)

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

head(x)
head(testing)
fitY<-SuperLearner(Y=y,X=x,newX=subset(testing,select = names(x)),family="binomial",
                   method="method.AUC",
                   SL.library=sl.lib,
                   cvControl=list(V=5,stratifyCV=T))

# Note: for rare binary outcomes, consider using the stratifyCV option to
#		maintain roughly the same # of outcomes per fold
# View the output: 'Risk' column returns the CV estimates of (1-AUC)
#		'Coef' column gives the weights for the final SuperLearner (meta-learner)
fitY

#newX=subset(testing,select = names(x)),
# Obtain the predicted probability of the outcome from SL
testing$SL_pred<-fitY$SL.predict
head(testing)
testing$SL_class <- as.numeric(testing$SL_pred > runif(nrow(testing)))
p <- data.frame(y=testing$ptb, y_pred=testing$SL_pred)
head(p)

# Use the roc() function to obtain measures of performance for binary classification
a <- roc(p$y, p$y_pred, direction="auto")
a$auc
# To plot the ROC curve, we need the sensitivity and specificity
C<-data.frame(sens=a$sensitivities,spec=a$specificities)


A <- rbind(tibble(sens=B$sens,spec=B$spec,Model="Logistic"),
           tibble(sens=C$sens,spec=C$spec,Model="Super Learner"))

pdf("./figures/logit_vs_stacked.pdf",width=5,height=5)
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
dev.off()

## confusion matrix

confusionMatrix(as.factor(1-as.numeric(testing$logit_pred>runif(nrow(testing)))),
                as.factor(1-testing$ptb))

confusionMatrix(as.factor(1-as.numeric(testing$SL_pred>runif(nrow(testing)))),
                as.factor(1-testing$ptb))


## ALGORITHMIC BIAS

testing0 <- testing %>% filter(dmomrace==0)
testing1 <- testing %>% filter(dmomrace==1)

confusionMatrix(as.factor(as.numeric(testing0$SL_pred>runif(nrow(testing0)))),
                as.factor(testing0$ptb))

confusionMatrix(as.factor(as.numeric(testing1$SL_pred>runif(nrow(testing1)))),
                as.factor(testing1$ptb))






