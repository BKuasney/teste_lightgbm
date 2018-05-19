##############################################################################
########## SALVAR CÓDIGOS E ANOTAÇÕES NO EVERNOTE ############################
##############################################################################

dt <- read.csv("C:/Users/bkuasney/Desktop/ML/FREELA/tratamento_outlier_fase2_CASA.csv")
names(dt)

head(dt)
dt <- dt[,c(-1,-2)]

install.packages("pROC")
install.packages("microbenchmark")

# Configurações Iniciais comuns aos modelos
library("pROC")
library("microbenchmark")
set.seed(42)

# Read
imoveis = read.csv("C:/Users/bkuasney/Desktop/ML/ML ARTIGOS - HANDS ON/gbm vs xgboost vs lightgbm/creditcard.csv")
head(credit.card.data)
names(credit.card.data)
ncol(credit.card.data)
nrow(credit.card.data)
summary(credit.card.data)

# SPlit
train.test.split = sample(2, nrow(credit.card.data), replace = TRUE, prob = c(0.7, 0.3))
train = credit.card.data[train.test.split == 1,]
test = credit.card.data[train.test.split == 2,]


# MODELING ###################################################

#######################
# GBM #################
#######################
library(gbm)
system.time(
  gbm.model <- gbm(Class ~., distribution = "bernoulli", data = rbind(train, test), n.trees = 500, interaction.depth = 3,
                   n.minobsinnode = 100, shrinkage = 0.01, bag.fraction = 0.5, train.fraction = nrow(train)/(nrow(train)+nrow(test)))
)

# Determinando a melhor iteração
best.iter = gbm.perf(gbm.model, method = "test") # melhor é 498 (entrar na função para ver tal valor)

# Importância das variáveis
gbm.feature.imp <- summary(gbm.model, n.trees = best.iter)
gbm.feature.imp <- summary(gbm.model2, n.trees = 500)

# Plot e calculo de AUC
gbm.test = predict(gbm.model, newdata = test, n.trees = best.iter)
install.packages("pROC")
require("pROC")
auc.gbm = roc(test$Class, gbm.test, plot = TRUE, col = "red")

gbm.test2 = predict(gbm.model2, newdata = test, n.trees = 500)
auc.gbm = roc(test$Class, gbm.test2, plot = TRUE, col = "red")

#Caso seja necessário o grid search para parameter tunning (adicionar outros elementos no grid que estão no código acima)
#grid = expand.grid(.n.trees=seq(100,500,by=200),.interaction.depth=seq(1,4,by=1), .shrinkage=c(.001,.01,.1), .n.minobsinnode = 100)
#control = trainControl(method="LOOCV")
#gbm.train = train(Class ~., data=train, method="gbm", trControl=control, tuneGrid = grid) # não conseguiu processar
#system.time(
#  gbm.model2 <- gbm(Class~., data=train, n.trees = 500, interaction.depth = 3, shrinkage=0.01, distribution = "bernoulli")
#)


#######################
# XGBOOSTING ##########
#######################
install.packages("xgboost")
require("xgboost")
?xgboost
#### PESQUISAR COMO QUE FUNCIONA EXATAMENTE OS PARÂMETROS DO XGBOOST




