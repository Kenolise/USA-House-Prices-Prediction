library(odbc)
library(dplyr)
library(corrplot)
library(leaps) # helps with function StepAIC
con <- dbConnect(odbc::odbc(),
                 Driver = "SQL Server",
                 Server = "LOGISTICS\\SQLEXPRESS",
                 Database = "HomePrediction",
                 Port = 1433)

View(train)

#Calculate PCA so that we can determine what are the significant columns
#We first convert to numeric because it can only work on numeric columns
traindata.numeric <- unlist(lapply(train, is.numeric), use.names = FALSE) 
corrplot(cor(train[traindata.numeric]),method = 'number', tl.cex =0.5)

dfnum<- select_if(train, is.numeric)
View(dfnum)

pcaresults <- prcomp(dfnum, scale = TRUE)
pcaresults$rotation <- -1*pcaresults$rotation
pcaresults$rotation

#Lets reverse the signs with -1

pcaresults$x <- -1*pcaresults$x

View(pcaresults$x)

biplot(pcaresults, scale =0)


# list rows of data that have missing values 
missing_row <- train[!complete.cases(train),]
head(missing_row)
nrow(missing_row)

## show all variable names
var_name <- names(train)
var_name


#Here, we select these important variables by creating a vector that contains variable names
select_var <- c('Id','MSZoning','Utilities', 'Neighborhood','BldgType','HouseStyle',
                'OverallQual','OverallCond','YearBuilt', 'ExterQual','ExterCond',
                'BsmtQual','BsmtCond','TotalBsmtSF','Heating','HeatingQC', 
                'CentralAir','Electrical','GrLivArea','BedroomAbvGr','KitchenAbvGr',
                'KitchenQual','TotRmsAbvGrd','Functional','Fireplaces','FireplaceQu',
                'GarageArea','GarageQual','GarageCond','OpenPorchSF','PoolArea',
                'Fence','MoSold','YrSold','SaleType','SaleCondition','SalePrice')

# construct subset of train dataset that is used for prediction
select_train <- train[,select_var]
View(select_train)
summary(select_train)

summary(select_train$SalePrice)

# Draw a higtogram to figure out the distribution of SalePrice
options(scipen=10000)
ggplot(select_train, aes(x = SalePrice, fill = ..count..)) +
  geom_histogram(binwidth = 5000) +
  ggtitle("Figure 1 Histogram of SalePrice") +
  ylab("Count of houses") +
  xlab("Housing Price") + 
  theme(plot.title = element_text(hjust = 0.5))


model_var <- c('SalePrice', 
               'OverallQual','OverallCond','YearBuilt','ExterCond',
               'TotalBsmtSF','HeatingQC', 
               'CentralAir','GrLivArea','BedroomAbvGr','KitchenAbvGr',
               'TotRmsAbvGrd','Fireplaces',
               'GarageArea','OpenPorchSF','PoolArea',
               'YrSold')
heat <- select_train[,model_var]


#plot correlation heatmap for SalePrice
options(repr.plot.width=8, repr.plot.height=6)
library(ggplot2)
library(reshape2)
qplot(x=Var1, y=Var2, data=melt(cor(heat, use="p")), fill=value, geom="tile") +
  scale_fill_gradient2(low = "green", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1))+
  coord_fixed()+
  ggtitle("Figure 7 Correlation Heatmap") +
  theme(plot.title = element_text(hjust = 0.4))



# convert factor to numeric
select_train$ExterCond2 <- as.numeric(factor(select_train$ExterCond, 
                                             levels = c("Ex", "Fa","Gd", "TA","Po"),
                                             labels = c(5,2,4,3,1) ,ordered = TRUE))
select_train$HeatingQC2 <- as.numeric(factor(select_train$HeatingQC, 
                                             levels = c("Ex", "Fa","Gd", "TA","Po"),
                                             labels = c(5,2,4,3,1) ,ordered = TRUE))
select_train$CentralAir2 <- as.numeric(factor(select_train$CentralAir, 
                                              levels = c("N", "Y"),
                                              labels = c(0,1) ,ordered = TRUE))



#select variables that be used for model buidling and heat map
model_var <- c('SalePrice', 
               'OverallQual','OverallCond','YearBuilt','ExterCond2',
               'TotalBsmtSF','HeatingQC2', 
               'CentralAir2','GrLivArea','BedroomAbvGr','KitchenAbvGr',
               'TotRmsAbvGrd','Fireplaces',
               'GarageArea','OpenPorchSF','PoolArea',
               'YrSold')
heat <- select_train[,model_var]



#prediction of lm
#build model dataset for linear regression 
model_lin <- select_train[, model_var]
model_lin$lSalePrice <- log(model_lin$SalePrice)

#partition data
set.seed(10000)
train.index <- sample(c(1:dim(model_lin)[1]), dim(model_lin)[1]*0.8)
model_lin_train = model_lin[train.index,]
model_lin_valid <- model_lin[-train.index,]




#use lm() to run linear regression of SalePrice on all variables in model dataset
linreg <- lm(lSalePrice~.-SalePrice, data = model_lin_train)
summary(linreg)




library(forecast)
#We use predict() to make prediction on a new set
pred1 <- predict(linreg,model_lin_valid,type = "response")

residuals <- model_lin_valid$lSalePrice - pred1

linreg_pred <- data.frame("Predicted" = pred1*10000, "Actual" = model_lin_valid$lSalePrice*10000, "Residual" = residuals*10000)

accuracy(pred1, model_lin_valid$lSalePrice)

head(linreg_pred)
View(linreg_pred)


write.csv(linreg_pred, "regression.csv")
