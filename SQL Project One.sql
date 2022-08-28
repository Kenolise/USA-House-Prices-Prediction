CREATE DATABASE HomePrediction
USE HomePrediction

SELECT COUNT(*) 
FROM INFORMATION_SCHEMA.COLUMNS
WHERE table_name = 'test'

SELECT COUNT(*) 
FROM INFORMATION_SCHEMA.COLUMNS
WHERE table_name = 'train'

SELECT * FROM train
SELECT * FROM test

CREATE VIEW SignificantColumns AS
SELECT  SalePrice, GrLivArea, OverallQual, GarageCars, GarageArea, FullBath, 1stFlrSF, TotalBsmtSF, TotRmsAbvGrd, YearBuilt, 
YearRemodAdd, Fireplaces, OpenPorchSF, 2ndFlrSF, BsmtFinSF1, WoodDeckSF, HalfBath, BedroomAbvGr, BsmtUnfSF, LotArea, 
BsmtFullBath, PoolArea, ScreenPorch, MoSold, 3SsnPorch, Id, BsmtFinSF2, LowQualFinSF, MiscVal, BsmtHalfBath, MSSubClass, YrSold, 
KitchenAbvGr, EnclosedPorch, OverallCond

FROM train

SELECT * FROM regression_prediction

SELECT * FROM SignificantColumns
