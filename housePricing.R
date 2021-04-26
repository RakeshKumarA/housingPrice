#Load Package
library('tidyverse')
library('e1071')
library(corrplot)
library(functional)
library(reshape2)

##Read Training data
df_train <- read_csv('train.csv')

##Check columns
colnames(df_train)

##Summary of sale price

summary(df_train$SalePrice)

## histogram on sale price

df_train %>% 
  ggplot(aes(SalePrice)) + 
  geom_histogram(aes(y=..density..), bins = 50, fill='lightblue') + 
  geom_density(col = "darkblue", size=2)

##Skewness and Kurtosis
skewness(df_train$SalePrice)
kurtosis(df_train$SalePrice)

##Scatter plot of saleprice and Grlivarea

df_train %>% 
  ggplot(aes(x = GrLivArea, y=SalePrice)) + 
  geom_point(color='blue')

##Scatter plot of saleprice and TotalBsmtSF

df_train %>% 
  ggplot(aes(x = TotalBsmtSF, y=SalePrice)) + 
  geom_point(color='blue')

##Box plot of OverallQual over sale price

df_train %>% 
  ggplot(aes(x=factor(OverallQual), y=SalePrice)) + 
  geom_boxplot(aes(fill=factor(OverallQual)))

##Box plot of Year built over sale price

df_train %>% 
  ggplot(aes(x=factor(YearBuilt), y=SalePrice)) + 
  geom_boxplot(aes(fill=factor(YearBuilt)), show.legend = FALSE)

##Find only numeric valued variables and drop NA's

df_train_num <- df_train %>% select_if(is.numeric) %>% drop_na()

##Find correlation of each element against each other

df_train.cor <- cor(df_train_num)

## Drawing a heated map
melted_df <- melt(df_train.cor)
head(melted_df)
ggplot(data = melted_df, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() + scale_fill_gradient(low="yellow", high="red")


##Zoomed to only top sale price correlations

melted_df

## Shows great positive correlation
melted_df %>% filter(Var1 == 'SalePrice') %>% arrange(desc(value)) %>% head(10)

## Shows very low negative correlation - CAn be discarded.
melted_df %>% filter(Var1 == 'SalePrice') %>% arrange((value)) %>% head(10)

## Required variables 

reqVariables <- c("SalePrice","OverallQual","GrLivArea","GarageCars",
                  "TotalBsmtSF", "FullBath", "YearBuilt")


selectedVariables <- df_train_num %>% select(reqVariables)
library(GGally)
selectedVariables

ggpairs(selectedVariables)

