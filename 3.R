#--------------------------USTAWIENIE WD------------------------------------------------
#setwd("C:/Users/Admin/Studia/Semestr 6/PADR/R-main")
setwd("C:/Users/aleks/OneDrive/Pulpit/sem6/PADR/R-main")
#--------------------------ANALIZA DANYCH ITP-------------------------------------------
#wczytanie danych
data<-read.table("mammographic_masses.data", header=FALSE, sep=",");
colnames(data) <- c('bi_rads', 'age', 'shape', 'margin', 'density', 'severity')

new_data <- replace(data, data=='?', NA); #zamiana ? na NA
new_data <- as.data.frame(lapply(new_data,as.numeric)) # konwersja wszystkich wartosci w dataframie na numeric

# Bindowanie kolorow do cech
featureColors <- c(bi_rads="#9ACD32",
                   age="#F08080",
                   shape="#B0E0E6",
                   margin="#DEB887",
                   density="#BA55D3",
                   severity="#66CDAA")

#--------funckje wazne----------------------------
getmode <- function(v, na.rm=TRUE) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

static_values <- function(v) {
  min <- min(v, na.rm = TRUE)
  max <- max(v, na.rm = TRUE)
  mean <- mean(v, na.rm = TRUE)
  sd <- sd(v, na.rm = TRUE)
  var <- var(v, na.rm = TRUE)
  median <- median(v, na.rm = TRUE)
  mode <- getmode(v, na.rm = TRUE)
  res <- c(min, max, mean, median, mode, sd, var)
  names(res) <-c('Min', 'Max', 'Mean', 'Median', 'Mode', 'Sd', 'Var')
  return(res)
}
#--------------------------------------------------

#wartosci statystyczne zbioru danych
data_stats <- sapply(new_data, static_values)



#------------------------USUNIECIE NIEKOMPLETNYCH DANYCH--------------------------------

data_clean <- new_data[complete.cases(new_data), ]


#dealing with element odstajacy
data_clean$bi_rads[data_clean$bi_rads==55] <- 5

#--------------------ANALIZA ZBIORU POD KATEM DETERMINANT ZLOSLIWOSCI GUZA------------------

bi_rads_benVSmag <- as.matrix(t(prop.table(table(data_clean[c('bi_rads', 'severity')]), 1)))
bi_rads_benVSmag <- cbind(bi_rads_benVSmag[,1], c(0.0,0.0), bi_rads_benVSmag[,-1])
colnames(bi_rads_benVSmag) <- as.character(0:6)

age_benVSmag <- t(prop.table(table(data_clean[c('age', 'severity')]), 1))
shape_benVSmag <- t(prop.table(table(data_clean[c('shape', 'severity')]), 1))
margin_benVSmag <- t(prop.table(table(data_clean[c('margin', 'severity')]), 1))
density_benVSmag <- t(prop.table(table(data_clean[c('density', 'severity')]), 1))


#------------------------------------STANDARYZACJA DANYCH-----------------------------------
# do zakresu [-1;1] (z wyj1tkiem severity)

s <- scale(data_clean[,2:5])

scaled_centers <- attr(s, 'scaled:center')
scaled_scales <- attr(s, 'scaled:scale')

data_stand <- data.frame(age=s[,1], shape=s[,2], margin=s[,3], density=s[,4], data_clean['severity'])


#----------------SIEC NEURONOWA----------------------------------------
## na razie na calosci zbioru, bez podzialu na treningowy i testowy


library(rpart)
library(caret)
require(neuralnet)

#podzia� na zbior treningnowy i testowy
#sev <- unlist(data_clean['severity'])


sev <- data_clean$severity
set.seed(100)
part <- createDataPartition(sev, p = 0.8, list  = FALSE)

training_set <- scale(data_clean[part, 2:5])
scaled_centers <- attr(training_set, 'scaled:center')
scaled_scales <- attr(training_set, 'scaled:scale')

testing_set <- data_clean[-part, 2:5]

training_sev <-sev[part]
testing_sev <-sev[-part]



data_stand_tr <- data.frame(age=training_set[,1], shape=training_set[,2], margin=training_set[,3], density=training_set[,4], training_sev)
data_ts <- data.frame(age=testing_set[,1], shape=testing_set[,2], margin=testing_set[,3], density=testing_set[,4], testing_sev)

nn <- neuralnet(training_sev ~ age+shape+margin+density, data=data_stand_tr,
                err.fct = "sse", hidden = 6, act.fct = "logistic")

predsVStarget_tr <- data.frame(case=rownames(data_stand_tr),
                               predictions=factor(round(unlist(nn$net.result), digits = 0), labels = c('benign', 'malignant')),
                               target=factor(data_stand_tr$training_sev, labels = c('benign', 'malignant')))

pred <-round(predict(nn, scale(testing_set, center = scaled_centers, scale = scaled_scales)), 2)

predsVStarget_ts <-data.frame(case = rownames(testing_set), 
                              predictions=factor(round(pred, digits = 0), labels = c('benign', 'malignant')),
                              target = factor(data_ts$testing_sev, labels = c('benign', 'malignant')))

confMatrix_tr <- confusionMatrix(data=predsVStarget_tr$predictions, reference=predsVStarget_tr$target, positive = "malignant")
confMatrix_ts <- confusionMatrix(data=predsVStarget_ts$predictions, reference=predsVStarget_ts$target, positive = "malignant")



# Faktoryzacja danych w data_clean na konkretne labele
# margin: [1,2,3,4,5] -> ["circumscribed", "microlobulated", "obscured", "ill-defined", "spiculated"]
# density: [1,2,3,4] -> ["high", "iso", "low", "fat-containing"]
# severity: [0,1] -> ["benign", "malignant"]

require(reshape2)
cormat <- round(cor(data_clean), 2);
melted_cormat <- melt(cormat);

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cormat);

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)



data_factorized <- data.frame(bi_rads=factor(data_clean$bi_rads, levels=(0:6)),
                              age=factor(data_clean$age),
                              shape=factor(data_clean$shape, labels = c("round", "oval", "lobular", "irregular")),
                              margin=factor(data_clean$margin, labels = c("circumscribed", "microlobulated", "obscured", "ill-defined", "spiculated")),
                              density=factor(data_clean$density, labels = c("high", "iso", "low", "fat-containing")),
                              severity=factor(data_clean$severity, levels=c(1,0), labels= c("malignant", "benign")))








sigmoid = function(x) {
  1 / (1 + exp(-x))
}
x <- seq(-10, 10, 1)
xx<-sigmoid(x)
xx <- data.frame(xx)

derivative = function(x) {
  f = expression(1 / (1 + exp(-x)))
  
  d<-D(f,'x')
}
f = expression(1 / (1 + exp(-x)))
d<-D(f,'x')
x <- seq(-10, 10, 1)
wynik <- eval(d)
f = expression(1 / (1 + exp(-x)))

d<-D(f,'x')