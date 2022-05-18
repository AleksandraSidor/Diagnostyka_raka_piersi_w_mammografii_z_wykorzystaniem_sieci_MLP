
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


#-------------------------histogramy pierwsze---------------------

bi_rads_h <- hist(new_data$bi_rads, main="Ocena BI-RADS", xlab="Wartosc", ylab="Ilosc wystapien", xlim=c(0,max(new_data$bi_rads, na.rm = TRUE)), ylim=c(0, 600), breaks=56, col=featureColors['bi_rads'])

zero = which(bi_rads_h$counts == 0)
text(bi_rads_h$mids[-zero],bi_rads_h$counts[-zero],labels=bi_rads_h$counts[-zero], adj=c(0.5, -0.5))

age_h <- hist(new_data$age, main="Wiek pacjentki (w latach)", xlab="Wiek", ylab="Ilosc wystapien", xlim=c(0,100), breaks=99, col=featureColors['age'])


shape_h <- hist(new_data$shape, main="Ksztalt guza", xlab="Ksztalt", ylab="Ilosc wystapien", xlim=c(0,4), ylim=c(0, 450), breaks=0:4, col=featureColors['shape'], xaxt="n")
text(shape_h$mids,shape_h$counts,labels=shape_h$counts, adj=c(0.5, -0.5))
axis(1, shape_h$mids, c("round", "oval", "lobular", "irregular"), line=-1, lwd=0, lwd.ticks = 1)


margin_h <- hist(new_data$margin, main="Margines guza", xlab="Margines", ylab="Ilosc wystapien", xlim=c(0,max(new_data$margin, na.rm = TRUE)), ylim=c(0, 400), breaks=0:5, col=featureColors['margin'], xaxt="n")
text(margin_h$mids,margin_h$counts,labels=margin_h$counts, adj=c(0.5, -0.5))
axis(1, at=margin_h$mids, labels = rep_len("", length(margin_h$mids)), lwd=0, lwd.ticks = 1, line = -1)
text(margin_h$mids, par("usr")[3] - 0.2, labels = c("circumscribed", "microlobulated", "obscured", "ill-defined", "spiculated"),
     srt = 35, pos = 1, xpd = TRUE)


density_h <- hist(new_data$density, main="Gestosc guza", xlab="Gestosc guza", ylab="Ilosc wystapien", xlim=c(0,max(new_data$density, na.rm = TRUE)), ylim=c(0, 850), breaks=0:4, col=featureColors['density'], xaxt="n")
text(density_h$mids,density_h$counts,labels=density_h$counts, adj=c(0.5, -0.5))
axis(1, density_h$mids, c("high", "iso", "low", "fat-containing"), line=-1, lwd=0, lwd.ticks = 1)


severity_h <- hist(new_data$severity, right=F, main="Stopien zaawansowania guza", xlab="Zlosliwosc", ylab="Ilosc wystapien", xlim=c(0,1), ylim = c(0,600), breaks=2, col=featureColors['severity'], xaxt="n")
text(severity_h$mids,severity_h$counts,labels=severity_h$counts, adj=c(0.5, -0.5))
axis(1, severity_h$mids, c("benign", "malignant"), line=-1, lwd=0, lwd.ticks = 1)



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


#-------------------------histogramy po czyszczeniu---------------------

bi_rads_new_h <- hist(data_clean$bi_rads, main="Ocena BI-RADS", xlab="Wartosc", ylab="Ilosc wystapien", xlim=c(0,max(data_clean$bi_rads, na.rm = TRUE)), ylim=c(0, 500), breaks=5, col=featureColors['bi_rads'], xaxt="n")
text(bi_rads_new_h$mids,bi_rads_new_h$counts,labels=bi_rads_new_h$counts, adj=c(0.5, -0.5))
axis(1, bi_rads_new_h$mids, seq_along(bi_rads_new_h$mids))

age_new_h <- hist(data_clean$age, main="Wiek pacjentki (w latach)", xlab="Wiek", ylab="Ilosc wystapien", xlim=c(0,100), breaks=99, col=featureColors['age'])

shape_new_h <- hist(data_clean$shape, main="Ksztalt guza", xlab="Ksztalt", ylab="Ilosc wystapien", xlim=c(0,4), ylim=c(0,400), breaks=0:4, col=featureColors['shape'], xaxt="n")
text(shape_new_h$mids,shape_new_h$counts,labels=shape_new_h$counts, adj=c(0.5, -0.5))
axis(1, shape_new_h$mids, c("round", "oval", "lobular", "irregular"), line=-1, lwd=0, lwd.ticks = 1)

margin_new_h <- hist(data_clean$margin, main="Margines guza", xlab="Margines", ylab="Ilosc wystapien", xlim=c(0,max(data_clean$margin, na.rm = TRUE)), ylim=c(0,400), breaks=0:5, col=featureColors['margin'], xaxt="n")
text(margin_new_h$mids,margin_new_h$counts,labels=margin_new_h$counts, adj=c(0.5, -0.5))
axis(1, at=margin_new_h$mids, labels = rep_len("", length(margin_new_h$mids)), lwd=0, lwd.ticks = 1, line = -1)
text(margin_new_h$mids, par("usr")[3] - 0.2, labels = c("circumscribed", "microlobulated", "obscured", "ill-defined", "spiculated"),
     srt = 25, pos = 1, xpd = TRUE)

density_new_h <- hist(data_clean$density, main="Gestosc guza", xlab="Gestosc guza", ylab="Ilosc wystapien", xlim=c(0,max(data_clean$density, na.rm = TRUE)), ylim=c(0,800), breaks=0:4, col=featureColors['density'], xaxt="n")
text(density_new_h$mids,density_new_h$counts,labels=density_new_h$counts, adj=c(0.5, -0.5))
axis(1, density_h$mids, c("high", "iso", "low", "fat-containing"), line=-1, lwd=0, lwd.ticks = 1)


severity_new_h <- hist(data_clean$severity, right=F, main="Stopien zaawansowania guza", xlab="Zlosliwosc", ylab="Ilosc wystapien", xlim=c(0,1), ylim = c(0,500), breaks=2, col=featureColors['severity'], xaxt="n")
text(severity_new_h$mids,severity_new_h$counts,labels=severity_new_h$counts, adj=c(0.5, -0.5))
axis(1, severity_new_h$mids, c("benign", "malignant"), line=-1, lwd=0, lwd.ticks = 1)



#--------------------ANALIZA ZBIORU POD KATEM DETERMINANT ZLOSLIWOSCI GUZA------------------

bi_rads_benVSmag <- as.matrix(t(prop.table(table(data_clean[c('bi_rads', 'severity')]), 1)))
bi_rads_benVSmag <- cbind(bi_rads_benVSmag[,1], c(0.0,0.0), bi_rads_benVSmag[,-1])
colnames(bi_rads_benVSmag) <- as.character(0:6)

age_benVSmag <- t(prop.table(table(data_clean[c('age', 'severity')]), 1))
shape_benVSmag <- t(prop.table(table(data_clean[c('shape', 'severity')]), 1))
margin_benVSmag <- t(prop.table(table(data_clean[c('margin', 'severity')]), 1))
density_benVSmag <- t(prop.table(table(data_clean[c('density', 'severity')]), 1))


# barploty
par(mar = c(5.1, 4.1, 4.1, 6))
b <- barplot(bi_rads_benVSmag, col = c("seagreen3", "firebrick3"),
        main = "Ocena BI-RADS vs. udzial guzow lagodnych/zlosliwych",
        xlab = "Ocena BI-RADS", ylab = "%",
        legend.text = c("benign", "malignant"),
        args.legend = list(x = "topright", inset = c(-0.2, 0)))
text(b[2], 0.48, "BRAK", col="tomato2", srt=90, font=2)

barplot(age_benVSmag, col = c("seagreen3", "firebrick3"),
           main = "Wiek pacjentki vs. udzial guzów lagodnych/zlosliwych",
           xlab = "Wiek pacjentki", ylab = "%",
           legend.text = c("benign", "malignant"),
           args.legend = list(x = "topright", inset = c(-0.2, 0)))

b <- barplot(shape_benVSmag, col = c("seagreen3", "firebrick3"),
           main = "Ksztalt masy vs. udzial guzow lagodnych/zlosliwych",
           xlab = "Ksztalt masy", ylab = "%", xaxt="n",
           legend.text = c("benign", "malignant"),
           args.legend = list(x = "topright", inset = c(-0.2, 0)))

axis(1, b, c("round", "oval", "lobular", "irregular"), line=-1, lwd=0, lwd.ticks = 1)

b <- barplot(margin_benVSmag, col = c("seagreen3", "firebrick3"),
             main = "Margines masy vs. udzial guzow lagodnych/zlosliwych",
             xlab = "Margines masy", ylab = "%", xaxt="n",
             legend.text = c("benign", "malignant"),
             args.legend = list(x = "topright", inset = c(-0.2, 0)))

axis(1, b, rep("", length(b)), line=-1, lwd=0, lwd.ticks = 1)
text(b, par("usr")[3], labels = c("circumscribed", "microlobulated", "obscured", "ill-defined", "spiculated"),
     srt = 25, pos = 1, xpd = TRUE)

b <- barplot(density_benVSmag, col = c("seagreen3", "firebrick3"),
             main = "Gestosc masy vs. udzial guzow lagodnych/zlosliwych",
             xlab = "Gestosc masy", ylab = "%", xaxt="n",
             legend.text = c("benign", "malignant"),
             args.legend = list(x = "topright", inset = c(-0.2, 0)))

axis(1, b, c("high", "iso", "low", "fat-containing"), line=-1, lwd=0, lwd.ticks = 1)


#------------------------------------STANDARYZACJA DANYCH-----------------------------------
# do zakresu [-1;1] (z wyj¹tkiem severity)

s <- scale(data_clean[,-6])
# œrednie i odchylenia standardowe u¿yte do standaryzacji s¹ zapamiêtywane
# w celu póŸniejszej standaryzacji danych wprowadzanych przez u¿ytkownika
scaled_centers <- attr(s, 'scaled:center')
scaled_scales <- attr(s, 'scaled:scale')

data_stand <- data.frame(bi_rads=s[,1], age=s[,2], shape=s[,3], margin=s[,4], density=s[,5], data_clean['severity'])


#----------------SIEC NEURONOWA----------------------------------------
## na razie na calosci zbioru, bez podzialu na treningowy i testowy

require(neuralnet)

## 2 warstwy ukryte, funkcja aktywacji=sigmoid, funkcja straty=SSE
nn <- neuralnet(severity ~ bi_rads+age+shape+margin+density, data=data_stand,
                err.fct = "sse", hidden = 2, act.fct = "logistic")


predsVStarget <- data.frame(case=rownames(data_stand),
                            predictions=factor(round(unlist(nn$net.result), digits = 0), labels = c('benign', 'malignant')),
                            target=factor(data_stand$severity, labels = c('benign', 'malignant')))

## Macierz pomylek
require(caret)
confMatrix <- confusionMatrix(data=predsVStarget$predictions, reference=predsVStarget$target, positive = "malignant")

fourfoldplot(confMatrix$table, color = c("firebrick2", "seagreen3"),
             conf.level = 0, margin = 1, main = "Macierz pomylek")


# Faktoryzacja danych w data_clean na konkretne labele
# margin: [1,2,3,4,5] -> ["circumscribed", "microlobulated", "obscured", "ill-defined", "spiculated"]
# density: [1,2,3,4] -> ["high", "iso", "low", "fat-containing"]
# severity: [0,1] -> ["benign", "malignant"]

data_factorized <- data.frame(bi_rads=factor(data_clean$bi_rads),
                              age=factor(data_clean$age),
                              shape=factor(data_clean$shape, labels = c("round", "oval", "lobular", "irregular")),
                              margin=factor(data_clean$margin, labels = c("circumscribed", "microlobulated", "obscured", "ill-defined", "spiculated")),
                              density=factor(data_clean$density, labels = c("high", "iso", "low", "fat-containing")),
                              severity=factor(data_clean$severity, levels=c(1,0), labels= c("malignant", "benign")))
