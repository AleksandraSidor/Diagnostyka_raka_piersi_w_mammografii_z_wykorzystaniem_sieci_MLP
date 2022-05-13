
#--------------------------ANALIZA DANYCH ITP-------------------------------------------
#wczytanie danych
data<-read.table("mammographic_masses.data", header=FALSE, sep=",");


nan_data <- which(data=='?'); #indeksy danych niekompletnych w całym zbiorze (bez sensu bo pokazuje indeksy jakby data było wektorem)
new_data <- replace(data, data=='?', NaN); #zamiana ? na NaN


#poszczegolne cechy
bi_rads <- as.numeric(unlist(new_data[1]));
age <- as.numeric(unlist(new_data[2]));
shape <- as.numeric(unlist(new_data[3]));
margin <- as.numeric(unlist(new_data[4]));
density <- as.numeric(unlist(new_data[5]));
severity <- as.numeric(unlist(new_data[6]));


#-------------------------histogramy pierwsze---------------------

bi_rads_h <- hist(bi_rads, main="Ocena BI-RADS", xlab="Wartość", ylab="Ilość wystąpień", xlim=c(0,max(bi_rads, na.rm = TRUE)), breaks=56, col="#9ACD32")
zero = which(bi_rads_h$counts == 0)
text(bi_rads_h$mids[-zero],bi_rads_h$counts[-zero],labels=bi_rads_h$counts[-zero], adj=c(0.5, -0.5))

age_h <- hist(age, main="Wiek pacjentki (w latach)", xlab="Wiek", ylab="Ilość wystąpień", xlim=c(0,100), breaks=99, col="#F08080")

shape_h <- hist(shape, main="Kształt guza", xlab="Kształt", ylab="Ilość wystąpień", xlim=c(0,4), breaks=0:4, col="#B0E0E6")
text(shape_h$mids,shape_h$counts,labels=shape_h$counts, adj=c(0.5, -0.5))

margin_h <- hist(margin, main="Margines guza", xlab="margines", ylab="Ilość wystąpień", xlim=c(0,max(margin, na.rm = TRUE)), breaks=0:5, col="#DEB887")
text(margin_h$mids,margin_h$counts,labels=margin_h$counts, adj=c(0.5, -0.5))

density_h <- hist(density, main="Gęstość guza", xlab="gęstość", ylab="Ilość wystąpień", xlim=c(0,max(density, na.rm = TRUE)), breaks=0:4, col="#BA55D3")
text(density_h$mids,density_h$counts,labels=density_h$counts, adj=c(0.5, -0.5))

severity_h <- hist(severity, right=F, main="Stopień zaawansowania guza", xlab="złośliwość", ylab="Ilość wystąpień", xlim=c(0,1), breaks=2, col="#66CDAA")
text(severity_h$mids,severity_h$counts,labels=severity_h$counts, adj=c(0.5, -0.5))



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
  
  return(c(min, max, mean, median, mode, sd, var))
}
#--------------------------------------------------

names = c('Min', 'Max', 'Mean', 'Median', 'Mode', 'Sd', 'Var');

#wartości statystyczne 
bi_rads_values <- matrix(c(names, static_values(bi_rads)), nrow = 2, byrow = TRUE);
age_values <- matrix(c(names, static_values(age)), nrow = 2, byrow = TRUE);
shape_values <- matrix(c(names, static_values(shape)), nrow = 2, byrow = TRUE);
margin_values <- matrix(c(names, static_values(margin)), nrow = 2, byrow = TRUE);
density_values <- matrix(c(names, static_values(density)), nrow = 2, byrow = TRUE);
severity_values <- matrix(c(names, static_values(severity)), nrow = 2, byrow = TRUE);


#------------------------USUNIECIE NIEKOMPLETNYCH DANYCH--------------------------------


#znajdowanie indeksów z wartością NaN
nan_bi_rads<-which(is.nan(bi_rads));
nan_age<-which(is.nan(age));
nan_shape<-which(is.nan(shape));
nan_margin<-which(is.nan(margin));
nan_density<-which(is.nan(density));
nan_severity<-which(is.nan(severity));

nan_all<-c(nan_bi_rads, nan_age, nan_shape, nan_margin, nan_density, nan_severity);
nan_all<-unique(nan_all); #indkesy niekompletnych wierszy w data

#usunięcie wektorów z niekompletnymi danymi
data_clean<-data[-nan_all, ];

#dealing with element odstający
data_clean$V1[data_clean$V1=='55'] <- '5'

#poszczegolne cechy po usunięciu niekompletnych wersów
bi_rads_new <- as.numeric(unlist(data_clean[1]));
age_new <- as.numeric(unlist(data_clean[2]));
shape_new <- as.numeric(unlist(data_clean[3]));
margin_new <- as.numeric(unlist(data_clean[4]));
density_new <- as.numeric(unlist(data_clean[5]));
severity_new <- as.numeric(unlist(data_clean[6]));



#wartości statystyczne nowe
bi_rads_new_values <- matrix(c(names, static_values(bi_rads_new)), nrow = 2, byrow = TRUE);
age__new_values <- matrix(c(names, static_values(age_new)), nrow = 2, byrow = TRUE);
shape_new_values <- matrix(c(names, static_values(shape_new)), nrow = 2, byrow = TRUE);
margin_new_values <- matrix(c(names, static_values(margin_new)), nrow = 2, byrow = TRUE);
density_new_values <- matrix(c(names, static_values(density_new)), nrow = 2, byrow = TRUE);
severity_new_values <- matrix(c(names, static_values(severity_new)), nrow = 2, byrow = TRUE)


#-------------------------histogramy po czyszczeniu---------------------

bi_rads_new_h <- hist(bi_rads_new, main="Ocena BI-RADS", xlab="Wartość", ylab="Ilość wystąpień", xlim=c(0,max(bi_rads_new, na.rm = TRUE)), breaks=5, col="#9ACD32")
text(bi_rads_new_h$mids,bi_rads_new_h$counts,labels=bi_rads_new_h$counts, adj=c(0.5, -0.5))

age_new_h <- hist(age_new, main="Wiek pacjentki (w latach)", xlab="Wiek", ylab="Ilość wystąpień", xlim=c(0,100), breaks=99, col="#F08080")

shape_new_h <- hist(shape_new, main="Kształt guza", xlab="Kształt", ylab="Ilość wystąpień", xlim=c(0,4), breaks=0:4, col="#B0E0E6")
text(shape_new_h$mids,shape_new_h$counts,labels=shape_new_h$counts, adj=c(0.5, -0.5))

margin_new_h <- hist(margin_new, main="Margines guza", xlab="margines", ylab="Ilość wystąpień", xlim=c(0,max(margin_new, na.rm = TRUE)), breaks=0:5, col="#DEB887")
text(margin_new_h$mids,margin_new_h$counts,labels=margin_new_h$counts, adj=c(0.5, -0.5))

density_new_h <- hist(density_new, main="Gęstość guza", xlab="gęstość", ylab="Ilość wystąpień", xlim=c(0,max(density_new, na.rm = TRUE)), breaks=0:4, col="#BA55D3")
text(density_new_h$mids,density_new_h$counts,labels=density_new_h$counts, adj=c(0.5, -0.5))

severity_new_h <- hist(severity_new, right=F, main="Stopień zaawansowania guza", xlab="złośliwość", ylab="Ilość wystąpień", xlim=c(0,1), breaks=2, col="#66CDAA")
text(severity_new_h$mids,severity_new_h$counts,labels=severity_new_h$counts, adj=c(0.5, -0.5))



#------------------------------------STANDARYZACJA DANYCH-----------------------------------

bi_rads_stand = scale(bi_rads_new)
age_stand = scale(age_new)
shape_stand = scale(shape_new)
margin_stand = scale(margin_new)
density_stand = scale(density_new)






