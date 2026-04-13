install.packages("psych")
install.packages("openxlsx")
install.packages("expss")
install.packages("maditr")
install.packages("xtable")
install.packages("ggplot2")

library(psych)
library(openxlsx)
library(expss)
library(maditr)
library(xtable)
library(ggplot2)
library(dplyr)


#Schnelltabelle
schnelleTabelle <- function(x) {
  write.table(x,
              sep = "\t",
              dec = ",",
              file = "clipboard-16384",
              row.names = FALSE,
              col.names = FALSE)
}

#Datensatz einlesen

data(bfi)
data <- bfi


Codebuch = info(data)

write.xlsx (data, file.choose ())
write.xlsx (Codebuch,file.choose ())


#Itemset nur mit Seg-Items


vars <- data %>%
  select(A1:O5) %>%
  na.omit()




# Ordner definieren (optional aber empfehlenswert)
output_path <- "C:/Users/kalin/Desktop/output/"

# Falls Ordner nicht existiert → erstellen
if(!dir.exists(output_path)) dir.create(output_path)

# Deskriptive Statistik speichern
Exploration = describe(vars)
write.xlsx(Exploration, 
           paste0(output_path, "01_Itemanalyse.xlsx"))

# Datensatz speichern
write.xlsx(data, 
           paste0(output_path, "02_Daten.xlsx"))

# Variablenlabels in Zwischenablage (bleibt gleich)
sapply(vars, var_lab) %>% schnelleTabelle()

# Variablennamen in Zwischenablage
names(vars) %>% schnelleTabelle()


#KMO Werte (MSAi)
#Faustregel: KMO sollte mind. 0.6 sein, aber besser mind. 0,8 betragen

#Berechnung KMO

KMO_Werte = KMO (vars)
KMO_Werte$MSAi %>% schnelleTabelle ()


#Anti Image Kovarianz Matrix

Anti <- KMO_Werte$ImCov
Anti %>% schnelleTabelle ()


#Korrelationsmatrix

cor(vars, use="complete.obs") %>% schnelleTabelle ()

#Inverse der Korrelationsmatrix

Korrelationsmatrix <-cor (vars, use="complete.obs")
solve (Korrelationsmatrix) %>% schnelleTabelle ()


#Bartlett Test der Grundgesamtheit (Prüfung der Korrelation/ Sphärizität)

cortest.bartlett(vars)


# Je nach Stichprobengröße ein anderer Test
# N=< 3000 -> Sharpio Wilk Test
# N> 3000 -> Kolmogorov Smirnov Test




#Sharpio-Test auf Normalverteilung pro Item
Shapiro = lapply(vars, shapiro.test) 
pWerte= Shapiro[[1]][[2]]
for (i in 2:25) {
  pWerte = c(pWerte, Shapiro [[i]] [[2]] )
}
pWerte %>% schnelleTabelle ()


# Werte sollen größer 0.5 sein              
WWerte = Shapiro [[1]] [[1]]
for (i in 2:25) {
  WWerte = c (WWerte, Shapiro [[i]] [[1]] )
}  
WWerte %>% schnelleTabelle ()


#Histograme

pdf(file = "Barplots_.pdf", paper="a4r") 
for (i in 1:25) {
Variable = vars [[i]] 
Grafik = ggplot (vars, aes (Variable)) + 
   geom_bar () +
   xlab (paste (names(vars [i]), var_lab(Variable), sep= " - "))+
   ylim(0, 1500) 
   print (Grafik) 
}
dev.off ()


