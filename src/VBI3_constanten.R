
#afgewerkt <- c(1,2,3)

# oppervlaktes----

opp_V <- 13625.55 # opp Landoppervlakte sinds 2018 in km² 
# (https://statbel.fgov.be/nl/themas/leefmilieu/grond/bodemgebruik) cfr advies INBO addendum 2020

opp_V_private_2021 <- 0.628907 * opp_V #  in km² 
opp_V_public_2021 <-  0.371093 * opp_V #  value


opp_ANT <- 2876.120363 ## oppervlakte  in km² statbel sinds 2019 2876.120363
opp_LIM <- 2427.428987 ## oppervlakte  in km²
opp_OVL <- 3007.075664 ## oppervlakte  in km²
opp_WVL <- 3196.575632 ## oppervlakte  in km²
opp_VBR <- 2118.348284 ## oppervlakte  in km²

opp_LMB <- 2427.428987 ## oppervlakte  in km²

# sample sizes----
nTot_ANT <- 5752 
nTot_LIM <- 4852
nTot_LMB <- 4852
nTot_OVL <- 6018
nTot_VBR <- 4236
nTot_WVL <- 6305

sample_size_vbi <- 27163

# start- en enddates----

#startVBI2 <- 2009
startVBI3 <- 2019
startVBI2 <- 2009
endVBI2 <- startVBI3 - 1


#minyear en maxyear per 'reeks' ifv wegschrijven naar resultatendb en reeksinfo----
maxyear <- endVBI2  + max(afgewerkt) 
maxyear2 <- maxyear # behouden om te voorkomen dat nog niet aangepaste code kraakt
minyear <- maxyear - 10 + 1 # behouden om te voorkomen dat nog niet aangepaste code kraakt
minyear2 <- minyear

minyear1 <- ifelse(max(afgewerkt < 4), 1997, ifelse(max(afgewerkt) < 7, 1998, 1999)) # zolang VBI3 niet volledig is afgewerkt. 
maxyear1 <- ifelse(max(afgewerkt < 1), 1999, max(afgewerkt) + startVBI2 - 1)


#timelaps----
timelaps  <-  paste(minyear2,'-' ,maxyear2) # behouden om te voorkomen dat nog niet aangepaste code kraakt
timelaps10 <- timelaps # steeds de timelaps van de laatste 10 jaar

timelaps20 <- (paste(minyear1,'-' ,maxyear2))


mtimelaps <- ( max(afgewerkt)*10 + (10-max(afgewerkt)) *16 ) / 10                   


# reeksinfo : voor de volledige beschrijving bij het wegschrijven ----
reeksinfo20 <- paste0(", aantal afgewerkte reeksen VBI3 : ", max(afgewerkt), ", berekening obv gegevens ", timelaps20)
reeksinfo10 <- paste0(", aantal afgewerkte reeksen VBI3 : ", max(afgewerkt), ", berekening obv gegevens ", timelaps10)
minreeks <- as.factor(1)
maxreeks <- as.factor(10)


# listMainSpecies----
# afgeleid van VBI2 (2009-2018) - en berkend ahv script 2_ab_VolumeAandeelPerBoomsoort.R

list20_N <- c("Grove den", "Berk", "Inlandse eik", "Zwarte els", "Corsicaanse den"
                , "Wilg", "Amerikaanse vogelkers", "Amerikaanse eik", "Gewone esdoorn", "Gewone es"
                , "Fijnspar", "Populier", "Hazelaar", "Beuk", "Tamme kastanje"
                , "Wilde lijsterbes", "Gewone vlier", "Lork species", "Meidoorn", "Witte els (grauwe)")

list20_G <- c("Grove den", "Inlandse eik", "Corsicaanse den", "Populier", "Berk"
              , "Beuk", "Amerikaanse eik", "Zwarte els", "Wilg", "Fijnspar"
              , "Gewone es", "Gewone esdoorn", "Lork species", "Tamme kastanje", "Amerikaanse vogelkers"
              , "Douglas", "Robinia (gewone)", "Boskers", "Hazelaar", "Haagbeuk")

list20_V <- c("Grove den", "Inlandse eik", "Populier", "Corsicaanse den", "Beuk"
              , "Berk", "Amerikaanse eik", "Zwarte els", "Wilg", "Gewone es"
              , "Lork species", "Fijnspar", "Gewone esdoorn", "Tamme kastanje", "Douglas" # es en lork staan gelijk op plaats 10
              , "Amerikaanse vogelkers", "Robinia (gewone)", "Boskers", "Zeeden", "Haagbeuk"
              , "Witte en Grauwe abeel")

list12_N <- list20_N[1:12] 
list12_G <- list20_G[1:12] 
list12_V <- list20_V[1:12] 

list10_N <- list20_N[1:10] 
list10_G <- list20_G[1:10] 
list10_V <- list20_V[1:10] 

# list testplots----

testplot <- c(392054)
