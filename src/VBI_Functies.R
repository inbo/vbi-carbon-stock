
##############################
#### FUNCTIES
#####################################



#'  Berekening gewogen gemiddelde en 95% betrouwbaarheidsintervallen op basis van gewogen variantie
#'
#' Langere functiebeschrijving
#' @param Data Analysedataset
#' @param VariableName vector met de namen van de responsvariabelen (die de parameterschattingen bevatten) (als string)
#' @param Periode Welke bosinventaris (1, 2, ...)
#' @param MinReeks Eerste jaarreeks binnen een periode van 1 bosinventarisatie (12 jaar, dus tss 1 en 12)
#' @param MaxReeks Laatste jaarreeks binnen een periode van 1 bosinventarisatie (12 jaar, dus tss 1 en 12)
#' @param MinYear Eerste jaar van de geanalyseerde periode
#' @param MaxYear Laatste jaar van de geanalyseerde periode
#' @param UseStrata Gebruik je strata (logische vector) even lang als de vector VariableName
#' @param Strata Welke strata wil je gebruiken (per respons gedefinieerd in variableName)
#' @return
#' @importFrom
#' @examples

My.WgtParEstimation<-function(Data,VariableName,Periode=NA,MinReeks=1,MaxReeks=12,MinYear=1,MaxYear=9999,UseStrata=rep(FALSE,1000),Strata){


  if (is.na(Periode)){
    DataSel<-Data[Data$Reeks<=MaxReeks
                  & Data$Reeks>=MinReeks
                  & Data$Year>=MinYear
                  & Data$Year<=MaxYear,, drop = FALSE]
  } else {
    DataSel<-Data[Data$Periode==Periode
                  & Data$Reeks<=MaxReeks
                  & Data$Reeks>=MinReeks
                  & Data$Year>=MinYear
                  & Data$Year<=MaxYear,, drop = FALSE]
  }

  i<-1
  #Doorloop de functie voor iedere gedefinieerde respons
  for (Var in VariableName ){

    #Indien we per strata werken
    if (UseStrata[i]){
      s<-1

      #Loop over alle strata
      for(Stratum in levels(DataSel[,Strata[i]])){

        DataStrat <- DataSel[!is.na(DataSel[,Strata[i]])&(DataSel[,Strata[i]]==Stratum),, drop=FALSE]

        Variable <-DataStrat[,Var]

        if (nrow(DataStrat)>0){

          wgt.mean <- sum(DataStrat$Weight * Variable, na.rm=TRUE)/(sum(DataStrat$Weight * (!is.na(Variable)),na.rm=TRUE))

        } else {

          wgt.mean<-NA

          }

        if (nrow(DataStrat)>1){

          V1<-sum(DataStrat$Weight * (!is.na(Variable)),na.rm=T)

          V2<-sum(((DataStrat$Weight) * (!is.na(Variable)))^2,na.rm=T)

          wgt.var <-sum(DataStrat$Weight*(Variable-wgt.mean)^2)/(V1-(V2/V1))

          lci<-wgt.mean-1.96*sqrt(wgt.var)/sqrt(V1)

          uci<-wgt.mean+1.96*sqrt(wgt.var)/sqrt(V1)

        }else {

          wgt.var <-NA

          lci<-NA

          uci<-NA

        }

        outputS<-data.frame(variabele=Var,
                            strata=Strata[i],
                            stratumNaam=Stratum,
                            periode=Periode,
                            minYear=ifelse(nrow(DataStrat)>0,min(DataStrat$Year,na.rm=TRUE),NA),
                            maxYear=ifelse(nrow(DataStrat)>0,max(DataStrat$Year,na.rm=TRUE),NA),
                            minReeks=ifelse(nrow(DataStrat)>0,min(DataStrat$Reeks,na.rm=TRUE),NA),
                            maxReeks=ifelse(nrow(DataStrat)>0,max(DataStrat$Reeks,na.rm=TRUE),NA),
                            nbObservaties=length(DataStrat$IDPlots),
                            wgt.mean=wgt.mean,
                            wgt.var=wgt.var,
                            llci=lci,
                            ulci=uci)
        if (s<=1){
          outputT<-outputS
        } else {
          outputT<-rbind (outputT,outputS)
        }
        s<-s+1
      }

    } else { #Indien we niet met strata werken
      Variable <-DataSel[,Var]
      wgt.mean <- sum(DataSel$Weight * Variable, na.rm=TRUE)/sum(DataSel$Weight * (!is.na(Variable)),na.rm=TRUE)
      V1<-sum(DataSel$Weight * (!is.na(Variable)),na.rm=T)
      V2<-sum(((DataSel$Weight)* (!is.na(Variable)))^2,na.rm=T)
      wgt.var <-sum(DataSel$Weight*(Variable-wgt.mean)^2,na.rm=T)/(V1-(V2/V1))
      lci<-wgt.mean-1.96*sqrt(wgt.var)/sqrt(V1)
      uci<-wgt.mean+1.96*sqrt(wgt.var)/sqrt(V1)
      outputT<-data.frame(variabele=Var,
                          strata="",
                          stratumNaam="",
                          periode=Periode,
                          minYear=min(DataSel$Year),
                          maxYear=max(DataSel$Year),
                          minReeks=min(DataSel$Reeks),
                          maxReeks=max(DataSel$Reeks),
                          nbObservaties=nrow(DataSel),
                          wgt.mean=wgt.mean,
                          wgt.var=wgt.var,
                          llci=lci,
                          ulci=uci)

    }
    if (i<=1){
      output<-outputT
    } else {
      output<-rbind (output,outputT)
    }
    i<-i+1
  }
  data.frame(output)
}

################################################################################################################################
#-------------------------------------------------------------------------------------------------------------------------------
################################################################################################################################

# Data<- analyseSetPerPlot
#
# ModelSummary <- summaryExot0
#
# Type <- "toestand"
#
# VariableName <- "v7_NVAutochtAbund"
#
# UseStrata <- TRUE
#
# Strata <- "OwnerType"


#' Berekening parameterschattingen voor toestand en verschil op basis van summary van een mixed-model gefit met glmer of lmer
#'
#' Langere functiebeschrijving
#' @param Data Analysedataset
#' @param VariableName naam van de responsvariabele  (als string)
#' @param UseStrata schatting per strata?
#' @param Strata naam van variable die strata definieert
#' @param GLMERModel  mixed-model gefit met glmer of lmer
#' @param Type 'verschil' of 'toestand'
#' @return
#' @importFrom
#' @examples


My.ParametersMB <- function (Data, VariableName, UseStrata = FALSE, Strata,  GLMERModel,Type)

{

  periode_t1 <- min(Data$Periode)

  periode_t2 <- max (Data$Periode)

  ModelSummary <- summary(GLMERModel)

  family_Type <- ModelSummary$family

  if (UseStrata){

    strataLevels <- levels(Data[,Strata])

  } else {

    strataLevels <- ""
    Strata =""

  }

  if (!Type %in% c("verschil","toestand")){

    call("Kies voor Type 'verschil' of 'toestand'")

  } else if (Type == "verschil") {

    if (!UseStrata){

      if (is.null(family_Type)){

        diff <- ModelSummary$coefficients[2,"Estimate"]

        diff.std.error <- ModelSummary$coefficients[2,"Std. Error"]

        diff.llci <- diff - 1.96 * diff.std.error

        diff.ulci <- diff + 1.96 * diff.std.error

      } else if (family_Type == "binomial" ){

        p1<-plogis(ModelSummary$coefficients[1,"Estimate"])

        p2<-plogis(ModelSummary$coefficients[1,"Estimate"] + ModelSummary$coefficients[2,"Estimate"])

        diff <-p2-p1

        diff.llci <-plogis(ModelSummary$coefficients[1,"Estimate"] + ModelSummary$coefficients[2,"Estimate"] - 1.96*ModelSummary$coefficients[2,"Std. Error"])-p1

        diff.ulci <-plogis(ModelSummary$coefficients[1,"Estimate"] + ModelSummary$coefficients[2,"Estimate"] + 1.96*ModelSummary$coefficients[2,"Std. Error"])-p1

      } else if (family_Type == "poisson" ){

        p1<-exp(ModelSummary$coefficients[1,"Estimate"])

        p2<-exp(ModelSummary$coefficients[1,"Estimate"] + ModelSummary$coefficients[2,"Estimate"])

        diff <-p2-p1

        diff.llci <- exp(ModelSummary$coefficients[1,"Estimate"] + ModelSummary$coefficients[2,"Estimate"] - 1.96*ModelSummary$coefficients[2,"Std. Error"])-p1

        diff.ulci <-exp(ModelSummary$coefficients[1,"Estimate"] + ModelSummary$coefficients[2,"Estimate"] + 1.96*ModelSummary$coefficients[2,"Std. Error"])-p1

      }

      output<-data.frame(Variable=VariableName,
                         Strata="",
                         StratumNaam="",
                         Periode_t1 = periode_t1,
                         Periode_t2 = periode_t2,
                         MinReeks_t1 = min(Data[Data$Periode==periode_t1,]$Reeks),
                         MaxReeks_t1 = max(Data[Data$Periode==periode_t1,]$Reeks),
                         MinYear_t1 = min(Data[Data$Periode==periode_t1,]$Year),
                         MaxYear_t1 = max(Data[Data$Periode==periode_t1,]$Year),
                         MinReeks_t2 = min(Data[Data$Periode==periode_t2,]$Reeks),
                         MaxReeks_t2 = max(Data[Data$Periode==periode_t2,]$Reeks),
                         MinYear_t2 = min(Data[Data$Periode==periode_t2,]$Year),
                         MaxYear_t2 = max(Data[Data$Periode==periode_t2,]$Year),
                         NbObservaties_t1 = length(Data[Data$Periode == periode_t1,]$IDPlots),
                         NbObservaties_t2 = length(Data[Data$Periode == periode_t2,]$IDPlots),
                         Verschil = diff,
                         Llci = diff.llci,
                         Ulci = diff.ulci
      )



    } else {  # einde schattingen zonder strata

      #contrastmatrix

      nbStrata <-length(strataLevels)
      contr <- NULL

      v <- c(-1,1)
      g <- c(0,0)

      for (i in 1:nbStrata){

        contrRow <- c(rep(g,i-1),v,rep(g,nbStrata-i))
        contr <- rbind (contr, contrRow)
      }

      rownames(contr) <- strataLevels

      M.contr <-glht(GLMERModel, linfct =  contr)

      ModelSummaryContr <- summary(M.contr)

      output <- NULL

      for (l in 1:length(strataLevels)){

        if (is.null(family_Type)){
          diff <- ModelSummaryContr$test$coefficients[l]

          diff.llci <- diff - 1.96 * ModelSummaryContr$test$sigma[l]

          diff.ulci <- diff + 1.96 * ModelSummaryContr$test$sigma[l]

        } else if (family_Type=="poisson"){

          p1<- exp(ModelSummary$coefficients[1+(l-1)*2,"Estimate"])

          p2 <- exp(ModelSummary$coefficients[1+(l-1)*2,"Estimate"] + ModelSummaryContr$test$coefficients[l])

          diff <- p2 - p1

          diff.llci <- exp(ModelSummary$coefficients[1+(l-1)*2,"Estimate"] + ModelSummaryContr$test$coefficients[l] - 1.96 * ModelSummaryContr$test$sigma[l]) - p1

          diff.ulci <- exp(ModelSummary$coefficients[1+(l-1)*2,"Estimate"] + ModelSummaryContr$test$coefficients[l] + 1.96 * ModelSummaryContr$test$sigma[l]) - p1

        } else if (family_Type=="binomial"){

          p1<- plogis(ModelSummary$coefficients[1+(l-1)*2,"Estimate"])

          p2 <- plogis(ModelSummary$coefficients[1+(l-1)*2,"Estimate"] + ModelSummaryContr$test$coefficients[l])

          diff <- p2 - p1

          diff.llci <- plogis(ModelSummary$coefficients[1+(l-1)*2,"Estimate"] + ModelSummaryContr$test$coefficients[l] - 1.96 * ModelSummaryContr$test$sigma[l]) - p1

          diff.ulci <- plogis(ModelSummary$coefficients[1+(l-1)*2,"Estimate"] + ModelSummaryContr$test$coefficients[l] + 1.96 * ModelSummaryContr$test$sigma[l]) - p1

          }


        outputS<-data.frame(Variable=VariableName,
                           Strata = Strata,
                            StratumNaam=strataLevels[l],
                            Periode_t1 = periode_t1,
                           Periode_t2 = periode_t2,
                           MinReeks_t1 = min(Data[Data$Periode==periode_t1,]$Reeks),
                           MaxReeks_t1 = max(Data[Data$Periode==periode_t1,]$Reeks),
                           MinYear_t1 = min(Data[Data$Periode==periode_t1,]$Year),
                           MaxYear_t1 = max(Data[Data$Periode==periode_t1,]$Year),
                           MinReeks_t2 = min(Data[Data$Periode==periode_t2,]$Reeks),
                           MaxReeks_t2 = max(Data[Data$Periode==periode_t2,]$Reeks),
                           MinYear_t2 = min(Data[Data$Periode==periode_t2,]$Year),
                           MaxYear_t2 = max(Data[Data$Periode==periode_t2,]$Year),
                           NbObservaties_t1 = length(Data[Data$Periode == periode_t1,]$IDPlots),
                           NbObservaties_t2 = length(Data[Data$Periode == periode_t2,]$IDPlots),
                           Verschil = diff,
                           Llci = diff.llci,
                           Ulci = diff.ulci)

        output <- rbind (output,outputS)


      } # einde output per stratum

    } # einde schatting per stratum

  } else if (Type == "toestand") {

    output <- NULL

    for (l in 1:length(strataLevels)){

      state1 <- ModelSummary$coefficients[(l-1)*2+1,"Estimate"]

      state1.std.error <- ModelSummary$coefficients[(l-1)*2+1,"Std. Error"]

      state1.llci <- state1 - 1.96 * state1.std.error

      state1.ulci <- state1 + 1.96 * state1.std.error

      state2 <- ModelSummary$coefficients[(l-1)*2+2,"Estimate"]

      state2.std.error <- ModelSummary$coefficients[(l-1)*2+2,"Std. Error"]

      state2.llci <- state2 - 1.96 * state2.std.error

      state2.ulci <- state2 + 1.96 * state2.std.error

      # herschalen indien nodig

      if (!is.null(family_Type)){

        state1 <- ifelse(family_Type=="binomial",plogis(state1),

                         ifelse(family_Type=="poisson",exp(state1), NA))

        state1.llci <- ifelse(family_Type=="binomial",plogis(state1.llci),

                              ifelse(family_Type=="poisson",exp(state1.llci), NA))

        state1.ulci <- ifelse(family_Type=="binomial",plogis(state1.ulci),

                              ifelse(family_Type=="poisson",exp(state1.ulci), NA))

        state2 <- ifelse(family_Type=="binomial",plogis(state2),

                         ifelse(family_Type=="poisson",exp(state2), NA))

        state2.llci <- ifelse(family_Type=="binomial",plogis(state2.llci),

                              ifelse(family_Type=="poisson",exp(state2.llci), NA))

        state2.ulci <- ifelse(family_Type=="binomial",plogis(state2.ulci),

                              ifelse(family_Type=="poisson",exp(state2.ulci), NA))

      }

        outputState1<-data.frame(Variable=VariableName,
                                 Strata= Strata,
                                 StratumNaam= strataLevels[l],
                                 Periode= periode_t1,
                                 MinReeks = min(Data[Data$Periode==periode_t1,]$Reeks),
                                 MaxReeks = max(Data[Data$Periode==periode_t1,]$Reeks),
                                 MinYear = min(Data[Data$Periode==periode_t1,]$Year),
                                 MaxYear = max(Data[Data$Periode==periode_t1,]$Year),
                                 NbObservaties = length(Data[Data$Periode == periode_t1,]$IDPlots),
                                 Mean=state1,
                                 Llci=state1.llci,
                                 Ulci=state1.ulci)

        outputState2<-data.frame(Variable=VariableName,
                                 Strata =Strata,
                                 StratumNaam= strataLevels[l],
                                 Periode= periode_t2,
                                 MinReeks = min(Data[Data$Periode==periode_t2,]$Reeks),
                                 MaxReeks = max(Data[Data$Periode==periode_t2,]$Reeks),
                                 MinYear = min(Data[Data$Periode==periode_t2,]$Year),
                                 MaxYear = max(Data[Data$Periode==periode_t2,]$Year),
                                 NbObservaties = length(Data[Data$Periode == periode_t2,]$IDPlots),
                                 Mean=state2,
                                 Llci=state2.llci,
                                 Ulci=state2.ulci)

        outputS <-rbind(outputState1,outputState2)

        output <- rbind (output, outputS)

    }


  }

  output

}




####----------------------------------------------------------------
#### Functie die grondvlak berekent en volume waarbij tarieven en aantal ingangen worden gespecifieerd. De berekeningen gebeuren op basis van een data.frame met de velden 'Perimeter_cm' en 'Height_m' (in geval van 2 ingangen)
####----------------------------------------------------------------

my.CalcVolBA<-function(treeMeasurements,tarieven,nIngang,varNamePerimeter="Perimeter_cm",varNameHeight="Height_m",varNameVolume="Volume",varNameBasalArea="BasalArea_m2",varNameDiameter="D"){

  #code soortnaam identiek voor verschillende periodes, maar soortnaam kan verschillen
  tarieven<-tarieven[,names(tarieven) != "Species"]

  trees<-merge(treeMeasurements,tarieven,by="IDTreeSp",all.x=TRUE)

  #grondvlak

  #Toon: trees$BasalArea_m2 <- ((trees[,varNamePerimeter]/pi/2/100)^2)*pi

  # A = pi * R^2  en C = 2 * pi * R

  #Hulpvariabelen
  Radius_m <- 1/100 * trees[,varNamePerimeter] / (2*pi)

  #Hulpvariabelen bewaard in dataset
  trees$BasalArea_m2 <- pi * Radius_m^2
  trees$D <- trees[,varNamePerimeter]/pi

  if (nIngang==2){

    trees$Volume <-
      ifelse( trees$Formule_type == 1,
              yes =
                trees$a + trees$b * trees[,varNamePerimeter] +
                trees$c *(trees[,varNamePerimeter]^2)+ trees$d *(trees[,varNamePerimeter]^3) +
                trees$e*trees[,varNameHeight] + trees$f*trees[,varNameHeight]* trees[,varNamePerimeter] +
                trees$g*trees[,varNameHeight]*(trees[,varNamePerimeter]^2),
              no =
                1/1000 *
                #spil
                (exp(1.10597 * log(trees[,varNameHeight]) + 1.78865 * log(trees$D) - 3.07192) -
                #Verlies
                exp(-4.608923 * log(trees$D) + 3.005989 * log(trees[,varNameHeight]) -
                       1.3209 * log(trees[,varNameHeight])*log(trees[,varNameHeight])+ 1.605266 * log(trees$D) * log(trees[,varNameHeight]) + 5.410272))
      )


  } else if (nIngang==1){
    trees$Volume<- trees$a + trees$b * trees[,varNamePerimeter] + trees$c *(trees[,varNamePerimeter]^2)+ trees$d *(trees[,varNamePerimeter]^3)
  } else {
    trees$Volume = NaN
  }

  trees<-trees[,!names(trees) %in% c("a","b","c","d","e","f","g","Formule_type","Tarief","groepNaam", "TariefID")]
  trees$Volume<-pmax(0,trees$Volume)
  trees<-plyr :: rename(trees,c(Volume=varNameVolume,BasalArea_m2=varNameBasalArea,D=varNameDiameter))
  trees
}




####----------------------------------------------------------------
#### Functie die volume van het (zwaar) kroonhout berekent, waarbij tarief wordt gespecifieerd.
# De berekeningen gebeuren op basis van een data.frame met het veld 'Perimeter_cm'
####----------------------------------------------------------------

my.CalcVolBranches<-function(treeMeasurements,tarieven,varNamePerimeter="Perimeter_cm",varNameVolume="VolumeKroon"){

  #code soortnaam identiek voor verschillende periodes, maar soortnaam kan verschillen
  tarieven<-tarieven[,names(tarieven) != "Species"]

  trees<-merge(treeMeasurements,tarieven,by="IDTreeSp",all.x=TRUE)

  #Hulpvariabelen
  Radius_m <- 1/100 * trees[,varNamePerimeter] / (2*pi)

  #Hulpvariabelen bewaard in dataset
  trees$VolumeKroon<- trees$a + trees$b * trees[,varNamePerimeter] + trees$c *(trees[,varNamePerimeter]^2)+ trees$d *(trees[,varNamePerimeter]^3)

  trees<-trees[,!names(trees) %in% c("a","b","c","d","Tarief","groepNaam", "TariefID")]
  trees$VolumeKroon<-pmax(0,trees$VolumeKroon)
  trees<-plyr :: rename(trees,c(VolumeKroon=varNameVolume))
  trees
}


# results<- results.toestand
# description <- "test"
# type <-"M"
# tblName <- "test"


###------------------------------------------------
### Resultaten wegschrijven naar resultatendatabank
###-----------------------------------------------

#' Resultaten wegschrijven naar resultatendatabank
#'
#' Langere functiebeschrijving
#' @param results Resultaten van de functie My.WgtParEstimation
#' @param dbHandle Pad naar databank waarin resultaten worden opgeslagen, default gelijk aan "dbResultaten_path" (RODBC-connectie in functie zelf)
#' @param tblName Naam van tabel waarin resultaten worden opgeslagen, default gelijk aan "tblResultaten"
#' @param scriptName Naam van script
#' @param scriptLocation Naam van folder waar script opgeslagen is (vaste lijst)
#' @param type "D" voor design-based analyse en "M" voor model-based analyse
#' @param description Beschrijving van de analyse
#' @param forestedge "met randplots", "zonder randplots" of "enkel randplots" (vaste lijst)
#' @param datasource versie van de gebruikte analysedb bv. "VBI_Analysedatabank_v2019-11-07.accdb"
#' @param datasource_hash file-hash die aangemaakt wordt in het script om te verwijzen naar gebruikte analysedb
#' @param stratasource versie van de gebruikte dbStrata bv. "VBI_Strata_v2019-11-07.accdb"
#' @param stratasource_hash file-hash die aangemaakt wordt in het script om te verwijzen naar gebruikte dbStrata
#' @param externalsource versie van de gebruikte dbExterneData bv. "VBIExterneData_v2022-08-23.accdb"
#' @param externalsource_hash file-hash die aangemaakt wordt in het script om te verwijzen naar gebruikte dbExterneData
#' @param meetprocessource versie van de gebruikte dbMeetproces bv. "VBI_Meetproces_v2021-11-01.accdb"
#' @param meetprocessource_hash file-hash die aangemaakt wordt in het script om te verwijzen naar gebruikte dbMeetproces
#' @param request_from verwijzing naar project waarbinnen data-aanvraag kadert, standaard leeg (bv. NARA2020)
#' @param run_by geeft aan door wie analyse gerund is, en of het al dan niet een test-run betreft (bv. "run_AL", "run_LG", "test", "test_AL", "test_LG", ....)
#' @return
#' @importFrom
#' @examples

My.ResultsToDatabase <-function (results, 
                                 dbHandle = dbResultaten_path, 
                                 tblName ="tblResultaten", 
                                 type, 
                                 scriptName=NULL, 
                                 #scriptLocation=c("1KenmBosOpp", "2Verjonging", "3DendroKwant", "4Vegetatie", "5Indices", "6DoodHout", "Temp", "AanvraagGegevens"), 
                                 scriptLocation=NULL,
                                 description, 
                                 forestedge = c("met randplots", "zonder randplots", "enkel randplots"), 
                                 datasource=NULL, 
                                 datasource_hash=NULL, 
                                 stratasource=NULL,
                                 stratasource_hash=NULL,
                                 externalsource=NULL,
                                 externalsource_hash=NULL,
                                 meetprocessource=NULL,
                                 meetprocessource_hash=NULL,
                                 request_from=NULL, 
                                 run_by=c(NA, "run_AL", "run_LG", "run_TW", "test", "test_AL", "test_LG", "test_TW"))
{
  
  stateOrTrend <- ifelse ("Periode_t1" %in% colnames (results),"verschil","toestand")
      # forestedge <- ifelse(is.na(forestedge), "met randplots", forestedge)   
      # overbodig: als parameter niet ingevuld wordt, wordt eerste waarde van de lijst genomen
  
  forestedge <- match.arg(forestedge)
  #scriptLocation <- match.arg(scriptLocation)
  run_by <- match.arg(run_by)
  
  # als run_by niet gespecifieerd is --> IP-adres nemen
  
  if(is.na(run_by)){
    x <- system("ipconfig", intern=TRUE)
    z <- x[grep("IPv4", x)]
    IP <- gsub(".*? ([[:digit:]])", "\\1", z)
    run_by <- IP
    warning('!! WARNING: "Run" werd niet gespecifieerd --> Run_by = IP-adres')
  }  else { #als run_by wél gespecifieerd is 
    run_by <- run_by
  }
  
  
  if (type %in% c("D","M")){
    
    if (type=="D"){
      
      # temp: om fctie te testen
      # results <- Resultaat
      # type <- "D"
      # description <- "test"
      # zonderRandplots <- "ja"
      # stateOrTrend <- "toestand"
      # tblName <-"tblResultaten"
      
      if((nrow(Resultaat) %% 2) == 0) {
        results <- results %>% arrange(periode)
        print(paste("aantal weg te schrijven records is even"))
              results$maxYear <- c(rep(as.factor(maxyear1), nrow(Resultaat)/2), rep(as.factor(maxyear2), nrow(Resultaat)/2))
              results$minYear <- c(rep(as.factor(minyear1), nrow(Resultaat)/2), rep(as.factor(minyear2), nrow(Resultaat)/2))
        
      } else {
        print(paste("aantal weg te schrijven records is oneven, zorg voor aangepast minyear, maxyear"))
              results$maxYear <- c(rep(as.factor(maxyear1), nrow(Resultaat)/1))
              results$minYear <- c(rep(as.factor(minyear1), nrow(Resultaat)/1))
              
      }
      
      
      tblResults <-data.frame(variabele=results$variabele,
                              analyseType = type,
                              scriptNaam = scriptName,
                              scriptLocatie = scriptLocation,
                              datum = date(),
                              beschrijving = description,
                              bosrand = forestedge,
                              strata=results$strata,
                              stratumNaam = results$stratumNaam,
                              periode = as.factor(results$periode),
                              minJaar = as.factor(results$minYear),
                              maxJaar = as.factor(results$maxYear),
                              minReeks = as.factor(results$minReeks),
                              maxReeks = as.factor(results$maxReeks),
                              nbObservaties = results$nbObservaties,
                              gemiddelde = round(results$wgt.mean,4),
                              variantie=round(results$wgt.var,4),
                              BI_ondergrens=round(results$llci,4),
                              BI_bovengrens=round(results$ulci,4),
                              brondata = datasource,
                              brondataHash = datasource_hash, 
                              bronstrata = stratasource,
                              bronstrataHash = stratasource_hash,
                              bronextern = externalsource,
                              bronexternHash = externalsource_hash,
                              bronmeetproces= meetprocessource,
                              bronmeetprocesHash = meetprocessource_hash,
                              aanvraag = request_from,
                              run = run_by)
      
      
    } else if (type=="M"){
      
      if (stateOrTrend == "verschil"){
        results$MaxYear_t2 <- maxyear2
        results$MinYear_t2 <- minyear2
        results$MaxYear_t1 <- maxyear1
        results$MinYear_t1 <- minyear1
        
        tblResults <-data.frame(variabele=results$Variable,
                                analyseType=type,
                                scriptNaam = scriptName,
                                scriptLocatie = scriptLocation,
                                datum=date(),
                                beschrijving= description,
                                bosrand = forestedge,
                                strata=results$Strata,
                                stratumNaam=results$StratumNaam,
                                periode_t1 = as.factor(results$Periode_t1),
                                minJaar_t1 = as.factor(results$MinYear_t1),
                                maxJaar_t1 = as.factor(results$MaxYear_t1),
                                minReeks_t1 = as.factor(results$MinReeks_t1),
                                maxReeks_t1 = as.factor(results$MaxReeks_t1),
                                nbObservaties_t1 = results$NbObservaties_t1,
                                periode_t2 = as.factor(results$Periode_t2),
                                minJaar_t2 = as.factor(results$MinYear_t2),
                                maxJaar_t2 = as.factor(results$MaxYear_t2),
                                minReeks_t2 = as.factor(results$MinReeks_t2),
                                maxReeks_t2 = as.factor(results$MaxReeks_t2),
                                nbObservaties_t2 = results$NbObservaties_t2,
                                verschil=round(results$Verschil,4),
                                variantie=NA,
                                BI_ondergrens=round(results$Llci,4),
                                BI_bovengrens=round(results$Ulci,4),
                                brondata = datasource,
                                brondataHash = datasource_hash,
                                bronstrata = stratasource,
                                bronstrataHash = stratasource_hash,
                                bronextern = externalsource,
                                bronexternHash = externalsource_hash,
                                bronmeetproces= meetprocessource,
                                bronmeetprocesHash = meetprocessource_hash,
                                aanvraag = request_from,
                                run = run_by)
        
        
      } else if (stateOrTrend == "toestand") {
        

        if((nrow(Resultaat) %% 2) == 0) {
          results <- results %>% arrange(periode)
          print(paste("aantal weg te schrijven records is even"))
          results$MaxYear <- c(rep(as.factor(maxyear1), nrow(Resultaat)/2), rep(as.factor(maxyear2), nrow(Resultaat)/2))
          results$MinYear <- c(rep(as.factor(minyear1), nrow(Resultaat)/2), rep(as.factor(minyear2), nrow(Resultaat)/2))
          
        } else {
          print(paste("aantal weg te schrijven records is oneven, zorg voor aangepast minyear, maxyear"))
          results$MaxYear <- c(rep(as.factor(maxyear1), nrow(Resultaat)/1))
          results$MinYear <- c(rep(as.factor(minyear1), nrow(Resultaat)/1))
          
        }
        
        tblResults <-data.frame(variabele=results$Variable,
                                analyseType=type,
                                scriptNaam = scriptName,
                                scriptLocatie = scriptLocation,
                                datum=date(),
                                beschrijving= description,
                                bosrand = forestedge,
                                strata=results$Strata,
                                stratumNaam=results$StratumNaam,
                                periode = as.factor(results$Periode),
                                minJaar=as.factor(results$MinYear),
                                maxJaar= as.factor(results$MaxYear),
                                minReeks = as.factor(results$MinReeks),
                                maxReeks = as.factor(results$MaxReeks),
                                nbObservaties= results$NbObservaties,
                                gemiddelde=round(results$Mean,4),
                                variantie=NA,
                                BI_ondergrens=round(results$Llci,4),
                                BI_bovengrens=round(results$Ulci,4),
                                brondata = datasource,
                                brondataHash = datasource_hash,
                                bronstrata = stratasource,
                                bronstrataHash = stratasource_hash,
                                bronextern = externalsource,
                                bronexternHash = externalsource_hash,
                                bronmeetproces= meetprocessource,
                                bronmeetprocesHash = meetprocessource_hash,
                                aanvraag = request_from,
                                run = run_by)
        
      }
    }
    
    tblName <-  paste(tblName,stateOrTrend,sep = "_")
    
    connectieResultaten <- odbcConnectAccess2007(dbHandle) 
    
    listTbl<-sqlTables(connectieResultaten)
    
    # als tblResultaten nog niet is aangemaakt --> nieuwe tabel aanmaken
    if(!tblName %in% listTbl$TABLE_NAME){
      
      tblData <- sqlSave(connectieResultaten, tblResults, tblName)
      
    }  else { #als tabel bestaat records toevoegen aan tabel
      
      tblData <- sqlSave(connectieResultaten,tblResults, tblName,append = TRUE)
      
    }
    
    odbcClose(connectieResultaten)
    
  } else {
    cat("Vul een D of een M in voor type analyse (D = design-based en M = model-based)  ")
  }
  
}

####-------------------------------------------------------------------------------
#### Functie om proportie te berekenen van gemiddelde van bepaalde variabele (bv. grondvlak) voor een soort t.o.v. het gemiddelde van die variabele voor alle soorten (komt overeen met proportie tussen totaal per soort en totaal over alle soorten)
####------------------------------------------------------------------------------

#' Proportie berekenen van totaal per soort en totaal over alle soorten (bv. grondvlak)
#'
#' Langere functiebeschrijving
#' @param data invoer
#' @param spName soortnaam
#' @param variableName grondvlak of volume of ....
#' @param maxReeks
#' @return
#' @importFrom
#' @examples


propMeasure_Species <- function(data, spName, variableName, maxReeks){

  data$SpeciesCalc <- spName

  data$Measure <- data[,variableName]

  species_Measure <- ddply(data,.(IDPlots,Periode,Reeks,Year,Weight),summarise,
                      Measure_species = sum( Measure* (NameNl == SpeciesCalc)  ),
                      Measure_allSpecies = sum(Measure))

  # species_BA <- ddply(data,.(IDPlots,Periode,Reeks,Year,Weight),summarise,
  #                     BasalArea_ha_species = sum(v2_BasalArea_ha * (NameNl == SpeciesCalc)  ),
  #                     BasalArea_ha_allSpecies = sum(v2_BasalArea_ha))

  result.species <- rbind(My.WgtParEstimation(species_Measure,  "Measure_species", Periode = 2,MaxReeks = maxReeks),
                  My.WgtParEstimation(species_Measure,  "Measure_species", Periode = 1,MaxReeks = maxReeks))

  result.species$strata <- "treeSpecies"
  result.species$stratumNaam <- spName
  result.species$variabele <- variableName


  result.allSpecies <- rbind( My.WgtParEstimation(species_Measure,  "Measure_allSpecies", Periode = 2,MaxReeks = maxReeks),
                              My.WgtParEstimation(species_Measure,  "Measure_allSpecies", Periode = 1,MaxReeks = maxReeks))

  result.prop <- data.frame(variabele = paste("Prop_",variableName,sep=""),
                            strata = "treeSpecies",
                            stratumNaam = spName,
                            periode = result.species$periode,
                            minYear = result.species$minYear,
                            maxYear = result.species$maxYear,
                            minReeks = result.species$minReeks,
                            maxReeks = result.species$maxReeks,
                            nbObservaties = result.species$nbObservaties
                            )

  result.prop$wgt.mean <- result.species$wgt.mean/result.allSpecies$wgt.mean

  # f = x/y   --> (sigma_f/f)^2 =(sigma_x/x)^2  + (sigma_y/y)^2 - 2cov_xy/x/y

  deel1 <- result.species$wgt.var/(result.species$wgt.mean)^2
  deel2 <- result.allSpecies$wgt.var/(result.allSpecies$wgt.mean)^2

  cov_periode1 <- cov(species_Measure[species_Measure$Periode==1,]$Measure_species,species_Measure[species_Measure$Periode==1,]$Measure_allSpecies)
  cov_periode2 <- cov(species_Measure[species_Measure$Periode==2,]$Measure_species,species_Measure[species_Measure$Periode==2,]$Measure_allSpecies)

  deel3 <- 2* c(cov_periode2,cov_periode1)/result.species$wgt.mean/result.allSpecies$wgt.mean

  # sigma_f = ((deel1 + deel2)^0.5) * f

  result.prop$wgt.var <- (deel1 + deel2 - deel3) * (result.prop$wgt.mean)^2

  result.prop$wgt.var <- ifelse(result.prop$wgt.var > 0, result.prop$wgt.var, result.species$wgt.var/((result.allSpecies$wgt.mean)^2))

  #test.wgt.var <- result.species$wgt.var/result.allSpecies$wgt.mean^2

  result.prop$wgt.mean <- result.prop$wgt.mean*100

  result.prop$wgt.var <- result.prop$wgt.var*10000

  n <- c(sum(species_Measure[species_Measure$Periode==2,]$Weight),sum(species_Measure[species_Measure$Periode==1 & species_Measure$Reeks <=5 ,]$Weight))

  result.prop$llci <- round(pmax(result.prop$wgt.mean - 1.96*((result.prop$wgt.var)^0.5)/(n^0.5),0),2)
  result.prop$ulci <- round(result.prop$wgt.mean + 1.96*((result.prop$wgt.var)^0.5)/(n^0.5),2)

  result.prop$wgt.mean <- round(result.prop$wgt.mean,2)

  result.species$wgt.mean <- round(result.species$wgt.mean,2)
  result.species$llci <- round(result.species$llci,2)
  result.species$ulci <- round(result.species$ulci,2)
  result <- rbind(result.species,result.prop)

  return(result)

  }



####--------------------------------------------------------------------------------------------------------
#### Functie om proportie te berekenen van gemiddelde van bepaalde variabele (bv. grondvlak) voor een diameterklasse t.o.v. het gemiddelde van die variabele voor alle diameterklasses (komt overeen met proportie tussen totaal per diameterklasse en totaal over alle klasses)
####-------------------------------------------------------------------------------------------------------

#' Proportie berekenen van totaal per diameterklasse en totaal over alle diameterklasses (bv. grondvlak)
#'
#' Langere functiebeschrijving
#' @param data invoer
#' @param diamclassName naam van diameterklasse
#' @param variableName grondvlak of volume of aantal stammen
#' @param maxReeks
#' @return
#' @importFrom
#' @examples

propMeasure_DiamClass <- function(data, diamclassName, variableName, maxReeks){

  data$DiamCalc <- diamclassName   # diameterklasse waar je berekening voor doet

  data$Measure <- data[,variableName]    #welke meting: N, G of V?

  diam_Measure <- plyr::ddply(data,.(IDPlots,Periode,Reeks,Year,Weight),summarise,
                      Meausure_diam = sum(Measure*(v6_DiamClass == DiamCalc)),
                      Measure_allDiamClasses = sum(Measure))

  # species_BA <- ddply(data,.(IDPlots,Periode,Reeks,Year,Weight),summarise,
  #                     BasalArea_ha_species = sum(v2_BasalArea_ha * (v6_DiamClass == DiamCalc)  ),
  #                     BasalArea_ha_allSpecies = sum(v2_BasalArea_ha))

  result.diamclass <- rbind(My.WgtParEstimation(diam_Measure,  "Meausure_diam", Periode = 2,MaxReeks = maxReeks),
                  My.WgtParEstimation(diam_Measure,  "Meausure_diam", Periode = 1,MaxReeks = maxReeks))

  result.diamclass$strata <- "DiamClass"   # was initieel "treeSpecies" ; zelf gekozen naam om in output te verschijnen
  result.diamclass$stratumNaam <- diamclassName
  result.diamclass$variabele <- variableName


  result.allClasses <- rbind( My.WgtParEstimation(diam_Measure,  "Measure_allDiamClasses", Periode = 2,MaxReeks = maxReeks),
                              My.WgtParEstimation(diam_Measure,  "Measure_allDiamClasses", Periode = 1,MaxReeks = maxReeks))

  result.prop <- data.frame(variabele = paste("Prop_",variableName,sep=""),
                            strata = "DiamClass",
                            stratumNaam = diamclassName,
                            periode = result.diamclass$periode,
                            minYear = result.diamclass$minYear,
                            maxYear = result.diamclass$maxYear,
                            minReeks = result.diamclass$minReeks,
                            maxReeks = result.diamclass$maxReeks,
                            nbObservaties = result.diamclass$nbObservaties
                            )

  result.prop$wgt.mean <- result.diamclass$wgt.mean/result.allClasses$wgt.mean

  # f = x/y   --> (sigma_f/f)^2 =(sigma_x/x)^2  + (sigma_y/y)^2 - 2cov_xy/x/y

  deel1 <- result.diamclass$wgt.var/(result.diamclass$wgt.mean)^2
  deel2 <- result.allClasses$wgt.var/(result.allClasses$wgt.mean)^2

  cov_periode1 <- cov(diam_Measure[diam_Measure$Periode==1,]$Meausure_diam,diam_Measure[diam_Measure$Periode==1,]$Measure_allDiamClasses)
  cov_periode2 <- cov(diam_Measure[diam_Measure$Periode==2,]$Meausure_diam,diam_Measure[diam_Measure$Periode==2,]$Measure_allDiamClasses)

  deel3 <- 2* c(cov_periode2,cov_periode1)/result.diamclass$wgt.mean/result.allClasses$wgt.mean

  # sigma_f = ((deel1 + deel2)^0.5) * f

  result.prop$wgt.var <- (deel1 + deel2 - deel3) * (result.prop$wgt.mean)^2

  result.prop$wgt.var <- ifelse(result.prop$wgt.var > 0, result.prop$wgt.var, result.diamclass$wgt.var/((result.allClasses$wgt.mean)^2))

  #test.wgt.var <- result.diamclass$wgt.var/result.allClasses$wgt.mean^2

  result.prop$wgt.mean <- result.prop$wgt.mean*100

  result.prop$wgt.var <- result.prop$wgt.var*10000

  n <- c(sum(diam_Measure[diam_Measure$Periode==2,]$Weight),sum(diam_Measure[diam_Measure$Periode==1 & diam_Measure$Reeks <=5 ,]$Weight))

  result.prop$llci <- round(pmax(result.prop$wgt.mean - 1.96*((result.prop$wgt.var)^0.5)/(n^0.5),0),2)
  result.prop$ulci <- round(result.prop$wgt.mean + 1.96*((result.prop$wgt.var)^0.5)/(n^0.5),2)

  result.prop$wgt.mean <- round(result.prop$wgt.mean,2)

  result.diamclass$wgt.mean <- round(result.diamclass$wgt.mean,2)
  result.diamclass$llci <- round(result.diamclass$llci,2)
  result.diamclass$ulci <- round(result.diamclass$ulci,2)
  result <- rbind(result.diamclass,result.prop)

  return(result)

  }


####-------------------------------------------------------------------------------
#### Functie om proportie te berekenen van gemiddelde van bepaalde variabele (bv. grondvlak) voor NH vs LH t.o.v. het gemiddelde van die variabele voor alle bomen (komt overeen met proportie tussen totaal per NH/LH en totaal over alle bomen)
####------------------------------------------------------------------------------

#' Proportie berekenen van totaal per NH/LH en totaal over alle soorten (bv. grondvlak)
#'
#' Langere functiebeschrijving
#' @param data invoer
#' @param LH_NH onderscheid loofhout-naaldhout
#' @param variableName grondvlak of volume of ....
#' @param maxReeks
#' @return
#' @importFrom
#' @examples


propMeasure_LHNH <- function(data, LHvsNH, variableName, maxReeks){

  data$SpeciesCalc <- LHvsNH

  data$Measure <- data[,variableName]

  species_Measure <- ddply(data,.(IDPlots,Periode,Reeks,Year,Weight),summarise,
                           Measure_species = sum( Measure* (LH_NH == SpeciesCalc)  ),
                           Measure_allSpecies = sum(Measure))

  # species_BA <- ddply(data,.(IDPlots,Periode,Reeks,Year,Weight),summarise,
  #                     BasalArea_ha_species = sum(v2_BasalArea_ha * (LH_NH == SpeciesCalc)  ),
  #                     BasalArea_ha_allSpecies = sum(v2_BasalArea_ha))

  result.species <- rbind(My.WgtParEstimation(species_Measure,  "Measure_species", Periode = 2,MaxReeks = maxReeks),
                          My.WgtParEstimation(species_Measure,  "Measure_species", Periode = 1,MaxReeks = maxReeks))

  result.species$strata <- "LHvsNH"
  result.species$stratumNaam <- LHvsNH
  result.species$variabele <- variableName


  result.allSpecies <- rbind( My.WgtParEstimation(species_Measure,  "Measure_allSpecies", Periode = 2,MaxReeks = maxReeks),
                              My.WgtParEstimation(species_Measure,  "Measure_allSpecies", Periode = 1,MaxReeks = maxReeks))

  result.prop <- data.frame(variabele = paste("Prop_",variableName,sep=""),
                            strata = "LHvsNH",
                            stratumNaam = LHvsNH,
                            periode = result.species$periode,
                            minYear = result.species$minYear,
                            maxYear = result.species$maxYear,
                            minReeks = result.species$minReeks,
                            maxReeks = result.species$maxReeks,
                            nbObservaties = result.species$nbObservaties
  )

  result.prop$wgt.mean <- result.species$wgt.mean/result.allSpecies$wgt.mean

  # f = x/y   --> (sigma_f/f)^2 =(sigma_x/x)^2  + (sigma_y/y)^2 - 2cov_xy/x/y

  deel1 <- result.species$wgt.var/(result.species$wgt.mean)^2
  deel2 <- result.allSpecies$wgt.var/(result.allSpecies$wgt.mean)^2

  cov_periode1 <- cov(species_Measure[species_Measure$Periode==1,]$Measure_species,species_Measure[species_Measure$Periode==1,]$Measure_allSpecies)
  cov_periode2 <- cov(species_Measure[species_Measure$Periode==2,]$Measure_species,species_Measure[species_Measure$Periode==2,]$Measure_allSpecies)

  deel3 <- 2* c(cov_periode2,cov_periode1)/result.species$wgt.mean/result.allSpecies$wgt.mean

  # sigma_f = ((deel1 + deel2)^0.5) * f

  result.prop$wgt.var <- (deel1 + deel2 - deel3) * (result.prop$wgt.mean)^2

  result.prop$wgt.var <- ifelse(result.prop$wgt.var > 0, result.prop$wgt.var, result.species$wgt.var/((result.allSpecies$wgt.mean)^2))

  #test.wgt.var <- result.species$wgt.var/result.allSpecies$wgt.mean^2

  result.prop$wgt.mean <- result.prop$wgt.mean*100

  result.prop$wgt.var <- result.prop$wgt.var*10000

  n <- c(sum(species_Measure[species_Measure$Periode==2,]$Weight),sum(species_Measure[species_Measure$Periode==1 & species_Measure$Reeks <=5 ,]$Weight))

  result.prop$llci <- round(pmax(result.prop$wgt.mean - 1.96*((result.prop$wgt.var)^0.5)/(n^0.5),0),2)
  result.prop$ulci <- round(result.prop$wgt.mean + 1.96*((result.prop$wgt.var)^0.5)/(n^0.5),2)

  result.prop$wgt.mean <- round(result.prop$wgt.mean,2)

  result.species$wgt.mean <- round(result.species$wgt.mean,2)
  result.species$llci <- round(result.species$llci,2)
  result.species$ulci <- round(result.species$ulci,2)
  result <- rbind(result.species,result.prop)

  return(result)

}




###----------------------------------------------
### Bostypologie ----
###-----------------------------------------------

#
# #Gebruikte formules in het programma bostypologie
# #--------------------------------------------------
#
#
#
# #' Formule 1: Presentie van een soort in een bostype
# #'
# #' @param n_ak aantal opnamen met soort k behorend tot type a
# #' @param n aantal opnamen behoren tot type a
# #'
# #' @return
# #' @export
# #'
# #' @examples
# P_ak <- function(n_ak, n_a){
#   n_ak / n_a * 100
# }
#
# #-------------------------------------------------------------
#
#
# #' Formule 2: Karakteristieke bedekkng
# #'
# #' @param B_ak vector met de bedekkingen van soort k in de opnames j behorend tot type a
# #' @param n_ak aantal opnamen met soort k behorend tot type a
# #'
# #' @return
# #' @export
# #'
# #' @examples
# KB_ak <- function(B_ak, n_ak){
#   sum(B_ak) / n_ak
# }
#
# #-------------------------------------------------------------
#
# #' Formule 4: Algemene presentie van een soort k in de hele dataset
# #'
# #' @param n_k aantal opnamen met soort k in de hele dataset
# #' @param n tottal aantal opnamen in de hele dataset
# #'
# #' @return
# #' @export
# #'
# #' @examples
# P_k <- function(n_k, n){
#   nk / n * 100
# }
#
# #-------------------------------------------------------------
#
# #' Formule 3: Trouw van een soort. De mate waarin een soort gebonden is aan een bostype.
# #'
# #' @param P_ak Presentie van een soort in een bostype (formule 1)
# #' @param P_k Algemene presentie van een soort in de dataset (formule 4)
# #'
# #' @return
# #' @export
# #'
# #' @examples
# T_ak <- function(P_ak, P_k){
#   P_ak / P_k
# }
#
# #-------------------------------------------------------------
#
# #' Formule 5: Indicatorwaarde van een soort voor een bepaalde gemeenschap (Dufrêne & Legendre, 1997)
# #'
# #' @param Pak Presentie van een soort in een bostype (formule 1)
# #' @param Pk mag leeg zijn als Tak ingevuld is. Algemene presentie van een soort in de dataset (formule 4).
# #' @param Tak mag leeg zijn als Pk ingevuld is. Trouw van een soort aan een bostype (formule 3)
# #'
# #' @return
# #' @export
# #'
# #' @examples
# IndVal_ak <- function(P_ak, P_k, T_ak){
#   if (!missing(Pk)){
#     rv <- P_ak^2 / (P_k * 100)
#   } else {
#     rv <- T_ak * P_ak / 100
#   }
#
#   rv
# }
#
# #-------------------------------------------------------------
#
# #' Formule 6: Normalisatie van de IndValwaarden
# #'
# #' @param IndVal_ak vector met indicatorwaarde van de soorten voor een bepaalde gemeenschap (formule 5)
# #'
# #' @return vector
# #' @export vector met genormaliseerde IndValwaarden
# #'
# #' @examples
# v_ak <- function (IndVal_ak){
#   IndVal_ak / sum(IndVal_ak)
# }
#
# #-------------------------------------------------------------
#
# #' Formule 7: Verwantschapscore voor opname j voor gemeenschap a (niet-gewogen)
# #'
# #' @param v_ak vector met genormaliseerde indvalwaarden voor alle soorten in een bepaalde opname (formule 6)
# #'
# #' @return
# #' @export
# #'
# #' @examples
# S_aj_simple <- function(v_ak){
#   sum(v_ak)
# }
#
# #-------------------------------------------------------------
#
# #' Formule 9: Wegingsfactoren
# #'
# #' @param B_jk Bedekking van soort k in opname j
# #' @param KB_ak karaktereistiek ebedekking van soort k in gemeenschap a
# #'
# #' @return
# #' @export
# #'
# #' @examples
# w_ajk <- function(B_jk, KB_ak){
#   sqrt(pmin(B_jk, KB_ak)/pmax(B_jk, KB_ak))
# }
#
# #-------------------------------------------------------------
#
# #' Formule 8: Gewogen verwantschapscore voor opname j voor gemeenschap a
# #'
# #' @param v_ak v_ak vector met genormaliseerde indvalwaarden voor alle soorten in een bepaalde opname (formule 6)
# #' @param w_ajk vector met wegingsfactoren (formule 9)
# #'
# #' @return
# #' @export
# #'
# #' @examples
# S_aj <- function(v_ak, w_ajk){
#   sum(v_ak * w_ajk)
# }


#------------------------------------------------------------


berekenIndValConfig <- function(Opnames, TypologieCode = "T1"){
  require("plyr")

  #hulpfunctie om eigenschappen van de data los van de gemeenschap te berekenen
  Calc0 <- function(dat0){
    rv0 <- ddply(dat0, .(SPECIES_NR), summarize, n_k = sum(BEDEKKING>0))
    rv0$n <- length(unique(dat0$OPNAMECODE))
    rv0$P_k <- rv0$n_k / rv0$n * 100
    rv0
  }

  #hulpfunctie om eigenschappen van de gemeenschappen te berekenen
  Calc1 <- function(dat){
    n_a <-  length(unique(dat$OPNAMECODE[dat$BEDEKKING > 0]))
    Res <- ddply(dat, .(SPECIES_NR), .fun = function(dat2){
      n_ak <- sum(dat2$BEDEKKING > 0)
      KB_ak <- sum(dat2$BEDEKKING) / n_ak
      P_k <- dat2$P_k[1]
      n <- dat2$n[1]
      n_k <- dat2$n_k[1]
      data.frame(n_ak, KB_ak, P_k, n, n_k)
    })
    Res$n_a <- n_a
    Res$P_ak <- Res$n_ak / Res$n_a * 100
    Res$T_ak <- Res$P_ak / Res$P_k
    Res$IndVal_ak <- Res$T_ak * Res$P_ak / 100
    Res$Freq <- Res$P_ak / 100
    Res$KarBed <- Res$KB_ak
    Res$v_ak <- Res$IndVal_ak / sum(Res$IndVal_ak)
    Res$IndVal <- Res$v_ak
    Res[c("SPECIES_NR","P_ak","T_ak", "KB_ak", "IndVal_ak", "v_ak", "n_ak", "n_a", "n", "n_k","P_k", "Freq", "KarBed", "IndVal")]
  }

  #Breidt de dataset uit met de eigenschappen die gelden over de gemeenschappen heen
  Opnames_Extended <- merge(Opnames, ddply(Opnames, NULL, Calc0), all.x = TRUE, by = "SPECIES_NR")

  #Geeft een score per soort per gemeenschap terug om een gemeenschap te karakteriseren
  rv <- ddply(Opnames_Extended, .(BostypeCode), .fun = Calc1)
  rv$TypologieCode <- TypologieCode
  rv
}





plotVerwantschap <- function(x, repeat_first = plot, size = 2){
  x <- na.omit(x)
  if (length(unique(x$OPNAMECODE))> 1) {
    stop("plot moet voor ieder opnamenummer apart gemaakt worden (eventueel via een for-lus)")
  }
  makeCircle <- function(rad, num){
    cko <- data.frame(rad = rad, x = c(seq(-rad,rad, length = num),
                                       seq(rad, -rad, length = num)),
                      sign = rep(c(1,-1), rep(num,2)))
    cko$y <- sqrt(cko$rad**2 - cko$x**2) * cko$sign
    cko
  }

  rv <- x
  plotdata <- x
  limits <- max(abs(range(plotdata$S_aj)))
  breaks <- pretty(limits, min.n = 4)

  plotdata$radius <- plotdata$S_aj
  plotdata$radiuslab <- limits
  plotdata$corner <- tail(seq(0, 2*pi, length = (nrow(plotdata)+1)/length(unique(plotdata$OPNAMECODE))), -1)
  plotdata$x <- plotdata$radius * cos(plotdata$corner)
  plotdata$y <- plotdata$radius * sin(plotdata$corner)
  plotdata$xlabel <- plotdata$radiuslab * cos(plotdata$corner) * 1.1
  plotdata$ylabel <- plotdata$radiuslab * sin(plotdata$corner) * 1.1
  plotdata <- rbind(plotdata, plotdata[1,,drop=F])
  num <- 100
  circle1 <- makeCircle(breaks[1], num)
  circle2 <- makeCircle(breaks[2], num)
  circle3 <- makeCircle(breaks[3], num)
  circle4 <- makeCircle(breaks[4], num)

  p <-
    ggplot(plotdata, aes(x = x, y = y)) + geom_path(colour = "green", size = size) +
    geom_text(aes(x=xlabel,y=ylabel , label = BostypeCode)) + coord_fixed() +
    xlim(-limits * 1.15, limits * 1.15) + ylim(-limits * 1.15, limits * 1.15) +
    geom_segment(aes(xend=0, yend=0, x = xlabel, y=ylabel)) +
    geom_path(data = circle1, aes(x = x, y = y)) +
    geom_path(data = circle2, aes(x = x, y = y)) +
    geom_path(data = circle3, aes(x = x, y = y)) +
    facet_wrap(~OPNAMECODE)
  p
}








berekenVerwantschap <- function(Opname, Referentie, correctie = TRUE, TypologieCode = "T1"){
  ####

  calcSimilarity <- function(dat, ref, cor, typo){
    cat(as.character(dat$OPNAMECODE[1]), "\n")

    ref <- subset(ref, TypologieCode == typo)
    #zo te zien niet echt meer nodig, de IndValwaarden in de databank zijn reeds genormaliseerd
    #refsum <- ddply(ref, .(BostypeCode), summarize, sum_IndVal = sum(IndVal))

    rv <- merge(dat[c("SPECIES_NR","BEDEKKING")],
                subset(ref, TypologieCode == typo, c("SPECIES_NR", "BostypeCode", "Freq", "KarBed", "IndVal")),
                all.x = TRUE, by = "SPECIES_NR")

    #rv <- merge(rv, refsum, all.x = TRUE, by = "BostypeCode")
    if(cor == TRUE){
      rv$w_ak<- sqrt(pmin(rv$BEDEKKING, rv$KarBed) / pmax(rv$BEDEKKING, rv$KarBed))
    } else {
      rv$w_ak <- 1
    }
    rv$v_ak<- rv$IndVal * rv$w_ak
    #print(rv)

    ddply(rv, .(BostypeCode), summarize, S_aj = sum(v_ak))
  }

  ####

  ddply(Opname, .(OPNAMECODE), .fun = calcSimilarity, ref = Referentie, cor = correctie, typo = TypologieCode)
}


