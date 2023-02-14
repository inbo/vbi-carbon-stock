#### Run_by ----

source(here::here("Scripts/run_by.R"))

cat(paste0("\n\n SCRIPT WORDT GERUND DOOR ", run_by, "\n--------------------------------\n\n"))



###################################################################
#### Bestandsnamen INVOERgegevens ----
###################################################################

# FireBirdTest
db_test <- "Data/MeetgegevensVal/MeetgegevensRuw/BOSINV_2019_Alex/test.FDB"




# Meetgegevens ruw -------------------------------------------------
dbVBI1Raw <- "Data/MeetgegevensRuw/bosinv1_2011.accdb"

dbVBI1_vegRaw<-"Data/MeetgegevensRuw/bosveg4.mdb"

dbVBI2Raw <- "Data/MeetgegevensRuw/FieldMapData_Bosinventaris_v4.accdb"


## VBI3----
source(here::here("Scripts/VBI3_Functies.R"))

if (run_by == 'LG') {
  db3 <- "C:/Users/govaerle/OneDrive - Vlaamse overheid - Office 365/BOSINVENTARISATIE/2030_FM_moederdatabankVBI3/MASTERDATA/MASTER_VBI3_F/FIELDMAPDATA_MASTER_VBI3_F.FDB"
} else if (run_by == 'AL') {
  db3 <- "C:/01_VBI/1Bosinventarisatie_v5/Data/2030_FM_moederdatabankVBI3/MASTER_VBI3_F/FIELDMAPDATA_MASTER_VBI3_F.FDB"
} 


## DHcurves (aangemaakt in FM-IA)----

if (run_by == 'LG') {
  dhcurves_path <- "C:/Users/govaerle/OneDrive - Vlaamse overheid - Office 365/BOSINVENTARISATIE/DHcurves/"
  } else if (run_by == 'AL') {
  dhcurves_path <- "C:/01_VBI/1Bosinventarisatie_v5/Data/DHcurves/"
} 


# Meetgegevens gevalideerd ------------------------------------------
dbVBI1 <- here::here("Data/MeetgegevensVal/bosinv1_2011Val.accdb")

dbVBI1_veg<-here::here("Data/MeetgegevensVal/bosveg4Val.mdb")

#
# dbVBI2 <- "Data/MeetgegevensVal/FieldMapDataVal_Bosinventaris_v4.accdb"


#LG
# dbVBI2Val_path <- "C:/Users/govaerle/OneDrive - Vlaamse overheid - Office 365/BOSINVENTARISATIE/2020_FM_moederdatabankVBI2/2020_SEL_F_X18_Val/FIELDMAPDATA_2020_SEL_F_X18_VAL.FDB"
# #AL
# dbVBI2Val_path <- "C:/1Bosinventarisatie_v5/Data/2020_FM_moederdatabankVBI2/2020_SEL_F_X18_Val/FIELDMAPDATA_2020_SEL_F_X18_Val.FDB"
 
if (run_by == 'LG') {
  dbVBI2Val_path <- "C:/Users/govaerle/OneDrive - Vlaamse overheid - Office 365/BOSINVENTARISATIE/2020_FM_moederdatabankVBI2/2020_SEL_F_X18_Val/FIELDMAPDATA_2020_SEL_F_X18_VAL.FDB"
} else if (run_by == 'AL') {
  dbVBI2Val_path <- "C:/01_VBI/1Bosinventarisatie_v5/Data/2020_FM_moederdatabankVBI2/2020_SEL_F_X18_Val/FIELDMAPDATA_2020_SEL_F_X18_Val.FDB"
} 




# Meetproces ---------------------------------------------------------
dbVBI1_treeID <-"Data/Meetproces/tblTreeDEF.accdb"
    #plots_overzicht_v4 bevat deze informatie :
    #   tblBos <-"Data/Meetproces/VBI2_bos.csv"
    #   tblNietBos<-"Data/Meetproces/VBI2_geenBos.csv"
    #   tblOntbrekendeWaardenDENDRO<-"Data/Meetproces/OntbrekendeWaardenDendro.csv"
    #   tblOntbrekendeWaardenVEG<-"Data/Meetproces/OntbrekendeWaardenVEG.csv"#shpA2<-"Data/Meetproces/A2_idFULLID.shp"
    #   shpA3<-"Data/Meetproces/A3_idFULLID.shp" # nog fout te herstellen voor A3  -  254057
    #   shpStandDescrSegm <- "Data/Meetproces/Standdescription_segments_poly_FME.shp"
    #   shpMeetpuntID<-"Data/Meetproces/invb2_ID.shp"
    #   shpMeasuredCoord<- "Data/Meetproces/Plots_NFI_09tot17_20160831_nrINBO.shp"
    #   tblPlotsZonderSegm <- "Data/Meetproces/PlotsZonderSegm.csv" (nu opgenomen in shapefile segmenten)
    #   shpTreesSegmentLink<-"Data/Meetproces/PointOnAreaOverlayTreesStandDescr.shp"
    #   tblTreesSegmentID <- "Data/Meetproces/tblTreesSegmentID.csv" #nieuw toegevoegd nav QC 2020_SEL 2018


#Databank meetproces ------------------------------------
dbPlotsoverzicht <- "Data/Meetproces/plots_overzicht_v4_20221115.accdb"
#dbPlotsoverzicht <- "Data/Meetproces/plots_overzicht_v4_20200217.accdb"
#dbPlotsoverzicht <- "Data/Meetproces/plots_overzicht_v4_20190827.accdb"
# dbPlotsoverzicht <- "Data/Meetproces/plots_overzicht_v4_.accdb" #toevoeging LG 21/2/2018

dbMeetproces <-  here::here("Output/VBI_Meetproces_v2022-12-21.accdb")
# dbMeetproces <-  here::here("Output/VBI_Meetproces_v2022-11-15.accdb")
# dbMeetproces <-  here::here("Output/VBI_Meetproces_v2022-11-11.accdb")
# dbMeetproces <-  here::here("Output/VBI_Meetproces_v2022-11-01.accdb")
#dbMeetproces <-  here::here("Output/VBI_Meetproces_v2022-05-20.accdb")
# dbMeetproces <-  here::here("Output/VBI_Meetproces_v2021-11-01.accdb") # nav bug tblRecords2 en A2 reg
# dbMeetproces <-  here::here("Output/VBI_Meetproces_v2021-08-31.accdb") # nav bug plotweights
# dbMeetproces <-  "Output/VBI_Meetproces_v2021-07-27.accdb"
# dbMeetproces <- "Output/VBI_Meetproces_v2021-04-23.accdb"
# dbMeetproces <- "Output/VBI_Meetproces_v2021-02-08.accdb"
# dbMeetproces <- "Output/VBI_Meetproces_v2020-11-04.accdb"
# dbMeetproces <- "Output/VBI_Meetproces_v2020-07-07.accdb"
# dbMeetproces <- "Output/VBI_Meetproces_v2020-04-17.accdb"
# dbMeetproces <- "Output/VBI_Meetproces_v2019-08-27.accdb"
# dbMeetproces <- "Output/VBI_Meetproces_v2019-06-24.accdb"
# dbMeetproces <- "Output/VBI_Meetproces_v2019-06-24.accdb"


#Externe data --------------------------------------------
dbExterneData<-here::here("Data/ExterneData/VBIExterneData_v2022-08-23.accdb")


# Strata ---------------------------------------------------
shpStrata<-"Data/Strata/invb2_ecod_bodem_BHR_ANB_GW_correctie20191015.shp"
shpStrata2<-"Data/Strata/invb2_UBBPvsBBBP.shp"
shpStrata3<-"Data/Strata/invb2_VNR_ENR_BR_HRL_VRL.shp"
shpStrata4<-"Data/Strata/invb3_RechtenANB.shp"
shpStrata5 <- "Data/Strata/invb3_IDBoswijzer2012.shp"
  # shpStrata_oud1 <- "Data/Strata/invb2_ecod_bodem_BHR_ANB_GW.shp"
  # shpStrata_oud2 <- "Data/Strata/invb2_ecod_bodem_BHR_ANB.shp"

shpStrataExtra <- "Data/Strata/ExtraStrata.shp"

soil_group <- "Data/Strata/Bodemassociatieklassen_Wim.csv"
shpDHM_elevation <- "Data/Strata/invb_PlotsMetTrees_DHMII.shp" 
shpSoil2017 <- "Data/Strata/XY_plots_VBI_soil2017_points.shp"  # aangemaakt door M.Stevens obv vernieuwe bodemkaart, voor alle 27163 plots


# Eigenaars (ifv aanschrijven VBI2- VBI3)---------------------------------------------------------------
tblPerceel <-"Data/ExterneData/DI19002062_natuurenbos_mrt_parcel.csv"
tblOwner <-"Data/ExterneData/DI19002062_natuurenbos_mrt_owner.csv"
tbl2019 <- "Data/ExterneData/2019_kapakey_bosinv.csv"
tblOwnerNotFound <- "DI19002062_notfound.csv"



##########################################################################
#### Bestandsnamen OUTPUT ----
#########################################################################

#Databank strata -------------------------------
dbStrata <- here::here("Output/VBI_Strata_v2022-11-01.accdb")
#dbStrata <- here::here("Output/VBI_Strata_v2022-08-02.accdb")
#dbStrata <- here::here("Output/VBI_Strata_v2022-02-04.accdb")
# dbStrata <- here::here("Output/VBI_Strata_v2021-11-23.accdb")
#dbStrata <- here::here("Output/VBI_Strata_v2021-06-18.accdb")
# dbStrata <- "Output/VBI_Strata_v2020-05-27.accdb"
# dbStrata <- "Output/VBI_Strata_v2020-05-26.accdb"
#dbStrata <- "Output/VBI_Strata_v2020-02-05.accdb"
#dbStrata <- "Output/VBI_Strata_v2019-15-10.accdb"
#dbStrata <- "Output/VBI_Strata_v2019-06-03.accdb"

#Analysedatabank ---------------------------
dbAnalyseData <- here::here("Output/VBI_Analysedatabank_v2023-02-01.accdb")

# dbAnalyseData <- here::here("Output/VBI_Analysedatabank_v2022-11-15.accdb")
# dbAnalyseData <- here::here("Output/VBI_Analysedatabank_v2022-11-11.accdb")
# dbAnalyseData <- here::here("Output/VBI_Analysedatabank_v2022-11-01.accdb")
#dbAnalyseData <- here::here("Output/VBI_Analysedatabank_v2022-05-20.accdb")
#dbAnalyseData <- here::here("Output/VBI_Analysedatabank_v2022-05-20.accdb")
#dbAnalyseData <- here::here("Output/VBI_Analysedatabank_v2021-11-01.accdb") # nav aanmaak nieuwe tbl0Boom / Boomsoorten en tbl3 + 20% VBI3
# dbAnalyseData <- here::here("Output/VBI_Analysedatabank_v2021-08-31.accdb") # nav aanmaak nieuwe tbl0Boom + 20% VBI3
# dbAnalyseData <- "Output/VBI_Analysedatabank_v2021-05-21.accdb" # aanmaak 1a_transitiematrix

  # dbAnalyseData <- "Output/VBI_Analysedatabank_v2020-09-17.accdb" # nieuwe tbl10bisBoomsoortenPlot

  # dbAnalyseData <- "Output/VBI_Analysedatabank_v2020-07-03.accdb" # nieuwe tbl0Boom
  # dbAnalyseData <- "Output/VBI_Analysedatabank_v2020-06-02.accdb" # nieuwe tbl0Boom
  # dbAnalyseData <- "Output/VBI_Analysedatabank_v2020-05-14.accdb"
  # dbAnalyseData <- "Output/VBI_Analysedatabank_v2020-03-16.accdb" 
              # tbl4 en tbl11 aangeast nalv slecht gekoppelde soorten uit herblayer (wegens afkappen in access op 40 tekens) - AL
  # dbAnalyseData <- "Output/VBI_Analysedatabank_v2020-02-17.accdb" 
             # aangepaste transitiematrix nav Review 'open ruimte' VBI1 2020 februari LG

  # dbAnalyseData <- "Output/VBI_Analysedatabank_v2019-11-29.accdb"
  # dbAnalyseData <- "Output/VBI_Analysedatabank_v2019-11-07.accdb"
  # dbAnalyseData <- "Output/VBI_Analysedatabank_v2019-10-15.accdb"
  # dbAnalyseData <- "Output/VBI_Analysedatabank_v2019-10-10.accdb"
  # dbAnalyseData <- "Output/VBI_Analysedatabank_v2019-09-20.accdb"
  # dbAnalyseData <- "Output/VBI_Analysedatabank_v2019-04-15.accdb"
  # dbAnalyseData <- "Output/VBI_Analysedatabank_v2017-06-10.accdb"
  # dbAnalyseData <- "Output/VBI_Analysedatabank_v2017-01-19.accdb"
  # dbAnalyseData<-"Output/VBI_Analysedatabank_v2016-01-11.accdb"


#Resultatendatabank --------------------------------
#dbResultaten<-here::here("Output/VBI_resultaten.accdb")
dbResultaten <- paste0("C:/Users/", user,"/Vlaamse overheid - Office 365/Natuur & Bos - Bosinventaris - Bib data en dataverwerking/Output_Resultatendb/VBI_resultaten.accdb")

#Anomalieëndatabank ---------------------------------
dbAnomalies<-"Validatie/VBI_Anomalies.accdb"


#Databank aanvraag gegevens  ------------------------
dbAanvraagGegevens<-"Output/VBI_AanvraagGegevens.accdb"



###################################################################
#### Bestandsnamen OUTPUT RESULTATENDB - TXT ----
###################################################################

# voor gebruik bij wegschrijven naar resultatendb
dbAnalyseDataTxt <- str_extract(dbAnalyseData ,"VBI_Analysedatabank.+") 
dbStrataTxt <- str_extract(dbStrata ,"VBI_Strata.+")   
dbExterneDataTxt <- str_extract(dbExterneData ,"VBIExterneData.+")
dbMeetprocesTxt <- str_extract(dbMeetproces ,"VBI_Meetproces.+")

# voor gebruik bij wegschrijven naar resultatendb - aanmaken hash
dbResultaten_path <- dbResultaten
dbAnalyseData_path <- dbAnalyseData
dbStrata_path <- dbStrata
dbExterneData_path <- dbExterneData
dbMeetproces_path <- dbMeetproces

