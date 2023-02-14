# ---
# title: "Functies Inlezen Data"
# author: "Leen Govaere"
# date: "9 april 2021"

# ---
# 
# 
# 
# ## Functies inlezen data en metadata VBI3
# 
# *   overal kleine letter
# *   waar zinvol ook fmguid
# *   
# 


get_Fase1_invb3 <- function(db = db3){

query_ADMINplot <- "SELECT ADMIN_Screening_OrthoPhoto.IDPLOTS as plot_id
  , ADMIN_Screening_OrthoPhoto.PLOTNR as IDPLOTS
  , ADMIN_Screening_OrthoPhoto.OrthoPhotoClass as Cde_Fase1
  , ADMIN_Screening_OrthoPhoto.X_M as Xr
  , ADMIN_Screening_OrthoPhoto.Y_M as Yr
  , QBOS.VALUE2 as Fase1
   FROM ADMIN_Screening_OrthoPhoto LEFT JOIN QBOS ON ADMIN_Screening_OrthoPhoto.OrthoPhotoClass = QBOS.ID;"

  if (str_sub(db, nchar(db) - 3, nchar(db)) == ".mdb") {
    connect_db <-   odbcConnectAccess(db)
  } else if (str_sub(db, nchar(db) - 5, nchar(db)) == ".accdb") {
    connect_db <-   odbcConnectAccess2007(db)
  } else if (str_sub(db, nchar(db) - 3, nchar(db)) == ".FDB") {
    connect_db <-   odbcDriverConnect(paste("DRIVER={Firebird/InterBase(r) driver};UID=SYSDBA;PWD=masterkey; DBNAME=", db))
  }

 gridpoints_ADMIN <- sqlQuery(connect_db, query_ADMINplot)
 colnames(gridpoints_ADMIN) <- str_to_lower(colnames(gridpoints_ADMIN))
 

  odbcClose(connect_db)

  return(gridpoints_ADMIN)
}


get_PlotDescr_invb3 <- function(db = db3){
  
query_PlotDescr <- "select distinct i.IDPLOTS
, p.VALUE1 as PLOTCYCLE
, i.PLOTSTATUS as cdePLOTSTATUS
, s.VALUE1 as PLOTSTATUS
, a.VALUE1 as ACCESSIBILITY
, i.REMARK
, u.VALUE1 as LANDUSE 
, i.FMGUID as fmguid_plotdescription
, f.VALUE1 as a34bomen
, l.VALUE1 as landcat
FROM INVCYCLE i
LEFT JOIN  QPLOTCYCLE p ON i.PLOTCYCLE = p.ID
LEFT JOIN QPLOTSTATUS s ON i.PLOTSTATUS = s.ID
LEFT JOIN  QACCESSIBILITY a ON i.ACCESSIBILITY = a.ID
LEFT JOIN QCONLANDUSE u on i.LANDUSE = u.ID
LEFT JOIN QCONFORESTAREATYPE f ON i.FORESTAREATYPE = f.ID
LEFT JOIN QCONLANDCATEGORY l ON i.LANDCATEGORY = l.ID
WHERE PLOTCYCLE = 3;" 
  
  if (str_sub(db, nchar(db) - 3, nchar(db)) == ".mdb") {
    connect_db <-   odbcConnectAccess(db)
  } else if (str_sub(db, nchar(db) - 5, nchar(db)) == ".accdb") {
    connect_db <-   odbcConnectAccess2007(db)
  } else if (str_sub(db, nchar(db) - 3, nchar(db)) == ".FDB") {
    connect_db <-   odbcDriverConnect(paste("DRIVER={Firebird/InterBase(r) driver};UID=SYSDBA;PWD=masterkey; DBNAME=", db))
  }

  PlotDescr_invb2_3 <- sqlQuery(connect_db, query_PlotDescr)
  colnames(PlotDescr_invb2_3) <- str_to_lower(colnames(PlotDescr_invb2_3))
 
 

    odbcClose(connect_db)
    
    return(PlotDescr_invb2_3)
  
}




get_StandDescr_invb3 <- function(db = db3){
  
query_StandDescr <- "select distinct
    s.IDPLOTS
    , s.ID AS IDSEGMENTS
    , s.FMGUID as fmguid_standdescr
    , AREA_M2
    
    , s.LANDUSE AS LanduseCode
    , q.VALUE1 AS Landuse
    , s.STANDTYPE AS StandTypeCode
    , qs.VALUE1 AS StandType 
    , s.STANDAGE AS StandAgeCode
    , qa.VALUE1 AS StandAge
    , qma.VALUE1 AS Age_highest_ageclass
    , s.MIXTYPE AS MixTypecode
    , qm.VALUE1 AS MixType
    , s.HARVESTTYPE AS HarvestTypeCode
    , qh.VALUE1 AS HarvestType
    , s.WINDTHROW AS WindThrowCode
    , REMARK 
  FROM STANDDESCRIPTION_SEGMENTS_3 s LEFT JOIN
  QSTANDDESCR_LANDUSE q ON s.LANDUSE = q.ID LEFT JOIN
  QSTANDTYPE qs ON s.STANDTYPE = qs.ID LEFT JOIN
  QSTANDAGE qa ON s.STANDAGE = qa.ID LEFT JOIN
  QMAXSTANDAGE qma ON s.AGE_HIGHEST_AGECLASS = qma.ID LEFT JOIN
  QMIXTYPE qm ON s.MIXTYPE = qm.ID LEFT JOIN 
  QHARVESTTYPE qh ON s.HARVESTTYPE = qh.ID LEFT JOIN
  YESNO ON s.WINDTHROW = YESNO.ID
  order by IDPLOTS, IDSEGMENTS;"

  
  if (str_sub(db, nchar(db) - 3, nchar(db)) == ".mdb") {
    connect_db <-   odbcConnectAccess(db)
  } else if (str_sub(db, nchar(db) - 5, nchar(db)) == ".accdb") {
    connect_db <-   odbcConnectAccess2007(db)
  } else if (str_sub(db, nchar(db) - 3, nchar(db)) == ".FDB") {
    connect_db <-   odbcDriverConnect(paste("DRIVER={Firebird/InterBase(r) driver};UID=SYSDBA;PWD=masterkey; DBNAME=", db))
  }

PlotDescr_invb3 <- sqlQuery(connect_db, query_StandDescr)
colnames(PlotDescr_invb3) <- str_to_lower(colnames(PlotDescr_invb3))
  
 

    odbcClose(connect_db)
    
    return(PlotDescr_invb3)
  
}


get_type_observed_foresthab_vbi <- function(db = db3){

query_status <- "SELECT p.id as IDPlots
        , p.Habitat as HabitatCde
        , q.VALUE1 AS Habitat
        FROM PLOTS p
        LEFT JOIN qHabitat Q
        ON p.Habitat = Q.id WHERE p.ID > 11000;"    

connect_db <- odbcDriverConnect(paste("DRIVER={Firebird/InterBase(r) driver};UID=SYSDBA;PWD=masterkey; DBNAME=", db))
  Habitat_orig <- sqlQuery(connect_db,query_status, stringsAsFactors = FALSE)

  Habitat <- Habitat_orig %>%
      filter(!is.na(HABITAT))

colnames(Habitat) <- str_to_lower(colnames(Habitat))
odbcClose(connect_db)

return(Habitat)
}


get_cover_veglayers_forest <- function(db = db3){

  query_veglayers <- "SELECT
  Vegetation_3.IDPlots,
  Vegetation_3.ID as IDSegments,
  Vegetation_3.Total_herb_cover as herblayer,
  Vegetation_3.Total_moss_cover as mosslayer,
  Vegetation_3.Total_shrub_cover as shrublayer,
  Vegetation_3.Total_tree_cover as treelayer,
  Vegetation_3.FMGUID as fmguid_veg
  FROM Vegetation_3 ;"

  query_coverscales <- "SELECT * FROM QTOTALCOVER;"
  
  connect_db <- odbcDriverConnect(paste("DRIVER={Firebird/InterBase(r) driver};UID=SYSDBA;PWD=masterkey; DBNAME=", db))

  coverscales <- sqlQuery(connect_db, query_coverscales, stringsAsFactors = FALSE)
  veglayers_orig <- sqlQuery(connect_db, query_veglayers, stringsAsFactors = FALSE)
  
  
  odbcClose(connect_db)

  colnames(veglayers_orig) <- str_to_lower(colnames(veglayers_orig))
  
  veglayer <- veglayers_orig %>%
    gather(herblayer, shrublayer, treelayer, mosslayer, key = "layer", value = "class_id") %>%
    left_join(coverscales, by = c("class_id" = "ID")) %>%
    select(IDPlots = idplots, IDSegments = idsegments, layer, cover = VALUE1) %>%
    arrange(IDPlots, IDSegments)
  
  colnames(veglayer) <- str_to_lower(colnames(veglayer))
  
  return(veglayer)

} 

get_cover_vegsample <- function(db = db3){

  query_herblayer<-"SELECT Herblayer_3.IDPlots,
  Herblayer_3.idvegetation_3,
  Herblayer_3.Species as name_id,
  qVEG_HerbSpecies.Value1 as name_nl,
  qVEG_HerbSpeciesScientific.Value1 as name_sc,
  Herblayer_3.Coverage_date1 as class_id
  FROM Herblayer_3
  LEFT JOIN qVEG_HerbSpecies ON Herblayer_3.Species = qVEG_HerbSpecies.ID
  LEFT JOIN qVEG_HerbSpeciesScientific ON Herblayer_3.Species_scientific = qVEG_HerbSpeciesScientific.ID;
  "

  query_shrublayer <- "
  SELECT Shrublayer_3.IDPlots,
  Shrublayer_3.idvegetation_3,
  Shrublayer_3.Species as name_id,
  qVEG_TreeSpecies.Value1 as name_nl,
  qVEG_TreeSpeciesScientific.Value1 as name_sc,
  Shrublayer_3.Coverage as class_id
  FROM Shrublayer_3
  LEFT JOIN qVEG_TreeSpecies ON Shrublayer_3.Species = qVEG_TreeSpecies.ID
  LEFT JOIN qVEG_TreeSpeciesScientific ON Shrublayer_3.Species_Scientific = qVEG_TreeSpeciesScientific.ID;
  "

  query_treelayer <- "
  SELECT Treelayer_3.IDPlots,
  Treelayer_3.idvegetation_3,
  Treelayer_3.Species as name_id,
  qVEG_TreeSpecies.Value1 as name_nl,
  qVEG_TreeSpeciesScientific.Value1 as name_sc,
  Treelayer_3.Coverage as class_id
  FROM Treelayer_3
  LEFT JOIN qVEG_TreeSpecies ON Treelayer_3.Species = qVEG_TreeSpecies.ID
  LEFT JOIN qVEG_TreeSpeciesScientific ON Treelayer_3.Species_Scientific = qVEG_TreeSpeciesScientific.ID;
  "
  
  query_coverscales <- "SELECT * FROM QCOVERHERBS;"
  
  connect_db <- odbcDriverConnect(paste("DRIVER={Firebird/InterBase(r) driver};UID=SYSDBA;PWD=masterkey; DBNAME=", db))

  coverscales <- sqlQuery(connect_db, query_coverscales, stringsAsFactors = FALSE)

  herblayer_orig <- sqlQuery(connect_db, query_herblayer, stringsAsFactors = FALSE)
  shrublayer_orig <- sqlQuery(connect_db, query_shrublayer, stringsAsFactors = FALSE)
  treelayer_orig <- sqlQuery(connect_db, query_treelayer, stringsAsFactors = FALSE)

  odbcClose(connect_db)
 
   herblayer <- herblayer_orig %>%
     mutate(layer = "herblayer")

   shrublayer <- shrublayer_orig %>%
     mutate(layer = "shrublayer")

   treelayer <- treelayer_orig %>%
     mutate(layer = "treelayer")

  veglayers <- bind_rows(herblayer, shrublayer, treelayer) 
  
  colnames(veglayers) <- str_to_lower(colnames(veglayers))
  
  vegsample <- veglayers %>%
    filter(!is.na(name_id)) %>%
    mutate(coverscale_name = "Braun-Blanquet") %>%
    left_join(coverscales, by = c("class_id" = "ID")) %>%
    select(IDPlots = idplots, idsegments=idvegetation_3, layer, name_nl, name_sc, coverscale_name) %>%
    arrange(IDPlots, idsegments, layer, name_sc) %>%
    filter(!is.na(name_nl) | !is.na(name_sc))
 
  colnames(vegsample) <- str_to_lower(colnames(vegsample))   
  return(vegsample)

}

get_date_forest <- function(db = db3){

  query_date_veg <- "SELECT DISTINCT
  IDPlots,
  date_vegetation
  FROM Observer_date_veg_3"
  
   query_date_dendro <- "SELECT DISTINCT
  IDPlots,
  date_dendro
  FROM Observer_date_dendro_3"

  connect_db <- odbcDriverConnect(paste("DRIVER={Firebird/InterBase(r) driver};UID=SYSDBA;PWD=masterkey; DBNAME=", db))

  observer_date_veg <- sqlQuery(connect_db, query_date_veg)
  observer_date_dendro <- sqlQuery(connect_db, query_date_dendro)

  odbcClose(connect_db)
  
  observer_date <- observer_date_veg %>%
      full_join(observer_date_dendro, by =  "IDPLOTS") %>%
      unique() %>%
      filter(!is.na(DATE_VEGETATION) | !is.na(DATE_DENDRO) )
  
  
  
  
  observer_date_1 <- observer_date %>%
      group_by("IDPLOTS") %>%
      summarise(date_vegetation = min(DATE_VEGETATION, na.rm = TRUE),
                date_dendro = min(DATE_DENDRO, na.rm = TRUE)) %>%
      ungroup() 
  
  colnames(observer_date) <- str_to_lower(colnames(observer_date))
  
  return(observer_date)
}

get_coordinates_forest <- function(db = db3){

  query_coord <- "SELECT
  Plots_Forest_Inventory.IDPlots
  , Plots_Forest_Inventory.Plotnr as plot_id
  , Plots_Forest_Inventory.X_m as x_measured
  , Plots_Forest_Inventory.Y_m as y_measured
  , Plots_Forest_Inventory.EDIT_DATE
  FROM Plots_Forest_Inventory"
  
  connect_db <- odbcDriverConnect(paste("DRIVER={Firebird/InterBase(r) driver};UID=SYSDBA;PWD=masterkey; DBNAME=", db))

  coordinates <- sqlQuery(connect_db, query_coord) %>%
      filter(!is.na(PLOT_ID))
  
  colnames(coordinates) <- str_to_lower(colnames(coordinates))

  odbcClose(connect_db)
  
  return(coordinates)
}

# _alle NOMT : alle bomen met een meting (levend en staand dood)
get_treesa3a4_all <- function (db = db3){

  query_trees<-"

SELECT DISTINCT Trees_3.IDPlots as IDPlots,
  Trees_3.IDSegment as IDSegments,
  Trees_3.ID as ID,
  Trees_3.X_m as X_m, 
  Trees_3.Y_m as Y_m,
  Trees_3.NewOrMissingTree as NOMTCde,
 qNEWORMISSINGTREE.VAlue1 AS NOMT,
  Trees_3.Species,
  Trees_3.Species_Scientific,
  Trees_3.Status_tree as statustreecode,
  Trees_3.Perimeter_cm,
  Trees_3.DBH_mm,
  Trees_3.Height_m,
  Trees_3.CodeCoppice_Individual as coppice_individualcode,
  Trees_3.IntactTree as intacttreecode,
  Trees_3.Species as IDTreeSp,
 qTreeSpecies.Value1 as namenl,
 qStatusTree.Value1 as statustree,
 qCONCoppice.Value1 as coppice_individual,
 qCONIntactTree.Value1 as intacttree,
  Trees_3.FMGUID as fmguid_tree,
  Trees_3.OLDFMGUID as oldfmguid_tree,
  Trees_3.OldID as oldid,
  Trees_3.EDIT_DATE as edit_date, 
  Trees_3.OPMERKING 
  FROM Trees_3 
  LEFT JOIN qTreeSpecies ON Trees_3.Species = qTreeSpecies.ID
  LEFT JOIN qStatusTree ON Trees_3.Status_tree = qStatusTree.ID
  LEFT JOIN qCONCoppice ON Trees_3.CodeCoppice_Individual = qCONCoppice.ID
  LEFT JOIN qCONIntactTree ON Trees_3.IntactTree = qCONIntactTree.ID
  LEFT JOIN qNEWORMISSINGTREE ON Trees_3.NewOrMissingTree = QNEWORMISSINGTREE.ID
  WHERE IDPLOTS > 11000 AND IDSegment IS NOT NULL;
  "

  connect_db <- odbcDriverConnect(paste("DRIVER={Firebird/InterBase(r) driver};UID=SYSDBA;PWD=masterkey; DBNAME=", db))
  
  treesa3a4_all <- sqlQuery(connect_db, query_trees, stringsAsFactors = FALSE)
  
  colnames(treesa3a4_all) <- str_to_lower(colnames(treesa3a4_all))
  
    odbcClose(connect_db)
  
  

  return(treesa3a4_all)
}

# _snag_living : alle bomen met een meting (levend en staand dood)
get_treesa3a4_snag_living <- function (db = db3){

  query_trees<-"
  SELECT DISTINCT Trees_3.IDPlots as IDPlots,
  Trees_3.IDSegment as IDSegments,
  Trees_3.ID as ID,
  Trees_3.X_m as X_m, 
  Trees_3.Y_m as Y_m,
  Trees_3.NewOrMissingTree as NOMTCde,
  qNEWORMISSINGTREE.VAlue1 AS NOMT,
  Trees_3.Status_tree as cde_status_tree,
  Trees_3.Perimeter_cm,
  Trees_3.DBH_mm,
  Trees_3.Height_m,
  qTreeSpecies.Value1 as name_nl,
  qStatusTree.Value1 as status_tree,
  qCONCoppice.Value1 as coppice_individual,
  qCONIntactTree.Value1 as intact_tree,
  Trees_3.FMGUID as fmguid_tree,
  Trees_3.OLDFMGUID as oldfmguid_tree,
  Trees_3.OldID as oldid
  FROM Trees_3 
  LEFT JOIN qTreeSpecies ON Trees_3.Species = qTreeSpecies.ID
  LEFT JOIN qStatusTree ON Trees_3.Status_tree = qStatusTree.ID
  LEFT JOIN qCONCoppice ON Trees_3.CodeCoppice_Individual = qCONCoppice.ID
  LEFT JOIN qCONIntactTree ON Trees_3.IntactTree = qCONIntactTree.ID
  LEFT JOIN qNEWORMISSINGTREE ON Trees_3.NewOrMissingTree = QNEWORMISSINGTREE.ID
  WHERE IDPLOTS > 11000 AND Trees_3.NewOrMissingTree IN (1, 100, 300, 150, 175, 250, 400, 800) AND IDSegment IS NOT NULL;
  "

  connect_db <- odbcDriverConnect(paste("DRIVER={Firebird/InterBase(r) driver};UID=SYSDBA;PWD=masterkey; DBNAME=", db))
  
  treesa3a4_snag_living <- sqlQuery(connect_db, query_trees, stringsAsFactors = FALSE)
  colnames(treesa3a4_snag_living) <- str_to_lower(colnames(treesa3a4_snag_living))

  odbcClose(connect_db)
  
  

  return(treesa3a4_snag_living)
}

# exploiteerd/ontbost en afgestorven
get_treesa3a4_EA <- function (db = db3){

  query_trees<-"
  SELECT Trees_3.IDPlots as IDPlots,
  Trees_3.IDSegment as IDSegments,
  Trees_3.ID as ID,
  Trees_3.X_m as X_m, 
  Trees_3.Y_m as Y_m,
  Trees_3.NewOrMissingTree as NOMTCde,
  qNEWORMISSINGTREE.VAlue1 AS NOMT,
  Trees_3.Status_tree as cde_status_tree,
  Trees_3.Perimeter_cm,
  Trees_3.DBH_mm,
  Trees_3.Height_m,
  qTreeSpecies.Value1 as name_nl,
  qStatusTree.Value1 as status_tree,
  qCONCoppice.Value1 as coppice_individual,
  qCONIntactTree.Value1 as intact_tree,
  Trees_3.FMGUID as fmguid_tree,
  Trees_3.OLDFMGUID as oldfmguid_tree, 
  Trees_3.OldID as oldid
FROM Trees_3 
  LEFT JOIN qTreeSpecies ON Trees_3.Species = qTreeSpecies.ID
  LEFT JOIN qStatusTree ON Trees_3.Status_tree = qStatusTree.ID
  LEFT JOIN qCONCoppice ON Trees_3.CodeCoppice_Individual = qCONCoppice.ID
  LEFT JOIN qCONIntactTree ON Trees_3.IntactTree = qCONIntactTree.ID
  LEFT JOIN qNEWORMISSINGTREE ON Trees_3.NewOrMissingTree = QNEWORMISSINGTREE.ID
  WHERE IDPLOTS > 11000 AND Trees_3.NewOrMissingTree IN (750, 775, 200, 225, 600, 700) AND IDSegment IS NOT NULL ;
  "

  connect_db <- odbcDriverConnect(paste("DRIVER={Firebird/InterBase(r) driver};UID=SYSDBA;PWD=masterkey; DBNAME=", db))
  
  treesa3a4_EA <- sqlQuery(connect_db, query_trees, stringsAsFactors = FALSE)
  colnames(treesa3a4_EA) <- str_to_lower(colnames(treesa3a4_EA))
  
  odbcClose(connect_db)
  
  

  return(treesa3a4_EA)
}

get_treesa2 <- function(db = db3){

  query_trees_a2 <- "SELECT 
  REGENERATIONPLOT_3.IDPLOTS as IDPlots
, REGENERATIONPLOT_3.REGDESCRIPTIONPRESENT
, REGENERATIONPLOT_3.ID as IDSegments
, DOORGROEIENDE_VERJONGING_3.SPECIES as IDTreeSp
, qTreeSpecies.VALUE1 as name_nl
, DOORGROEIENDE_VERJONGING_3.NUMBER as n_individuals
  FROM REGENERATIONPLOT_3 LEFT JOIN DOORGROEIENDE_VERJONGING_3 
  ON REGENERATIONPLOT_3.IDPLOTS = DOORGROEIENDE_VERJONGING_3.IDPLOTS AND REGENERATIONPLOT_3.ID = DOORGROEIENDE_VERJONGING_3.IDREGENERATIONPLOT_3
  LEFT JOIN QTREESPECIES ON DOORGROEIENDE_VERJONGING_3.SPECIES = QTREESPECIES.ID
  ;"

   
  connect_db <- odbcDriverConnect(paste("DRIVER={Firebird/InterBase(r) driver};UID=SYSDBA;PWD=masterkey; DBNAME=", db))

  trees_a2 <- sqlQuery(connect_db, query_trees_a2)
  
  odbcClose(connect_db)
  
  colnames(trees_a2) <- str_to_lower(colnames(trees_a2))
  
  #check extra NA-record in doorgroeiende_verjonging (per ongeluk aangeklikt)
  segm_met_verjonging <- trees_a2 %>% 
    dplyr::group_by(idplots, idsegments) %>% 
    dplyr::summarize(aantalverj = sum(n_individuals, na.rm = T)) %>% 
    dplyr::ungroup() %>% 
    filter(aantalverj > 0)
  
  t <- trees_a2 %>% 
    inner_join(segm_met_verjonging) %>% 
    filter(is.na(idtreesp)) 
  
  t2 <- trees_a2 %>% 
    inner_join(t %>% dplyr::select(idplots, idsegments))
  
  trees_a2_ <- trees_a2 %>% 
    anti_join(t %>% dplyr::select(idplots, idsegments, idtreesp))
  
  trees_a2 <- trees_a2_

  return(trees_a2)

}
    
get_logs <- function(db = db3){

  query_logs <- "
  SELECT Line_intersect_method_3.IDPLOTS as IDPlots, 
  Line_intersect_method_3.ID, 
  Line_intersect_method_3.DeadWoodInventoryDone,  
  LIM_data_3.Diameter_cm, 
  LIM_data_3.Angle_degrees
  FROM Line_intersect_method_3 
  LEFT JOIN lim_data_3 ON (Line_intersect_method_3.ID = lim_data_3.IDLine_intersect_method_3) AND (Line_intersect_method_3.IDPLOTS = lim_data_3.IDPLOTS)
  ;
  "
  
  connect_db <- odbcDriverConnect(paste("DRIVER={Firebird/InterBase(r) driver};UID=SYSDBA;PWD=masterkey; DBNAME=", db))

  logs <- sqlQuery(connect_db, query_logs, stringsAsFactors = FALSE)

  odbcClose(connect_db)
  
  colnames(logs) <- str_to_lower(colnames(logs))
  
  return(logs)
  
}


get_kapakey<- function(db = db3){
  
  query_kapakey <- "SELECT PLOTNR, 
  Capakey,
  Capakey_tidy FROM
  ADMIN_Screening_OrthoPhoto
  ;
  "
  connect_db <- odbcDriverConnect(paste("DRIVER={Firebird/InterBase(r) driver};UID=SYSDBA;PWD=masterkey; DBNAME=", db))
  
  kapakey <- sqlQuery(connect_db, query_kapakey, stringsAsFactors = FALSE)
  
  odbcClose(connect_db)
  
  
  return(kapakey)
  
}

save_results_access <- function(results, database, remove_tables = FALSE) {
  con <- odbcConnectAccess2007(database)
  for (tablename in names(results)) {
    if (remove_tables) {
      dbtables <- sqlTables(con)
      if (tablename %in% dbtables$TABLE_NAME) {
        sqlDrop(con, tablename)
      }
    }
    sqlSave(con, results[[tablename]], tablename = tablename)
  }
  odbcClose(con)
}


write_inputfiles_as_output_csv <- function(inputfiles){
  
for(i in 1:length(inputfiles)) {                              # Head of for-loop
  write.csv2(get(inputfiles[i]),                              # Write CSV files to folder
             paste0(here::here(), "/Output/",
                    inputfiles[i],
                    ".csv"),
             row.names = FALSE)
}  
  
} 


# My.WgtParEstimation omgezet naar lowercase van inputvariabelen zoals idplots, idsegments, reeks, periode, ...
my.wgt_par_estimation<-function(Data,VariableName,Periode=NA,MinReeks=1,MaxReeks=12,MinYear=1,MaxYear=9999,UseStrata=rep(FALSE,1000),Strata){
  
  
  if (is.na(Periode)){
    DataSel<-Data[Data$reeks<=MaxReeks
                  & Data$reeks>=MinReeks
                  & Data$year>=MinYear
                  & Data$year<=MaxYear,, drop = FALSE]
  } else {
    DataSel<-Data[Data$periode==Periode
                  & Data$reeks<=MaxReeks
                  & Data$reeks>=MinReeks
                  & Data$year>=MinYear
                  & Data$year<=MaxYear,, drop = FALSE]
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
          
          wgt.mean <- sum(DataStrat$weight * Variable, na.rm=TRUE)/(sum(DataStrat$weight * (!is.na(Variable)),na.rm=TRUE))
          
        } else {
          
          wgt.mean<-NA
          
        }
        
        if (nrow(DataStrat)>1){
          
          V1<-sum(DataStrat$weight * (!is.na(Variable)),na.rm=T)
          
          V2<-sum(((DataStrat$weight) * (!is.na(Variable)))^2,na.rm=T)
          
          wgt.var <-sum(DataStrat$weight*(Variable-wgt.mean)^2)/(V1-(V2/V1))
          
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
                            minYear=ifelse(nrow(DataStrat)>0,min(DataStrat$year,na.rm=TRUE),NA),
                            maxYear=ifelse(nrow(DataStrat)>0,max(DataStrat$year,na.rm=TRUE),NA),
                            minReeks=ifelse(nrow(DataStrat)>0,min(DataStrat$reeks,na.rm=TRUE),NA),
                            maxReeks=ifelse(nrow(DataStrat)>0,max(DataStrat$reeks,na.rm=TRUE),NA),
                            nbObservaties=length(DataStrat$idplots),
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
      wgt.mean <- sum(DataSel$weight * Variable, na.rm=TRUE)/sum(DataSel$weight * (!is.na(Variable)),na.rm=TRUE)
      V1<-sum(DataSel$weight * (!is.na(Variable)),na.rm=T)
      V2<-sum(((DataSel$weight)* (!is.na(Variable)))^2,na.rm=T)
      wgt.var <-sum(DataSel$weight*(Variable-wgt.mean)^2,na.rm=T)/(V1-(V2/V1))
      lci<-wgt.mean-1.96*sqrt(wgt.var)/sqrt(V1)
      uci<-wgt.mean+1.96*sqrt(wgt.var)/sqrt(V1)
      outputT<-data.frame(variabele=Var,
                          strata="",
                          stratumNaam="",
                          periode=Periode,
                          minYear=min(DataSel$year),
                          maxYear=max(DataSel$year),
                          minReeks=min(DataSel$reeks),
                          maxReeks=max(DataSel$reeks),
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


# omzetten van link-variabelen naar uppercase
colnames_to_uppercase <- function(data){
  data_upper <- data %>% 
    rename(IDPlots = idplots,
           IDSegments = idsegments,
           Periode = periode,
           IDGroup = idgroup,
           StartPeriode = startperiode,
           DateVeg = dateveg, 
           Reeks = reeks,
           PlotWeight = plotweight,
           )
  
  # !! enkel als die variabelen effectief voorkomen!!
    
  
}


# v2 : subset maken van laatste 10 gemeten reeksen : obv van de afgewerkte reeksen (% itt Date, omdat DateDendro over het jaareinde kan gaan)
get_last_10y <- function(Data, Afgewerkt = afgewerkt) {
  maxreeksVBI3 <- max(afgewerkt)
  Data_y10 <- Data %>%
    filter(Periode == 2 & Reeks >= (maxreeksVBI3 + 1) | (Periode == 3 & Reeks <= maxreeksVBI3) ) %>%
    mutate(RY = paste0('vbi', Periode, '_', Reeks)) %>%
    arrange(Reeks) %>%
    mutate(VBI = Periode
           , fPeriode = as.factor(2)
           , Periode = 2) %>% #moet numeriek zijn voor My.WgtParEstimation en My.ParametersMB
    data.frame() 
  
  return(Data_y10)
}

# subset maken van laatste 20 gemeten reeksen 
# get_last_20y <- function(Data, Afgewerkt = afgewerkt) {
#   maxreeksVBI3 <- max(afgewerkt)
#   Data_P2 <- Data %>%
#     filter(Periode == 2 & Reeks >= (maxreeksVBI3 + 1) | (Periode == 3 & Reeks <= maxreeksVBI3) ) %>%
#     arrange(Reeks) %>%
#     mutate(RY = paste0('vbi', Periode, '_', Reeks)) %>%
#     arrange(Reeks) %>%
#     mutate(VBI = Periode
#            , fPeriode = as.factor(-10), Periode = -10)
#   
#   Data_P1 <- Data %>%
#     filter( (Periode == 2 & Reeks <= maxreeksVBI3) | (Periode == 1 & Reeks >= (maxreeksVBI3 + 1 )) ) %>%
#     mutate(RY = paste0('vbi', Periode, '_', Reeks)) %>%
#     arrange(Reeks) %>%
#     mutate(VBI = Periode
#            , fPeriode = as.factor(-20), Periode = -20)
#   
#   
#   Data_y20 <- rbind(Data_P1, Data_P2) %>%
#       data.frame()
#   
#   
#   return(Data_y20)
#  
# }

# v2 subset maken van laatste 20 gemeten reeksen --> PErIODE ipv -20 !!!!
get_last_20y <- function(Data, Afgewerkt = afgewerkt) {
  maxreeksVBI3 <- max(afgewerkt)
  Data_P2 <- Data %>%
    filter(Periode == 2 & Reeks >= (maxreeksVBI3 + 1) | (Periode == 3 & Reeks <= maxreeksVBI3) ) %>%
    arrange(Reeks) %>%
    mutate(RY = paste0('vbi', Periode, '_', Reeks)) %>%
    arrange(Reeks) %>%
    mutate(VBI = Periode
           , fPeriode = as.factor(2), Periode = 2)
  
  Data_P1 <- Data %>%
    filter( (Periode == 2 & Reeks <= maxreeksVBI3) | (Periode == 1 & Reeks >= (maxreeksVBI3 + 1 )) ) %>%
    mutate(RY = paste0('vbi', Periode, '_', Reeks)) %>%
    arrange(Reeks) %>%
    mutate(VBI = Periode
           , fPeriode = as.factor(1), Periode = 1)
  
  
  Data_y20 <- rbind(Data_P1, Data_P2) %>%
    data.frame()
  
  
  return(Data_y20)
  
}


get_lmer_output <- function(Model) {
  m_summary_coef_ <- as.data.frame(coef(summary(Model))) %>%
    dplyr::select(Estimate, Std_Error = `Std. Error`, t_value = `t value`, everything()) %>%
    mutate(CI_lwr = Estimate - 1.96*Std_Error
           , CI_upr = Estimate + 1.96*Std_Error
           , CI = paste0('(', round(CI_lwr,4), ' - ', round(CI_upr,4), ')')
    ) 
  
  
  
  
  
}


get_bosplots <- function(Data, afgewerkt) {
  bosplots_afgewerkt <- MeetnetDetails %>%
    filter(periode %in% c(1,2) | (periode == 3 & reeks %in% afgewerkt)) %>%
    # bos is hier bos sensu latu : bos en open ruimte behorende tot bos, de oppervlakte bos zoals gerapporteerd
    filter(fase2_forest %in% c("foa", "forest") & accessibility == TRUE) %>%
    dplyr::select(IDPlots = idplots, Periode = periode) %>%
    inner_join(Data, by = c("IDPlots", "Periode"))

    return(bosplots_afgewerkt)
}


# enkel de plots in productieve bosopp. (wél kapvlakte, geen FOA)
get_prod_bosplots <- function(Data, afgewerkt) {
  bosplots_afgewerkt <- MeetnetDetails %>%
    filter(periode %in% c(1,2) | (periode == 3 & reeks %in% afgewerkt)) %>%
    # enkel forest want op plotniveau & foa is er toch geen dendro-opname gedaan. Plots irrelevant voor dendro-analyse
    filter(fase2_forest %in% c("forest") & accessibility == TRUE) %>% 
    dplyr::select(IDPlots = idplots, Periode = periode) %>%
    inner_join(Data, by = c("IDPlots", "Periode"))
  
  return(bosplots_afgewerkt)
}


get_strata <- function(Data) {
    tblDataStr <- tblData %>%
        left_join(StratStat, by = ('IDPlots')) %>%
        left_join(StratDyn, by = c("IDPlots", "IDSegments", "Periode"))
  
    return(tblDataStr)
  
}


get_prod_bosopp <- function(connectieResultaten, MeetnetDetails, afgewerkt) {
    timelaps20
  
    connectieResultaten <- odbcConnectAccess2007(dbResultaten_path)
    bosopp <- sqlFetch(connectieResultaten, "tblResultaten_toestand", rownames = F) %>%
      filter(scriptNaam == "1_a_bosopp_toestand.R" & variabele == "bosOppervlakte" & strata == "") %>%
      dplyr::select(-datum) %>%
      unique()
    odbcClose(connectieResultaten)
    
    bosopp1 <- bosopp %>% 
      filter(periode == 1) %>% 
      filter(str_detect(string = beschrijving, pattern = reeksinfo20) & strata == '') %>%
      unique() %>%
      dplyr::pull(gemiddelde)    
    
    bosopp2 <- bosopp %>% 
      filter(periode == 2) %>% 
      filter(str_detect(string = beschrijving, pattern = reeksinfo20) & strata == '') %>%
      unique() %>%
      dplyr::pull(gemiddelde)
    
    
    bosopp1; bosopp2 
    

    bosopp_productief <- MeetnetDetails  %>%
      filter(fase2_forest == 'foa') %>% 
      dplyr::select(IDPlots = idplots, Periode = periode, Reeks = reeks, everything()) %>%
      get_last_20y() %>%
      mutate(bosopp = ifelse(Periode == 1, bosopp1
                             , ifelse(Periode == 2, bosopp2, NA)))  %>%
      dplyr::select(periode = Periode, bosopp) %>%
      dplyr::group_by(periode, bosopp) %>%
      dplyr::summarise(OpenRuimte = n()) %>%
      dplyr::ungroup() %>%
      mutate(opp_prd_bos = bosopp - 50 * OpenRuimte,
             prop_prd_bos = opp_prd_bos/(opp_V*100), #opp_V in km2
             se_bos_prod = (prop_prd_bos * (1-prop_prd_bos)/sample_size_vbi)^0.5 * (opp_V*100), #opp_V in km2
             # bron berekeningswijze: addendum advies INBO A.3744 (GEG_LULUCF_TotaalVolumePerBoomSoort.Rmd)
             perc = (opp_prd_bos/bosopp)*100) %>%
      dplyr::select(periode, bosopp, prop_prd_bos, perc, opp_prd_bos, se_bos_prod)
    
    

    
    return(bosopp_productief)
  
 
  
}


# Alle bomen met een kwaliteitsmeting
get_quality_trees <- function (db = db3){
  
  query_quality<-"SELECT Quality_3.IDPlots
  , Quality_3.IDTrees_3
  , Trees_3.Species
  , qTreeSpecies.Value1
  , Trees_3.DBH_mm
  ,Trees_3.Perimeter_cm
  , Trees_3.Height_m 
  , Quality_3.StemH_first_brench_2cm
  , Quality_3.Branche_angle_10cm
  , Quality_3.Stemprofile
  , qStemprofile.Value1
  , Quality_3.StemDefects
  , qDefects.Value1
  FROM (((Quality_3 
          LEFT JOIN Trees_3 ON (Quality_3.IDPlots = Trees_3.IDPlots) 
          AND (Quality_3.IDTrees_3 = Trees_3.ID)) 
         LEFT JOIN qTreeSpecies ON Trees_3.Species = qTreeSpecies.ID) 
        LEFT JOIN qStemprofile ON Quality_3.Stemprofile = qStemprofile.ID) 
  LEFT JOIN qDefects ON Quality_3.StemDefects = qDefects.ID;"

  connect_db <- odbcDriverConnect(paste("DRIVER={Firebird/InterBase(r) driver};UID=SYSDBA;PWD=masterkey; DBNAME=", db))
  
  quality_trees <- sqlQuery(connect_db, query_quality, stringsAsFactors = FALSE)
  
  colnames(quality_trees) <- str_to_lower(colnames(quality_trees))
  
  odbcClose(connect_db)
  
  
  
  return(quality_trees)
}



mode_Nul <- function(x) {
  x <- 0
}

mode_NA <- function(x) {
  x <- NA
}

mode_F <- function(x) {
  x <- FALSE
}

mode_T <- function(x) {
  x <- TRUE
}

get_cdeNOMT <- function (db = db3){
  
  query_cde<-"SELECT * FROM
  qNEWORMISSINGTREE
  ;
  "
  
  connect_db <- odbcDriverConnect(paste("DRIVER={Firebird/InterBase(r) driver};UID=SYSDBA;PWD=masterkey; DBNAME=", db))
  
  cde <- sqlQuery(connect_db, query_cde, stringsAsFactors = FALSE)
  
  
  odbcClose(connect_db)
  
  
  
  return(cde)
}

