library(tidyverse)
library(ROMOPOmics)
library(DT)
library(knitr)
library(kableExtra)
library(here)
library(RSQLite)
library(shiny)
library(DBI)
library(GGally) #https://briatte.github.io/ggnet/
library(network)
library(sna)
library(data.table)

dirs          <- list()
dirs$base     <- file.path(here())
dirs$app      <- file.path(dirs$base,"app")
dirs$src      <- file.path(dirs$base,"src")
dirs$data     <- file.path(dirs$base,"data")
dirs$masks    <- file.path(dirs$base,"masks")
dirs$db_file  <- file.path(dirs$data,"sqlDB.sqlite")
renderDataTable <- DT::renderDataTable
dataTableOutput <- DT::dataTableOutput
as_tibble       <- tibble::as_tibble
#Source all functions in the src directory.
invisible(lapply(Sys.glob(file.path(dirs$src,"*.R")),source))

#Load data model, and make sure all names are unique.
dm      <- loadDataModel()

#Load masks.
msks    <- loadModelMasks(data_model = dm,mask_file_directory = dirs$masks)

#Append aliases for each mask to data model.
for(i in 1:length(msks)){
  col_nm  <- paste0(names(msks)[i],"_alias")
  dm      <- msks[[i]] %>%
              mutate(table=tolower(table)) %>%
              select(table,field,alias) %>%
              group_by(table,field) %>% 
              summarize(alias=paste(alias,collapse="\n")) %>%
              ungroup() %>%
              rename(!!as.name(col_nm) := alias) %>%
              merge(dm,.,all.x=TRUE) %>%
              as_tibble()
}

#If DB file doesn't exist, create it.
if(file.exists(dirs$db_file)){
  db  <- DBI::dbConnect(RSQLite::SQLite(), dirs$db_file)
}else{
  #Read existing data files into comprehensive input tables.
  #input_files       <- lapply(names(msks), function(x) Sys.glob(paste0(dirs$inputs[[x]],"/*\\.*sv")))
  #Find input files.
  input_files <- Sys.glob(paste0(dirs$data,"/*\\.[tc]sv"))
  names(input_files)<- gsub("\\.[tc]sv","",basename(input_files))
  {
    st_tm <- Sys.time()
  input_tables      <- list()
  for(mask in names(msks)){
    fls         <- input_files[[mask]]
    tbs         <- lapply(fls,readInputFiles,data_model=dm,mask_table=msks[[mask]])
    input_tables<- c(input_tables,tbs)
  }
    en_tm <- Sys.time()
    en_tm - st_tm
  }
  
  #Build DB and SQLite connection. For now these don't change after runtime.
  #input_table_list = input_table_list = input_tables
  db            <- combineInputTables(input_tables) %>%
                    buildSQLDBR(sql_db_file = dirs$db_file)
}

db_tabs       <- getDBTables(db,find_key = FALSE)
db_tabs       <- db_tabs[!grepl("^sqlite",names(db_tabs))]

#Key tables.
#For now, always inner_join all tables with at least one table depending on it.
db_key_tabs   <- getDependencies(data_model=dm,inc_tabs = tolower(names(db_tabs))) %>%
                  unlist(use.names=FALSE) %>% unique()

########################################
#Options
opt_dm_select_table     <- c("All",sort(unique(dm$table)))
opt_dm_select_table_def <- "All"
opt_dm_layout           <- c("adj","circle","circrand","eigen",
                             "fruchtermanreingold","geodist","hall","kamadakawai",
                             "mds","princoord","random","rmds","segeo","seham",
                             "spring","springrepulse","target")
opt_dm_layout_def       <- "circle"
opt_msk_layout          <- opt_dm_layout
opt_msk_layout_def      <- "fruchtermanreingold"
opt_mask_select         <- sort(names(msks))
opt_mask_select_def     <- opt_mask_select[2]
opt_db_mask_select      <- c("None",opt_mask_select)
opt_db_mask_select_def  <- "None"
opt_db_field_select     <- getFields(db_tabs,mask = "none")
opt_db_field_select_def <- c("person|person_id","person|gender_source_value","person|hla_source_value","sequencing|file_local_source","sequencing|file_remote_source_url")
opt_db_filt_select      <- opt_db_field_select
opt_db_filt_select_def  <- "person|hla_source_value"
opt_db_manual_def       <- 
'SELECT person_source_value, person.person_id,file_remote_repo_id,file_remote_repo_value
 FROM person INNER JOIN sequencing 
 WHERE file_remote_repo_id IS NOT NULL and person_source_value is "tcga-3c-aaau" and file_remote_repo_value is "tumor sample barcode"
 ORDER BY "file_remote_repo_value"'

#source(file.path(dirs$app,"scrap.R"))

