#------------------------------------------------------------------------------*
# Load and prepare data from C4 using the most recent RedCap export
#------------------------------------------------------------------------------*


#------------------------------------------------------------------------------*
# Prepare analysis ----
#------------------------------------------------------------------------------*

# Load used packages
library(package = "tidyverse")

# Helper functions
as_time <- . %>% hms::parse_hm()




#------------------------------------------------------------------------------*
# Get data ----
#------------------------------------------------------------------------------*

# Get most recent export file
file <- list.files(
  path = "data/exports", pattern = "*.csv", full.names = TRUE
) %>%
  grep("vigilancia", ., ignore.case = TRUE, value = TRUE) %>%
  data_frame(
    file = .,
    set = case_when(
      grepl("piloto", file, ignore.case = TRUE) ~ "nuevo_piloto",
      grepl("VigilanciaGuate", file, ignore.case = TRUE) ~ "viejo_piloto"
    ),
    export_date = lubridate::ymd_hm(
      gsub("[^0-9]+_([-0-9_]+).csv", "\\1", .)
    )
  ) %>%
  group_by(set) %>%
  slice(which.max(export_date))


# Read in data
pneumo_visits_wide <- read_csv(
  file = file$file[file$set == "viejo_piloto"], col_types = cols(.default = "c")
)


# LUS data
lus_data_uvg <- read_csv(file = file$file[file$set == "nuevo_piloto"]) %>%
  select(
    record_id, c34_date, c34_normal, c34_interpretable,
    c34_pneumonia, c34_atelectasis, c34_interstitial
  ) %>%
  set_names(gsub("^c34_", "", names(.))) %>%
  mutate(record_id = as.character(record_id)) %>%
  filter(record_id != "349293")



#------------------------------------------------------------------------------*
# Clean data from original pilog ----
#------------------------------------------------------------------------------*

# Tidy data structure
pneumo_visits <- pneumo_visits_wide %>%
  mutate(record_id = as.character(record_id)) %>%
  filter(record_id != "99999") %>%
  # Collect all variables as rows
  gather(
    key = variable, value = value,
    -record_id, -redcap_repeat_instance, -redcap_repeat_instrument
  ) %>%
  # Fix variable names
  mutate(
    variable = variable %>%
      gsub("(_v[0-9])*(_v[0-9])$", "\\2", .) %>% # Keep last "visit suffix"
      gsub("(([0-9]_1)|([a-f][0-9]+))$", "\\1_v1", .) %>% # Add missing suffix
      gsub("^visita_([2-5])_(.*)", "\\2_v\\1", .) %>% # Move prefix
      gsub("([a-z])$", "\\1_v1", .) %>% # Add missing suffix
      gsub("(c36_gt_41).+(complete)", "\\1_\\2", .) %>% # Shorten complete name
      gsub("([a-f])([0-9]([^0-9]))", "\\1\t0\\2", .) %>% # Pad variable numnber
      gsub("\t", "", .)
  ) %>%
  # Separate variable name from visit suffix
  extract(
    col = variable, into = c("variable", "visit"), regex = "(.+)_v?([0-9])$"
  ) %>%
  # Structure as one row per household visit
  spread(key = variable, value = value) %>%
  filter(!is.na(record_id), !is.na(id)) %>%
  # Order variables
  select(
    record_id, id, visit, interviewer, c36_gt_41_complete,
    today, time, hora_fin,
    dateofbirth, age,
    municipio, matches("com_"),
    service_type = tipo_servicio,
    matches("[a-e][0-9]"), everything()
  ) %>%
  # Fix variable types
  mutate(
    today = lubridate::ymd(today),
    time = as_time(time),
    hora_fin = as_time(hora_fin),
    dateofbirth = lubridate::ymd(dateofbirth)
  ) %>%
  mutate_at(
    vars(record_id, age, c03, c04, c05, d01, d02, d03),
    funs(as.numeric)
  ) %>%
  # Fix communities
  # mutate(
  #   com_jalapa = factor(
  #     x = com_jalapa,
  #     levels = c(
  #       "2101308","2101283","2101127","2101045","2101207","2101006","2101181",
  #       "2101220","2101200","2101115","2101210","2101131","2101076","2101202",
  #       "2101072","2101035","2101351","2101173","2101138","2101216","2101132",
  #       "2101182","2101080","2101137","2101151","2101104","2101175","2101020",
  #       "2101116","2101014","2101129","2101030","2101032","2101145","2101081",
  #       "2101002","2101238","2101070","2101036","2101083","2101074","2101123",
  #       "2101113","2101114","2101039","2101241","2101251","2101134","2101037",
  #       "2101157","2101203","2101195","2101227","2101071","2101124","2101022",
  #       "2101133","2101021","2101128","2101150","2101253","2101027","2101208",
  #       "2101015","2101060","2101172","2101052","2101029","2101117","2101031",
  #       "2101028","2101062","2101068","2101166","2101026","2101265","2101125",
  #       "2101023","2101147","2101010","2101188","2101224","2101065","2101007",
  #       "2101148","2101179","2101144","2101066","2101193","2101061","2101012",
  #       "2101064","2101105","2101053","2101205","2101004","2101009","2101025",
  #       "2101016","2101213","2101099","2101222","2101109","2101106","2101108",
  #       "2101232","2101187","2101102","2101110","2101011","2101120","2101196",
  #       "2101250","2101249","2101247","2101159","2101121","2101047","2101059",
  #       "2101003","2101215","2101201","2101158","2101255","2101252","2101063",
  #       "2101161","2101051","2101044","2101997","2101310","2101041","2101311",
  #       "2101312","2101352","2101314","2101315","2101180","2101362","2101355",
  #       "2101316","2101257","2101318","2101354","2101103","2101319","2101320",
  #       "2101321","2101361","2101322","2101323","2101324","2101325","2101326",
  #       "2101327","2101328","2101329","2101330","2101126","2101331","2101332",
  #       "2101333","2101334","2101335","2101336","2101337","2101338","2101339",
  #       "2101359","2101340","2101341","2101342","2101343","2101344","2101346",
  #       "2101350","2101270","2101090","2101254","2101347","2101235","2101017",
  #       "2101034","2101139","2101256","2101018","2101240","2101309","2101268",
  #       "2101239","2101033","2101348","2101358","2101111","2101194","2101357",
  #       "2101360","2101048","2101293","2101353","2101349","2101001","999"
  #     ),
  #     labels = c(
  #       "SANTA CRUZ","EL MILAGRO","LAS JOYITAS","LAS GALERAS","LAS PEAS",
  #       "ANSHIGUA","CARRIZALITO","SHICAL","LA PASTORIA","POTRERO CARRILLO",
  #       "LOS PINOS","SAN LORENZO","LA PUENTE","LA LAGUNA","LOS LIMARES",
  #       "EL MOJON","LAGUNA VERDE","SAN LUIS GUISHORO","SANTA ELENA","RIO FRIO",
  #       "SAN ANTONIO LA NORIA","EL CASCABILLAL Y EL COPALITO","LOS CIEGOS",
  #       "SAN IGNACIO","LA VENTURA","LLANO DE LA PUERTA","LOS MEZCALES",
  #       "CHAGUITE","POTRERO DEL BURRO","EL CARRIZAL","SAN JOSE","EL TALQUEZAL",
  #       "EL RODEO","TIERRA BLANCA","LAGUNETA LOS ACHIOTES O QUEBRADA HONDA",
  #       "ACHIOTES JUMAY","EL GUAJE","LOS TALPETATES","EL TABLON","LA TEJERA",
  #       "LOS IZOTES","SANSAYO","PATA GALANA","PALO VERDE","EL LIMON",
  #       "EL PINALITO","LA LAGUNILLA","SANSURUTATE","EL TERRERO I","YERBABUENA",
  #       "LAGUNETA EL SAPO","ITZACOBA","VOLCAN PAZ","LA PAZ","SASHICO",
  #       "EL DURAZNAL","SUQUINAY","DIVISADERO","SANYUYO","VOLCAN SANYUYO",
  #       "LAS MARIAS","EL PITO","LAS PIEDRAS","CERRO DE ALCOBA","LAZARETO",
  #       "LOS GONZALEZ","LA FUENTE DE LA MONTAA","EL DURAZNO","PINO GORDO",
  #       "EL ROBLAR","EL ARENAL","LA AURORA","LA LAGUNETA","AGUA ZARCA",
  #       "EL DURAZNITO","LAS CRUCES","SAN JOSE LA FUENTE","EL PARAISO","URLANTA",
  #       "EL MIRADOR","EL COYOTE","TRUJILLO","LOS LLANITOS","ARLOROMA",
  #       "URAYANSAPO","ARAISAPO","TATASIRIRE","LAS AZUCENAS","GOLFILLO",
  #       "LOS TABLONES","BUENA VISTA","LOMA DE ENMEDIO","MIRAFLORES",
  #       "LAS GUACAMAYAS","LAS JOYAS","ALTUPE","EL AGUACATE","EL BOSQUE",
  #       "CORONA","LA PIEDRONA","LOS CEDROS","SUQUINAY","MOJON DEL MUERTO",
  #       "MIRAMUNDO","MAL PASO","SAUSAL","EL CONFITERO","LAS MORITAS","ORCHOJ",
  #       "AGUIJOTES","QUEBRADITAS","JOYA GRANDE","LOS LOPEZ","EL ASTILLERO",
  #       "SHICAL","CARTAGO","RIO BLANCO","EL RODEO","JICALTEPEQUE","ASTILLERO",
  #       "RIO BLANCO ARRIBA","LA VICENTINA","LAS TAPIAS","LAGUNA VERDE","GARCIA",
  #       "LAS DELICIAS","VAREJONES","EL ZAPOTE","EL RETIRO","POBLACION DISPERSA",
  #       "SAN LORENZO","EL TIGRE","SAN FRANSISICO BELLA VISTA","LA ESMERALDA",
  #       "LINDA VISTA","INGENIO DE LOS FIERROS","CARTAGO","BETHANIA",
  #       "VILLA ADRIANA","SALFATE","LAS LOMITAS","CARRIZALITO","PARINAQUE",
  #       "EL CAFETAL","LLANO GRANDE","MOSQUITO","HATO VIEJO","INGENIO DE AYARZA",
  #       "VERDUGO","CUESTA GRANDE","EL JUTE","GRACIAS A DIOS","EL SAUZAL",
  #       "LA TOMA","EL VOLCAN","LA LAGUNA","LA CUCHILLA","TALQUETZAL",
  #       "SANSIRISAY","LA TEJERA","SAN FRANCISCO","SANCASTI","AGUA CALIENTE",
  #       "EL SITIO","CEBOLLIN","EL AGUACATE","LOS LLANITOS","RASTROJO LIMPIO",
  #       "EL CEDRO","EL INCIENSO","AGUA BLANCA","LAGUNA","LAS MARIAS",
  #       "LA CARBONERA","LA VENTANA","LAS CRUCESITAS","SAN FERNANDO O CAMELOT",
  #       "LAGUNA VERDE","JOSE LA CARBONERA","LAGUNA VERDE","AGUA BLANCA",
  #       "DOBLE R","HACIENDA LA PONDEROSA","SAN MIGUEL","LA CIENEGA",
  #       "SAN ISIDRO","EL CARMEN","EL CARMEN","EL CHAGUITE","LAS UVAS",
  #       "EL MANGUITO","LOS MEZCALES","EL AGUACATE","OJO DE AGUA",
  #       "INGENIO FIERRO","SABANETAS","COCALES","EL JUTILLO","ESTACON",
  #       "LA BOTIJA","SAN FRANSISCO POZA VERDE","JALAPA","OTRO"
  #     )
  #   )
  # ) %>%
  filter(
    # Only keep recorded visits
    !is.na(id),
    # Remove repeated visits
    grepl("^32", record_id) | visit == 1
  ) %>%
  mutate(
    group = case_when(
      a01 == 1 | a02 == 1 ~ "respiratory illnesses",
      !(a01 == 1 | a02 == 1) ~ "other illnesses",
      TRUE ~ "other illnesses"
    ),
    study = case_when(
      grepl("^32", record_id) ~ "household pneumonia surveillance",
      grepl("^34", record_id) ~ "clinic pneumonia surveillance",
      TRUE ~ NA_character_
    ),
    record_id = as.character(record_id),
    # Decode services
    service_type = recode_factor(
      service_type,
      `1` = "Hospital",
      `2` = "Health Center",
      `3` = "Health Post",
      .missing = "Health Center"
    )
  )

pneumo_measures <- pneumo_visits %>%
  # rename variables
  select(
    group, record_id, visit, pn_rr1 = c03, pn_rr2 = c04,
    pn_oxy_60 = d01, pn_oxy_90 = d02, pn_oxy_120 = d03
  )




pneumo_visits %>%
  filter(
    # Keep only those with "service visit id"
    grepl("^34", record_id)
  ) %>%
  count(group)




#------------------------------------------------------------------------------*
# Clean data for new pilot (starting on Aug 15 2018) ----
#------------------------------------------------------------------------------*

# Read in the data
pneumo_new_wide <- read_csv(file = file$file[file$set == "nuevo_piloto"]) %>%
  select(-matches("c34_")) %>%
  mutate(
    record_id = as.character(record_id)
  ) %>%
  set_names(
    gsub(
      pattern = "^([a-f])([0-9]([^0-9]|$))",
      replacement = "\\10\\2",
      x = names(.)
    )
  )


# Prepare visits frome new pilot
pneumo_visits_new <- pneumo_new_wide %>%
  mutate(
    group = case_when(
      a01 == 1 | a02 == 1 ~ "respiratory illnesses",
      !(a01 == 1 | a02 == 1) ~ "other illnesses",
      TRUE ~ "other illnesses"
    ),
    study = case_when(
      grepl("^32", record_id) ~ "household pneumonia surveillance",
      grepl("^3[345]", record_id) ~ "clinic pneumonia surveillance",
      TRUE ~ NA_character_
    ),
    record_id = as.character(record_id),
    # Decode services
    service_type = recode_factor(
      tipo_servicio,
      `1` = "Hospital",
      `2` = "Health Center",
      `3` = "Health Post",
      .missing = "Health Center"
    )
  )



# End of script
