#------------------------------------------------------------------------------*
# Review surveillance effort
#------------------------------------------------------------------------------*


#------------------------------------------------------------------------------*
# Prepare environment ----
#------------------------------------------------------------------------------*

# Load used packages
library(package = "tidyverse")



#------------------------------------------------------------------------------*
# Get data from manual record ----
#------------------------------------------------------------------------------*

# Read in manual log
esfuerzo <- readxl::read_excel(
  path = "data/logs/Esfuerzo Vigilancia Neumonia.xlsx",
  skip = 1
) %>%
  # Fix names
  set_names(tolower(names(.))) %>%
  rename(personal = `personal hapin`)

# Prepare log
esfuerzo <- esfuerzo %>%
  # roll fill missing values
  mutate_at(
    vars(fecha, horario, personal),
    funs(zoo::na.locf(., na.rm = FALSE))
  ) %>%
  mutate(
    # Simplify dates
    fecha = as.Date(fecha),
    # Clean site categories
    sitio_vigilancia = case_when(
      sitio_vigilancia %in% c(
        "HOSPITAL", "C/S Jalapa", "CoEx", "Emergencia", "Pediatria", "Hospital Nacional"
      ) ~ "Hospital",
      sitio_vigilancia == "CAP Sanyuyo" ~ "Health Center",
      grepl("CAP", sitio_vigilancia) ~ "Health Center",
      grepl("(P/S|PUESTO|Centro)", sitio_vigilancia) ~ "Health Post",
      TRUE ~ sitio_vigilancia
    ),
    sitio_vigilancia = factor(
      sitio_vigilancia,
      levels = c("Hospital", "Health Center", "Health Post")
    ),
    # Clean communities
    comunidad = recode(
      comunidad,
      "Azucenas" = "Azucenas",
      "Buena Vista" = "Buena Vista",
      "Chagûite" = "Chagûite",
      "Divisadero" = "Divisadero",
      "Duraznal" = "Duraznal",
      "Durazno" = "Durazno",
      "El Durazno" = "El Durazno",
      "El Paraiso" = "El Paraiso",
      "El Rodeo" = "El Rodeo",
      "Emergencia" = "Jalapa",
      "hospital" = "Jalapa",
      "Hospital" = "Jalapa",
      "Itzacoba" = "Itzacoba",
      "Izotes" = "Izotes",
      "Jalapa" = "Jalapa",
      "La Fuente" = "La Fuente",
      "La Paz" = "La Paz",
      "Laguna del Pito" = "Laguna del Pito",
      "Laguneta" = "Laguneta",
      "Lagunilla palo verde" = "Lagunilla palo verde",
      "Palo verde" = "Palo verde",
      "Paraiso" = "Paraiso",
      "Pastoria" = "Pastoria",
      "Pediatria" = "Jalapa",
      "San José Carrizal" = "San José Carrizal",
      "San Miquel Mojon" = "San Miquel Mojon",
      "Sn. Miguel Mojon" = "San Miquel Mojon",
      "Sansirizay" = "Sansirizay",
      "Sansurutate" = "Sansurutate",
      "Sanyuyo" = "Sanyuyo",
      "Talquezal" = "Talquezal"
    )
  )


surveillance_effort_manual <- esfuerzo %>%
  select(
    date = fecha, municipality = municipio, community = comunidad,
    service_type = sitio_vigilancia, surveillance_personnel = personal,
    horario
  ) %>%
  separate(horario, into = c("start_time", "end_time"), sep = " *- *") %>%
  mutate_at(
    vars(start_time, end_time),
    funs(
      gsub(pattern = "[.]", replacement = ":", x = .) %>%
        gsub(pattern = " *", replacement = "", x = .) %>%
        paste0(":00") %>%
        hms::as.hms()
    )
  ) %>%
  mutate(
    surveillance_time = as.numeric(end_time - start_time, units = "hours")
  )



#------------------------------------------------------------------------------*
# Get data from RedCap record ----
#------------------------------------------------------------------------------*

redcap_effort <- list.files(
  path = "data/exports", pattern = "ESFUERZO", full.names = TRUE
) %>%
  tibble(
    file = .,
    time = lubridate::ymd_hm(gsub("[^0-9]+([-0-9_]+)[.]csv", "\\1", file))
  ) %>%
  slice(which.max(time)) %>%
  pull(file) %>%
  read_csv()


redcap_effort_harmonized <- redcap_effort %>%
  # de-code variable
  mutate(
    municipio = recode(
      municipio,
      `1` = "Jalapa"
    ),
    comunidad = recode(
      comunidad,
      `1` = "Jalapa",
      `2` = "Sanyuyo"
    ),
    hospital = recode(
      hospital,
      `1` = "CAP",
      `2` = "Hospital"
    )
  ) %>%
  gather(service, value, matches("services")) %>%
  mutate(
    service = recode(
      as.integer(gsub("services___", "", service)),
      `1` = "Emergencia",
      `2` = "Pediatria",
      `3` = "Consulta externa",
      `4` = "Maternidad",
      `5` = "Centro de salud"
    )
  ) %>%
  nest(service, value, .key = "services") %>%
  transmute(
    date = as.Date(time_start),
    municipality = municipio,
    community = comunidad,
    service_type = hospital,
    surveillance_personnel = encuestador,
    start_time = time_start %>% substr(12, 16) %>% paste0(":0") %>% hms::as.hms(),
    end_time = time_end
  ) %>%
  mutate(
    tt = start_time,
    start_time = if_else(
      condition = tt < end_time,
      true = start_time,
      false = end_time
    ),
    end_time = if_else(
      condition = tt > end_time,
      true = tt,
      false = end_time
    ),
    surveillance_time = as.numeric(end_time - start_time, units = "hours")
  ) %>%
  select(-tt)



# Join both
surveillance_effort <- surveillance_effort_manual %>%
  bind_rows(redcap_effort_harmonized) %>%
  # fixes
  mutate(
    date = case_when(
      lubridate::year(date) == 2108 ~ lubridate::`year<-`(date, 2018),
      date == as.Date("2017-01-24") ~ as.Date("2018-01-24"),
      TRUE ~ date
    )
  )


write_csv(
  x = surveillance_effort,
  path = "output/surveillance_effort.csv"
)


surveillance_effort %>%
  ggplot() +
  geom_tile(
    aes(x = date, y = start_time, width = 1, height = surveillance_time * 3600)
  )




# End of script
