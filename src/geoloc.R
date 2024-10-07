# geoloc.R

## Script para geolocalizar os locais de votação. O arquivo do TSE tem uma precisão boa, 
## mas alguns locais não estão bem geolocalizados. Por isso, optamos por geolocalizar novamente
## todos os locais, pois não são muitos (680 para BH, JF e SD). Recomendação para uso com mais 
## locais: filtrar só aqueles com latitude/longitude igual a -1.


# setup ---------------------------------------------------------------------------------------

library(dplyr)
library(readr)



# download ------------------------------------------------------------------------------------

url_tse <- paste0("https://cdn.tse.jus.br/estatistica/sead/odsele/",
                  "eleitorado_locais_votacao/eleitorado_local_votacao_2024.zip")

download.file(url_tse, "data-raw/db_eleitorado_2024.zip")

archive::archive_extract("data-raw/db_eleitorado_2024.zip", "data-raw")



# geolocalizar locais de voto -----------------------------------------------------------------

db_tse <- read_csv2("data/eleitorado_local_votacao_2024.csv", locale = locale(encoding = "latin1"))

db_tse <- db_tse %>% 
  filter(NM_MUNICIPIO %in% c("BELO HORIZONTE", "SANTOS DUMONT", "JUIZ DE FORA"))

db_locais <- db_tse %>% 
  distinct(NM_LOCAL_VOTACAO, DS_ENDERECO, NR_CEP, NM_MUNICIPIO) %>% 
  mutate(addr = paste0(DS_ENDERECO, " - ", NM_MUNICIPIO, ", ", NR_CEP))

db_locais <- db_locais %>% 
  tidygeocoder::geocode(address = addr, method = "here")

## consertar na mão escola mal geolocalizada em Santos Dumont
db_locais <- db_locais %>% 
  mutate(
    long = case_when(NM_MUNICIPIO == "SANTOS DUMONT" & 
                       NM_LOCAL_VOTACAO == "E.E. PADRE ANTÔNIO VIEIRA" ~ -43.53344,
                     .default = long),
    lat = case_when(NM_MUNICIPIO == "SANTOS DUMONT" & 
                       NM_LOCAL_VOTACAO == "E.E. PADRE ANTÔNIO VIEIRA" ~ -21.45232,
                     .default = lat),
  )


## salvar
db_locais %>% 
  saveRDS("data/shp/locais_voto.RDS")

# db_locais <- readRDS("data/shp/locais_voto.RDS")

 
 
# jogar a info de volta nas seções eleitorais -------------------------------------------------

db_tse <- db_tse %>% 
  select(AA_ELEICAO, DT_ELEICAO, DS_ELEICAO, NR_TURNO, NM_MUNICIPIO, NR_ZONA, NR_SECAO,
         NM_LOCAL_VOTACAO, DS_ENDERECO, NR_CEP) %>% 
  left_join(db_locais) %>% 
  select(-addr)

db_tse %>% 
  saveRDS("data/shp/secoes_geolocalizadas.RDS")



# check ---------------------------------------------------------------------------------------

library(sf)
library(ggplot2)

muni <- c(3106200, 3136702, 3160702) %>% 
  purrr::map(
    \(x) geobr::read_municipality(code_muni = x) %>% 
      st_transform(crs = 4326)
  )
  

db_locais <- db_locais %>% 
  mutate(name_muni = stringr::str_to_title(NM_MUNICIPIO)) %>% 
  st_as_sf(crs = 4326, coords = c("long", "lat"))

muni %>% 
  purrr::map(
    \(x)
    ggplot() +
      geom_sf(data = x, fill = NA) +
      geom_sf(data = db_locais %>% filter(name_muni %in% x$name_muni &
                                            NM_LOCAL_VOTACAO != "E.E. PADRE ANTÔNIO VIEIRA"), stroke = 0) +
      theme_void()
  )
