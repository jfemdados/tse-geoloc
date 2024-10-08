# geoloc.R

## Script para geolocalizar os locais de votação. O arquivo do TSE tem uma precisão boa, 
## mas alguns locais não estão bem geolocalizados. Por isso, optamos por geolocalizar novamente
## todos os locais, pois não são muitos (680 para BH, JF e SD). Recomendação para uso com mais 
## locais: filtrar só aqueles com latitude/longitude igual a -1.


# setup ---------------------------------------------------------------------------------------

library(dplyr)
library(readr)
library(purrr)



# download ------------------------------------------------------------------------------------

anos <- c(2022, 2024)

url_tse <- anos %>% 
  map(
    \(x) paste0("https://cdn.tse.jus.br/estatistica/sead/odsele/",
                "eleitorado_locais_votacao/eleitorado_local_votacao_", x, ".zip")
  ) %>% 
  set_names(anos)

anos %>% 
  map(
    \(x) download.file(url_tse[[as.character(x)]], paste0("data-raw/db_eleitorado_", x, ".zip"))
  )

list.files("data-raw", pattern = "db_eleitorado", full.names = T) %>% 
  map(archive::archive_extract, dir = "data-raw")

archive::archive_extract("data-raw/db_eleitorado_2022.zip", "data-raw")



# geolocalizar locais de voto -----------------------------------------------------------------

db_tse <- list.files("data-raw", pattern = "\\.csv$", full.names = T) %>%
  map(read_csv2, locale = locale(encoding = "latin1")) %>% 
  set_names(anos)

db_tse <- db_tse %>% 
  map(\(x) x %>% filter(NM_MUNICIPIO %in% c("BELO HORIZONTE", "SANTOS DUMONT", "JUIZ DE FORA")))


## aqui, criamos uma lista de endereços independentemente do ano. 
db_locais <- db_tse %>% 
  bind_rows() %>% 
  distinct(NM_LOCAL_VOTACAO, NR_LOCAL_VOTACAO, DS_ENDERECO, NR_CEP, NM_MUNICIPIO) %>% 
  mutate(addr = paste0(DS_ENDERECO, " - ", NM_MUNICIPIO, ", ", NR_CEP))


## para ATUALIZAR a lista, pule o restante da seção e vá para a próxima.


## geolocalizar
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



# opcional: atualizar a lista -----------------------------------------------------------------

## para incluir mais anos no futuro ou atualizar a lista, faça o seguinte:

### carregue os dados originais
db_locais_0 <- readRDS("data/shp/locais_voto.RDS")

### faça um anti_join para manter apenas os locais ausentes
db_locais <- anti_join(db_locais, db_locais_0)

### geolocalize o restante
db_locais <- db_locais %>% 
  tidygeocoder::geocode(address = addr, method = "here")

### junte tudo
db_locais <- bind_rows(db_locais_0, db_locais)


### e salve!
db_locais %>% 
  saveRDS("data/shp/locais_voto.RDS")

 
 
# jogar a info de volta nas seções eleitorais -------------------------------------------------

db_tse <- db_tse %>% 
  map(
    \(x) x %>% 
      select(AA_ELEICAO, DT_ELEICAO, DS_ELEICAO, NR_TURNO, NM_MUNICIPIO, NR_ZONA, NR_SECAO,
             NM_LOCAL_VOTACAO, NR_LOCAL_VOTACAO, DS_ENDERECO, NR_CEP) %>% 
      left_join(db_locais) %>% 
      select(-addr)
  )

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
