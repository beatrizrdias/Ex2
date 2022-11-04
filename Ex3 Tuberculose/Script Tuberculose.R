setwd("C:\\Users\\BEATRIZ\\OneDrive\\Área de Trabalho\\estagio\\Ex3 Tuberculose")

library(tidyverse)
library(scales)
library(dplyr)
library(lubridate)
library(readr)
library(data.table)
library(sampling)
library(readr)
library(geojsonio)
library(rjson)
library(sp)
library(sf)
library(devtools)
library(leaflet)
library(RColorBrewer)
library(measurements)
library(cartography)
library(plotly)


#Criar datas 
dados1 <- read.csv2("ProjetoTuberculose_2022_DESA.csv", fileEncoding="latin1") %>% unique
View(dados1)

dados2 <- fread("ProjetoTuberculose_2022_DESA2.csv") %>% unique
View(dados2)

dados3 <- fread("ProjetoTuberculose_2022_DESA3.csv") %>% unique
View(dados3)

caract_exp <- fread("FicheiroTotalCaracterizacaoExploracoes-2022-10-04.csv") %>% unique
View(caract_exp)

ent_exp <- fread("FicheiroTotalEntidadesExploracoes-2022-10-04.csv") %>% unique
View(ent_exp)

tipo_ent_exp <- fread("FicheiroTotalTiposEntidadeExploracoes-2022-10-04.csv") %>% unique
View(tipo_ent_exp)

corre_freg <- fread("Correspondências freg 2013-14.xlsx - Correspondências freg 2013-14.csv") %>% unique
View(corre_freg)


#juntar PT ao ME dos dados 1 (=cod_me) para depois fazer coincidir

dados1 <- mutate(dados1, ME = paste("PT", dados1$ME, sep=""))
View(dados1)



#queremos dar merge ou left join nos dados 1,2,3 (dados1=codme; abate=dados3)

abates <- inner_join(dados3, dados1, by= c("ME_alt" = "Cód"))
tuberculo <- inner_join(dados2, dados1, by= c("ME_alt" = "Cód"))

#Isto é só para arrumar a casa
tuberculo <- select(tuberculo, -DiCo.x)
abates <- select(abates, -DiCo.x)



#juntar latitude e longitude e depois em baixo correr a função tirada da net para substituir a latitude e longitude pelas atualizadas

caract_exp <- mutate(caract_exp, LATITUDE = paste(caract_exp$CEX_GRA_N, caract_exp$CEX_MIN_N, caract_exp$CEX_SEG_N, sep = " "))
caract_exp <- mutate(caract_exp, LONGITUDE = paste(caract_exp$CEX_GRA_W, caract_exp$CEX_MIN_W, caract_exp$CEX_SEG_W, sep = " "))

caract_exp$LATITUDE <- conv_unit(caract_exp$LATITUDE, "deg_min_sec", "dec_deg")
caract_exp$LONGITUDE <- conv_unit(caract_exp$LONGITUDE, "deg_min_sec", "dec_deg")

caract_exp <- mutate(caract_exp, LATITUDE = round(as.numeric(caract_exp$LATITUDE), digit = 5)) %>% 
  mutate(LONGITUDE = round(as.numeric(caract_exp$LONGITUDE), digit = 5))

View(caract_exp)

#Organizar por data de alteração e tirar repetidos
caract_exp %>% arrange(desc(DAT_ALT)) %>% distinct(CEX_MAR_EXP, .keep_all = TRUE)
Total_Caract_Expl_ordenado <- unite(caract_exp, "DiCoFre", CEX_COD_DIS, CEX_COD_CON, CEX_COD_FRE, sep = "")

# Vou tentar tirar aqui apenas os que me interessa
select_Total_Caract_Expl_ordenado <- select(Total_Caract_Expl_ordenado, CEX_MAR_EXP, DiCoFre, LATITUDE, LONGITUDE)
names(select_Total_Caract_Expl_ordenado)[names(select_Total_Caract_Expl_ordenado) == 'CEX_MAR_EXP'] <- 'ME'

select_Total_Caract_Expl_ordenado$LATITUDE <- as.character(select_Total_Caract_Expl_ordenado$LATITUDE)
select_Total_Caract_Expl_ordenado$LONGITUDE <- as.character(select_Total_Caract_Expl_ordenado$LONGITUDE)

#Com o left_join, juntar as tabelas e obtemos todos os dados já com coordenadas certinhas e ME oficiais
abates1 <- left_join(abates, select_Total_Caract_Expl_ordenado)
tuberculo1 <- left_join(tuberculo, select_Total_Caract_Expl_ordenado)

View(abates1)
View(tuberculo1)
View(select_Total_Caract_Expl_ordenado)


#####VAMOS CRIAR MAPAS
#Primeiro carregar os mapas 
continente <- st_read("Cont_AAD_CAOP2015.shp") 
madeira <- st_read("ArqMadeira_AAd_CAOP2015.shp")
acores_central <- st_read("ArqAcores_GCentral_AAd_CAOP2015.shp")
acores_ocidental <- st_read("ArqAcores_GOcidental_AAd_CAOP2015.shp")
acores_oriental <- st_read ("ArqAcores_GOriental_AAd_CAOP2015.shp")

continente$geometry <- st_transform(continente$geometry, "+init=epsg:4326")

acores_central$geometry <- st_transform(acores_central$geometry, "+init=epsg:4326")

acores_oriental$geometry <- st_transform(acores_oriental$geometry, "+init=epsg:4326")

acores_ocidental$geometry <- st_transform(acores_ocidental$geometry, "+init=epsg:4326")

madeira$geometry <- st_transform(madeira$geometry, "+init=epsg:4326")



# Tabela com a soma de abates positivos por Dicofre com a geometria
pos_freguesia_ab <- abates1 %>% group_by(DiCoFre) %>% 
  summarise(Total_Ps = sum(`PM +`, na.rm = TRUE),
            .groups = 'drop')

# Por os 0 como NA para nao aparecerem no mapa
pos_freguesia_ab[pos_freguesia_ab == 0] <- NA

# Merge dos dois ficheiros para dar o ficheiro do mapa
continente_ab <- sp::merge(continente,pos_freguesia_ab, by.x="DICOFRE", by.y="DiCoFre")


# Palete de cores para abates
bins_ab <- c(0, 2, 4, 8, 16, 32, 64, 128, Inf)

pal_ab <- colorBin("Greens",continente_ab$Total_Ps, bins_ab, na.color = NA) 

# Texto para o pop up
mytext_ab <- paste(
  "<strong>", "Freguesia: ", "</strong>", continente_ab$Freguesia, "<br/>", 
  "<strong>", "Positivos: ", "</strong>", continente_ab$Total_Ps, "<br/>", 
  sep="") %>%
  lapply(htmltools::HTML)


#Mapa dos abates
portugal_abates <- leaflet(data = continente_ab) %>% 
  addProviderTiles(providers$CartoDB.DarkMatter) %>%  
  addPolygons(weight=.75, fillColor = ~pal_ab(Total_Ps), fillOpacity = .7, color = "black", dashArray = "",
              label = mytext_ab, labelOptions = labelOptions( 
                style = list("font-weight" = "normal", padding = "3px 8px"), 
                textsize = "13px",direction = "auto"), 
              highlightOptions = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE)) %>% 
  addLegend(pal = pal_ab, values = ~Total_Ps, opacity = 0.7, title = NULL,
            position = "bottomright")

portugal_abates



#Tabela com a soma de positivos ao IDTC para cada Dicofre com a geometry associada
pos_freguesia_IDTC <- tuberculo1 %>% group_by(DiCoFre) %>% 
  summarise(Total_Ps = sum(Ps, na.rm = TRUE),
            .groups = 'drop')

# Merge para base de dados do mapa
continente_IDTC <- sp::merge(continente,pos_freguesia_IDTC, by.x="DICOFRE", by.y="DiCoFre")

# Palete para IDTC
bins_IDTC <- c(0, 10, 20, 40, 80, 160, 320, 640, Inf)
pal_IDTC <- colorBin("Greens",continente_IDTC$Total_Ps, bins_IDTC)# Texto para o pop up
mytext_IDTC <- paste(
  "<strong>", "Freguesia: ", "</strong>", continente_IDTC$Freguesia, "<br/>", 
  "<strong>", "Positivos: ", "</strong>", continente_IDTC$Total_Ps, "<br/>", 
  sep="") %>%
  lapply(htmltools::HTML)

# Mapa de casos IDTC
portugal_IDTC <- leaflet(data = continente_IDTC) %>% 
  addProviderTiles(providers$CartoDB.DarkMatter) %>%  
  addPolygons(weight=.75, fillColor = ~pal_IDTC(Total_Ps), fillOpacity = .7, color = "black", dashArray = "",
              label = mytext_IDTC, labelOptions = labelOptions( 
                style = list("font-weight" = "normal", padding = "3px 8px"), 
                textsize = "13px",direction = "auto"), 
              highlightOptions = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE)) %>% 
  addLegend(pal = pal_IDTC, values = ~Total_Ps, opacity = 0.7, title = NULL,
            position = "bottomright")

portugal_IDTC


###################################TABELAS#################################################
# Tabela com todos os que ja foram positivos
# Tabela com as unicas colunas que nos interessam
tuberculo_2 <- select(tuberculo1, ME, DiCoFre, LATITUDE, LONGITUDE, Ps, Ano) %>% unique
abates_2 <- select(abates1, ME, DiCoFre, LATITUDE, LONGITUDE, `PM +`, DtAbate) %>% unique %>% 
  mutate(DtAbate =  as_datetime(DtAbate, format="%d/%m/%Y")) %>% mutate(DtAbate = year(DtAbate))

View(tuberculo_2)

# Passar os 0's para NA's
tuberculo_2[tuberculo_2 == 0] <- NA
abates_2[abates_2 == 0] <- NA

# Mudar os nomes para facilitar visualizacao
names(tuberculo_2)[names(tuberculo_2) == 'Ps'] <- 'Pos_IDTC'
names(abates_2)[names(abates_2) == 'PM +'] <- 'Pos_AB'

# Merge das duas tabelas
tabela_TB <- full_join(tuberculo_2, abates_2) 

# Filtrar a tabela para manter apenas as rows que tem um dos testes positivos
tabela_TB_1 <- tabela_TB[(tabela_TB$Pos_IDTC >= 1 | tabela_TB$Pos_AB >= 1),]  
View(tabela_TB_1)

# Criar duas novas colunas, uma que diz se a exploracao foi positiva no abate ou no IDTC
# e outra para 
tabela_TB_final <- tabela_TB_1 %>% 
  mutate(Ano_IDTC = case_when(Pos_IDTC >= 1 ~ tabela_TB_1$Ano)) %>% 
  mutate(Ano_AB = case_when(Pos_AB >= 1 ~ tabela_TB_1$DtAbate)) %>%
  mutate("Primeiro Ano" =pmin(Ano_IDTC, Ano_AB, na.rm = TRUE)) %>%
  mutate("Ultimo Ano" = pmax(Ano_IDTC, Ano_AB, na.rm = TRUE)) %>% 
  select(-Ano_IDTC, -Ano_AB) 

#Passar NA's para 0
tabela_TB_final[is.na(tabela_TB_final)] <- 0

#Criar a variavel Positivo com a soma dos positivos ao abate e ao IDTC, tirando as variaveis que nao interessam
try1 <- tabela_TB_final %>% group_by(ME) %>% 
  mutate(Positivo = Pos_IDTC+Pos_AB) %>%
  select(-Pos_AB, -Pos_IDTC) %>% select(-Ano, -DtAbate) %>%
  unique
View(try1)

#Estao ME's repetidos com contagens diferentes, vamos agrupar e somar para nao se perderem positivos quando se fizer arrange+distinct
try2 <- try1 %>% group_by(ME) %>% mutate(Positivo = sum(Positivo))
View(try2)

#For some reason preciso de continuar a fazer group_by, mas enfim. Arrange+distinct para tirar repetidos
try3 <- try2 %>% group_by(ME) %>% 
  arrange(desc("Ultimo Ano")) %>% distinct(ME, .keep_all = TRUE) %>% 
  arrange("Primeiro Ano") %>% distinct(ME, .keep_all = TRUE) %>% arrange("ME")
View(try3)
