if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(ggplot2, tidyverse, readxl, patchwork, ggiraph, ggbump, gt)

# Età per sesso ----------------------------------------------------------------
## Importazione dati -----------------------------------------------------------
### 2024
it24 <- read_excel('Dati/Italia - Popolazione residente.xlsx',
                   skip = 1) |> 
  slice(1:101) |> 
  as_tibble()     # pop residente Italia
um24 <- read_excel("Dati/Umbria - Popolazione residente.xlsx",
                   skip = 1) |> 
  slice(1:101) |> 
  as_tibble() # pop residente Umbria

### 2000
it00 <- read_excel('Dati/Italia - Ricostruzione intercensuaria della popolazione 1992-2001.xlsx',
                   skip = 1) |> 
  select(1:2 | '2000') |> 
  slice(1:303) |> 
  as_tibble()     # pop ricostruita Italia

um00 <- read_excel('Dati/Umbria - Ricostruzione intercensuaria della popolazione 1992-2001.xlsx',
                   skip = 1) |>
  select(1:2 | '2000') |> 
  slice(1:303) |> 
  as_tibble()     # pop ricostruita Italia

### data cleaning ----
it24 <-  it24 |> 
  rename('M' = `Totale maschi`,
         'F' = `Totale femmine`) |> 
  pivot_longer(cols = c('M', 'F', Totale), names_to = 'Sesso', values_to = 'Tot per genere') |> # long form
  mutate(Età = as.numeric(case_match(Età, '100 e oltre' ~ '100', .default = Età)))

um24 <-  um24 |> 
  rename('M' = `Totale maschi`,
         'F' = `Totale femmine`) |> 
  pivot_longer(cols = c('M', 'F', Totale), names_to = 'Sesso', values_to = 'Tot per genere') |>  # long form
  mutate(Età = as.numeric(case_match(Età, '100 e oltre' ~ '100', .default = Età)))

it00 <- it00 |> 
  mutate(Sesso = case_match(Sesso, 'Maschi' ~ 'M',
                            'Femmine' ~ 'F', .default = Sesso)) |> 
  mutate(Età = as.numeric(case_match(Età, '100 e oltre' ~ '100', .default = Età)))

um00 <- um00 |> 
  mutate(Sesso = case_match(Sesso, 'Maschi' ~ 'M',
                            'Femmine' ~ 'F', .default = Sesso)) |> 
  mutate(Età = as.numeric(case_match(Età, '100 e oltre' ~ '100', .default = Età)))

## Data export -----------------------------------------------------------------
write_rds(it00, 'Dati/Export/it00.rds')
write_rds(it24, 'Dati/Export/it24.rds')
write_rds(um00, 'Dati/Export/um00.rds')
write_rds(um24, 'Dati/Export/um24.rds')
write_rds(etadf, 'Dati/Export/etadf.rds')


# Fecondità --------------------------------------------------------------------
## Importazione dati ------------------------------------------------------------
tftCitt <- read_excel("Dati/TFT per cittadinanza.xlsx",
                      skip = 1, n_max = 496) |> 
  filter(Territorio == 'Umbria' | Territorio == 'Italia')
etaCitt <- read_excel("Dati/Età media al parto per cittadinanza.xlsx", 
                      skip = 1, n_max = 506) |> 
  filter(Territorio == 'Umbria' | Territorio == 'Italia')

FXit <- read_excel("Dati/Fx italiane.xlsx", 
                   skip = 1) |> 
  filter(Territorio == 'Umbria') |> 
  select(!Note & !`Fino a 14 anni` & !`50 anni e oltre`) |> 
  mutate(citt = 'Madri italiane')
FXstra <- read_excel("Dati/Fx straniere.xlsx", 
                     skip = 1) |> 
  filter(Territorio == 'Umbria') |> 
  select(!Note & !`Fino a 14 anni` & !`50 anni e oltre`) |> 
  mutate(citt = 'Madri straniere')

### Merge
fecdf <- full_join(tftCitt, etaCitt) |> 
  select(!Note) |> 
  pivot_longer(cols = `Tasso di fecondità totale, madri italiane`:`Età media al parto, tutte le madri`,
               names_to = 'var') |> 
  rename(anno = `Anno di evento`)

FX <- bind_rows(FXit, FXstra) |> 
  pivot_longer(cols = `15 anni`:`49 anni`, names_to = 'Età', values_to = 'FX') |> 
  mutate(Età = as.numeric(gsub(' anni', '', Età)))

rm(etaCitt, FXit, FXstra, tftCitt)

## Data export -----------------------------------------------------------------
write_rds(fecdf, 'Dati/Export/fecdf.rds')
write_rds(FX, 'Dati/Export/FX.rds')

# Indicatori -------------------------------------------------------------------
## Importazione dati -----------------------------------------------------------
nat <- read_excel("Dati/Indicatori_demografici.xls", 
                  sheet = "quoziente_di_natalità", skip = 1) |> 
  rename('geo' = '...1')
mort <- read_excel("Dati/Indicatori_demografici.xls", 
                   sheet = "quoziente_di_mortalità", skip = 1) |>
  rename('geo' = '...1')
cresc <- read_excel("Dati/Indicatori_demografici.xls", 
                    sheet = "crescita_naturale", skip = 1) |> 
  rename('geo' = '...1')
migInt <- read_excel("Dati/Indicatori_demografici.xls", 
                     sheet = "saldo_migratorio_interno", skip = 1) |> 
  rename('geo' = '...1')
migEst <- read_excel("Dati/Indicatori_demografici.xls", 
                     sheet = "saldo_migratorio_con_l_estero", skip = 1) |> 
  rename('geo' = '...1')
mig <- read_excel("Dati/Indicatori_demografici.xls", 
                  sheet = "saldo_migratorio_totale", skip = 1) |> 
  rename('geo' = '...1')

## Tibble Umbria (merge) -------------------------------------------------------
umdf <- nat |>
  filter(geo == 'Umbria') |> 
  mutate(geo = 'Quoziente di natalità')
umdf <- mort |> 
  filter(geo == 'Umbria') |> 
  mutate(geo = 'Quoziente di mortalità') |> 
  bind_rows(umdf)
umdf <- cresc |> 
  filter(geo == 'Umbria') |> 
  mutate(geo = 'Crescita naturale') |> 
  bind_rows(umdf)
umdf <- mig |> 
  filter(geo == 'Umbria') |> 
  mutate(geo = 'Saldo migratorio') |> 
  bind_rows(umdf)
umdf <- migInt |> 
  filter(geo == 'Umbria') |> 
  mutate(geo = 'Saldo migratorio interno') |> 
  bind_rows(umdf)
umdf <- migEst |> 
  filter(geo == 'Umbria') |> 
  mutate(geo = 'Saldo migratorio con l\'estero') |> 
  bind_rows(umdf)


umdf2 <- data.frame(t(umdf[-1])) # trasposiz
colnames(umdf2) <- umdf$geo # nomi variabili
umdf <- umdf2
rm(umdf2)
umdf <- rownames_to_column(umdf, var = 'anno')
# umdf$anno <- dmy(sprintf("01-01-%s",umdf$anno))

umts <- umdf |> 
  pivot_longer(cols = 2:7, names_to = 'var') |> 
  mutate(Territorio = 'Umbria')

## Tibble Italia (merge) -------------------------------------------------------
itdf <- nat |>
  filter(geo == 'ITALIA') |> 
  mutate(geo = 'Quoziente di natalità')
itdf <- mort |> 
  filter(geo == 'ITALIA') |> 
  mutate(geo = 'Quoziente di mortalità') |> 
  bind_rows(itdf)
itdf <- cresc |> 
  filter(geo == 'ITALIA') |> 
  mutate(geo = 'Crescita naturale') |> 
  bind_rows(itdf)
itdf <- mig |> 
  filter(geo == 'ITALIA') |> 
  mutate(geo = 'Saldo migratorio') |> 
  bind_rows(itdf)
itdf <- migInt |> 
  filter(geo == 'ITALIA') |> 
  mutate(geo = 'Saldo migratorio interno') |> 
  bind_rows(itdf)
itdf <- migEst |> 
  filter(geo == 'ITALIA') |> 
  mutate(geo = 'Saldo migratorio con l\'estero') |> 
  bind_rows(itdf)
rm(mig, nat, mort, migInt, migEst, cresc)

itdf2 <- data.frame(t(itdf[-1])) # trasposiz
colnames(itdf2) <- itdf$geo # nomi variabili
itdf <- itdf2
rm(itdf2)
itdf <- rownames_to_column(itdf, var = 'anno')
# itdf$anno <- dmy(sprintf("01-01-%s",itdf$anno))

itts <- itdf |> 
  pivot_longer(cols = 2:7, names_to = 'var') |> 
  mutate(Territorio = 'Italia')

## Tibble finale (join) ----
inddf <- full_join(umts, itts) |> 
  mutate(anno = as.numeric(
    gsub('2023*', '2023', anno, fixed = T)))
rm(itts, itdf, umts, umdf)

## Data export -----------------------------------------------------------------
write_rds(inddf, 'Dati/Export/inddf.rds')

# Mortalità --------------------------------------------------------------------
## Importazione dati -----------------------------------------------------------

tavMort <- read_csv("Dati/Tavole mortalità/datiregionalicompleti1974.csv") |> 
  mutate(anno = '1974')
tavMort <- read_csv("Dati/Tavole mortalità/datiregionalicompleti1979.csv") |> 
  mutate(anno = '1979') |> 
  bind_rows(tavMort)
tavMort <- read_csv("Dati/Tavole mortalità/datiregionalicompleti1984.csv") |> 
  mutate(anno = '1984') |> 
  bind_rows(tavMort)
tavMort <- read_csv("Dati/Tavole mortalità/datiregionalicompleti1989.csv") |> 
  mutate(anno = '1989') |> 
  bind_rows(tavMort)
tavMort <- read_csv("Dati/Tavole mortalità/datiregionalicompleti1994.csv") |> 
  mutate(anno = '1994') |> 
  bind_rows(tavMort)
tavMort <- read_csv("Dati/Tavole mortalità/datiregionalicompleti1999.csv") |> 
  mutate(anno = '1999') |> 
  bind_rows(tavMort)
tavMort <- read_csv("Dati/Tavole mortalità/datiregionalicompleti2004.csv") |> 
  mutate(anno = '2004') |> 
  bind_rows(tavMort)
tavMort <- read_csv("Dati/Tavole mortalità/datiregionalicompleti2009.csv") |> 
  mutate(anno = '2009') |> 
  bind_rows(tavMort)
tavMort <- read_csv("Dati/Tavole mortalità/datiregionalicompleti2014.csv") |> 
  mutate(anno = '2014') |> 
  bind_rows(tavMort)
tavMort <- read_csv("Dati/Tavole mortalità/datiregionalicompleti2019.csv") |> 
  mutate(anno = '2019') |> 
  bind_rows(tavMort)
tavMort <- read_csv("Dati/Tavole mortalità/datiregionalicompleti2020.csv") |> 
  mutate(anno = '2020') |> 
  bind_rows(tavMort)
tavMort <- read_csv("Dati/Tavole mortalità/datiregionalicompleti2023.csv") |> 
  mutate(anno = '2023') |> 
  select(!Informazioni) |> 
  bind_rows(tavMort)

## filtro Umbria ----
umbMort <- tavMort |> 
  filter(Regione == 'Umbria')

## Data export -----------------------------------------------------------------
write_rds(umbMort, 'Dati/Export/umbMort.rds')