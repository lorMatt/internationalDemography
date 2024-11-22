if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(ggplot2, tidyverse, readxl, patchwork, ggiraph, ggbump, gt)

# Importazione dati ------------------------------------------------------------
## 2024
it24 <- read_excel('Dati/Italia - Popolazione residente.xlsx',
                    skip = 1) |> 
         slice(1:101) |> 
         as_tibble()     # pop residente Italia
um24 <- read_excel("Dati/Umbria - Popolazione residente.xlsx",
                    skip = 1) |> 
         slice(1:101) |> 
         as_tibble() # pop residente Umbria

## 2000
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

## data cleaning ----
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


# Piramidi delle età -----------------------------------------------------------

## Italia ----
### 2024
it24gg <- it24 |> 
  filter(Sesso != 'Totale') |>
  ggplot(aes(x = Età,
             y = ifelse(Sesso == 'M',
                        -`Tot per genere`, `Tot per genere`),
             fill = Sesso)) +
  geom_bar(stat = 'identity', width = 1) +
  coord_flip() +
  labs(subtitle = 'Italia 2024') +
  theme(axis.title.x = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_line(colour = 'gray90'),
        legend.title = element_blank())
  
it00gg <- it00 |> 
  filter(Sesso != 'Totale') |> 
  ggplot(aes(x = Età,
             y = ifelse(Sesso == 'M',
                        -`2000`, `2000`),
             fill = Sesso)) +
  geom_bar(stat = 'identity', width = 1) +
  coord_flip() +
  labs(subtitle = 'Italia 2000') +
  theme(axis.title.x = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_line(colour = 'gray90'),
        legend.title = element_blank())

## Umbria ----
### 2024
um24gg <- um24 |> 
  filter(Sesso != 'Totale') |>
  ggplot(aes(x = Età,
             y = ifelse(Sesso == 'M',
                        -`Tot per genere`, `Tot per genere`),
             fill = Sesso)) +
  geom_bar(stat = 'identity', width = 1) +
  coord_flip() +
  labs(subtitle = 'Umbria 2024') +
  theme(axis.title.x = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_line(colour = 'gray90'),
        legend.title = element_blank())

um00gg <- um00 |> 
  filter(Sesso != 'Totale') |> 
  ggplot(aes(x = Età,
             y = ifelse(Sesso == 'M',
                        -`2000`, `2000`),
             fill = Sesso)) +
  geom_bar(stat = 'identity', width = 1) +
  coord_flip() +
  labs(subtitle = 'Umbria 2000') +
  theme(axis.title.x = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_line(colour = 'gray90'),
        legend.title = element_blank())

## Patchwork ----
it00gg + um00gg + it24gg + um24gg +
  plot_layout(guides = 'collect', axes = 'collect') +
  plot_annotation(title = 'Piramidi delle età',
                  caption = 'Dati Demo.Istat',
                  theme = theme(plot.title = element_text(size = 18))) &
  theme(legend.position = 'bottom',
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_line(colour = 'gray90'),
        legend.title = element_blank(),
        plot.title.position = 'panel')

# Indici -----------------------------------------------------------------------

## Età media per genere ----

### Italia ----
#### 2024
etaMed <- it24 |> 
  group_by(Sesso) |> 
  summarise(etaMed = sum(`Età` * `Tot per genere`)/sum(`Tot per genere`)) |> 
  mutate(anno = 2024, geo = 'Italia')

#### 2000
etaMed <- it00 |> 
  group_by(Sesso) |> 
  summarise(etaMed = sum(`Età` * `2000`)/sum(`2000`)) |> 
  mutate(anno = 2000, geo = 'Italia') |> 
  bind_rows(etaMed)

### Umbria ----
### 2024
etaMed <- um24 |> 
  group_by(Sesso) |> 
  summarise(etaMed = sum(`Età` * `Tot per genere`)/sum(`Tot per genere`)) |> 
  mutate(anno = 2024, geo = 'Umbria') |> 
  bind_rows(etaMed)

### 2000
etaMed <- um00 |> 
  group_by(Sesso) |> 
  summarise(etaMed = sum(`Età` * `2000`)/sum(`2000`)) |> 
  mutate(anno = 2000, geo = 'Umbria') |> 
  bind_rows(etaMed)

### Dataviz ----
#### ggbump
etaMedgg <- etaMed |> 
  filter(Sesso != 'Totale') |> 
  ggplot(aes(x = anno, y = etaMed, col = Sesso, shape = geo, dataID = Sesso, tooltip = round(etaMed, 2))) +
  geom_bump(data = ~. |> filter(geo == 'Umbria'),
            linetype = 'dotdash',
            linewidth = .9) +
  geom_bump(data = ~. |> filter(geo == 'Italia'),
            linetype = 'dotted',
            linewidth = .9) +
  geom_point_interactive(size = 5) +
  geom_point_interactive(col = 'white', size = 3) +
  labs(title = 'Età media per area e per genere',
       caption = 'Dati Demo.Istat') +
  scale_y_continuous(limits = c(38,50)) +
  scale_x_continuous(breaks = c(2000, 2024)) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        axis.title = element_blank())

#### gt
etaMedGt <- etaMed |> 
  pivot_wider(names_from = Sesso, values_from = etaMed) |> 
  gt()

#### patchwork
etaMedgg / etaMedGt

girafe(ggobj = (etaMedgg / etaMedGt),
       options = list(
         opts_tooltip(css = 'background-color:white;
                      color:black;
                      font-family:Helvetica;
                      font-style:empty;
                      padding:8px;
                      border-radius:10px;
                      background:rgba(255, 255, 255, 0.8);'),
         opts_hover(css = ''),
         opts_toolbar(position = 'bottomleft'),
         opts_zoom(min = .7, max = 6)))

## Rapporto di mascolinità ----
### Umbria ----
rapMasc <- um24 |> 
  filter(Sesso != 'Totale') |> 
  group_by(Sesso) |> 
  summarise(pop = sum(`Tot per genere`)) |> 
  pivot_wider(names_from = Sesso, values_from = pop) |> 
  mutate(rapMasc = 100*(`M`/`F`),
         anno = 2024,
         geo = 'Umbria') |> 
  select(anno, geo, rapMasc)

rapMasc <- um00 |> 
  filter(Sesso != 'Totale') |> 
  group_by(Sesso) |> 
  summarise(pop = sum(`2000`)) |> 
  pivot_wider(names_from = Sesso, values_from = pop) |> 
  mutate(rapMasc = 100*(`M`/`F`),
         anno = 2000,
         geo = 'Umbria') |> 
  select(anno, geo, rapMasc) |> 
  bind_rows(rapMasc)

### Italia ----
rapMasc <- it24 |> 
  filter(Sesso != 'Totale') |> 
  group_by(Sesso) |> 
  summarise(pop = sum(`Tot per genere`)) |> 
  pivot_wider(names_from = Sesso, values_from = pop) |> 
  mutate(rapMasc = 100*(`M`/`F`),
         anno = 2024,
         geo = 'Italia') |> 
  select(anno, geo, rapMasc) |> 
  bind_rows(rapMasc)

rapMasc <- it00 |> 
  filter(Sesso != 'Totale') |> 
  group_by(Sesso) |> 
  summarise(pop = sum(`2000`)) |> 
  pivot_wider(names_from = Sesso, values_from = pop) |> 
  mutate(rapMasc = 100*(`M`/`F`),
         anno = 2000,
         geo = 'Italia') |> 
  select(anno, geo, rapMasc) |> 
  bind_rows(rapMasc)

## Indici di dipendenza ----

### Totale ----
#### Umbria
indDip <- um24 |> 
  filter(Sesso == 'Totale') |> 
  summarise(dip = (colSums(um24 |> filter(Sesso == 'Totale' & Età < 15) |> select(`Tot per genere`)) +
              colSums(um24 |> filter(Sesso == 'Totale' & Età > 64) |> select(`Tot per genere`)))/
              colSums(um24 |> filter(Sesso == 'Totale' & Età %in% c(15:64) ) |> select(`Tot per genere`))) |> 
  mutate(anno = 2024,
         geo = 'Umbria')
indDip <- um00 |> 
  filter(Sesso == 'Totale') |> 
  summarise(dip = (colSums(um00 |> filter(Sesso == 'Totale' & Età < 15) |> select(`2000`)) +
                     colSums(um00 |> filter(Sesso == 'Totale' & Età > 64) |> select(`2000`)))/
              colSums(um00 |> filter(Sesso == 'Totale' & Età %in% c(15:64) ) |> select(`2000`))) |> 
  mutate(anno = 2000,
         geo = 'Umbria') |> 
  bind_rows(indDip)

#### Italia
indDip <- it24 |> 
  filter(Sesso == 'Totale') |> 
  summarise(dip = (colSums(it24 |> filter(Sesso == 'Totale' & Età < 15) |> select(`Tot per genere`)) +
                     colSums(it24 |> filter(Sesso == 'Totale' & Età > 64) |> select(`Tot per genere`)))/
              colSums(it24 |> filter(Sesso == 'Totale' & Età %in% c(15:64) ) |> select(`Tot per genere`))) |> 
  mutate(anno = 2024,
         geo = 'Italia') |> 
  bind_rows(indDip)

indDip <- it00 |> 
  filter(Sesso == 'Totale') |> 
  summarise(dip = (colSums(it00 |> filter(Sesso == 'Totale' & Età < 15) |> select(`2000`)) +
                     colSums(it00 |> filter(Sesso == 'Totale' & Età > 64) |> select(`2000`)))/
              colSums(it00 |> filter(Sesso == 'Totale' & Età %in% c(15:64) ) |> select(`2000`))) |> 
  mutate(anno = 2000,
         geo = 'Italia') |> 
  bind_rows(indDip)

### Giovanile ----
#### Umbria
indDipGiov <- um24 |> 
  filter(Sesso == 'Totale') |> 
  summarise(dipGiov = (colSums(um24 |> filter(Sesso == 'Totale' & Età < 15) |> select(`Tot per genere`))/ 
              colSums(um24 |> filter(Sesso == 'Totale' & Età %in% c(15:64) ) |> select(`Tot per genere`)))) |> 
  mutate(anno = 2024,
         geo = 'Umbria')
indDipGiov <- um00 |> 
  filter(Sesso == 'Totale') |> 
  summarise(dipGiov = (colSums(um00 |> filter(Sesso == 'Totale' & Età < 15) |> select(`2000`))/
              colSums(um00 |> filter(Sesso == 'Totale' & Età %in% c(15:64) ) |> select(`2000`)))) |> 
  mutate(anno = 2000,
         geo = 'Umbria') |> 
  bind_rows(indDipGiov)

#### Italia
indDipGiov <- it24 |> 
  filter(Sesso == 'Totale') |> 
  summarise(dipGiov = (colSums(it24 |> filter(Sesso == 'Totale' & Età < 15) |> select(`Tot per genere`)))/
              colSums(it24 |> filter(Sesso == 'Totale' & Età %in% c(15:64) ) |> select(`Tot per genere`))) |> 
  mutate(anno = 2024,
         geo = 'Italia') |> 
  bind_rows(indDipGiov)

indDipGiov <- it00 |> 
  filter(Sesso == 'Totale') |> 
  summarise(dipGiov = (colSums(it00 |> filter(Sesso == 'Totale' & Età < 15) |> select(`2000`)))/
              colSums(it00 |> filter(Sesso == 'Totale' & Età %in% c(15:64) ) |> select(`2000`))) |> 
  mutate(anno = 2000,
         geo = 'Italia') |> 
  bind_rows(indDipGiov)

### Vecchiaia ----
#### Umbria
indDipVec <- um24 |> 
  filter(Sesso == 'Totale') |> 
  summarise(dipVec = (colSums(um24 |> filter(Sesso == 'Totale' & Età > 64) |> select(`Tot per genere`))/ 
                     colSums(um24 |> filter(Sesso == 'Totale' & Età %in% c(15:64) ) |> select(`Tot per genere`)))) |> 
  mutate(anno = 2024,
         geo = 'Umbria')
indDipVec <- um00 |> 
  filter(Sesso == 'Totale') |> 
  summarise(dipVec = (colSums(um00 |> filter(Sesso == 'Totale' & Età > 64) |> select(`2000`))/
                     colSums(um00 |> filter(Sesso == 'Totale' & Età %in% c(15:64) ) |> select(`2000`)))) |> 
  mutate(anno = 2000,
         geo = 'Umbria') |> 
  bind_rows(indDipVec)

#### Italia
indDipVec <- it24 |> 
  filter(Sesso == 'Totale') |> 
  summarise(dipVec = (colSums(it24 |> filter(Sesso == 'Totale' & Età > 64) |> select(`Tot per genere`)))/
              colSums(it24 |> filter(Sesso == 'Totale' & Età %in% c(15:64) ) |> select(`Tot per genere`))) |> 
  mutate(anno = 2024,
         geo = 'Italia') |> 
  bind_rows(indDipVec)

indDipVec <- it00 |> 
  filter(Sesso == 'Totale') |> 
  summarise(dipVec = (colSums(it00 |> filter(Sesso == 'Totale' & Età > 64) |> select(`2000`)))/
              colSums(it00 |> filter(Sesso == 'Totale' & Età %in% c(15:64) ) |> select(`2000`))) |> 
  mutate(anno = 2000,
         geo = 'Italia') |> 
  bind_rows(indDipVec)

## Indice di vecchiaia ----

#### Umbria
indVec <- um24 |> 
  filter(Sesso == 'Totale') |> 
  summarise(vec = (colSums(um24 |> filter(Sesso == 'Totale' & Età > 64) |> select(`Tot per genere`))/ 
                     colSums(um24 |> filter(Sesso == 'Totale' & Età < 15) |> select(`Tot per genere`)))) |> 
  mutate(anno = 2024,
         geo = 'Umbria')
indVec <- um00 |> 
  filter(Sesso == 'Totale') |> 
  summarise(vec = (colSums(um00 |> filter(Sesso == 'Totale' & Età > 64) |> select(`2000`))/
                     colSums(um00 |> filter(Sesso == 'Totale' & Età < 15) |> select(`2000`)))) |> 
  mutate(anno = 2000,
         geo = 'Umbria') |> 
  bind_rows(indVec)

#### Italia
indVec <- it24 |> 
  filter(Sesso == 'Totale') |> 
  summarise(vec = (colSums(it24 |> filter(Sesso == 'Totale' & Età > 64) |> select(`Tot per genere`)))/
              colSums(it24 |> filter(Sesso == 'Totale' & Età < 15) |> select(`Tot per genere`))) |> 
  mutate(anno = 2024,
         geo = 'Italia') |> 
  bind_rows(indVec)

indVec <- it00 |> 
  filter(Sesso == 'Totale') |> 
  summarise(vec = (colSums(it00 |> filter(Sesso == 'Totale' & Età > 64) |> select(`2000`)))/
              colSums(it00 |> filter(Sesso == 'Totale' & Età < 15) |> select(`2000`))) |> 
  mutate(anno = 2000,
         geo = 'Italia') |> 
  bind_rows(indVec)

## Join ----
etadf <- full_join(indDip, indDipGiov) |> 
  full_join(indDipVec) |> 
  full_join(indVec) |> 
  full_join(rapMasc) |> 
  full_join(etaMed |>
              pivot_wider(names_from = `Sesso`,
                          values_from = etaMed,
                          names_prefix = 'etaMed_')) |> 
  select(anno, geo, etaMed_F, etaMed_M, etaMed_Totale, dipGiov, dipVec, dip, vec, rapMasc)

## GT ----
etadf |>
  mutate(Anno = anno,
         'Territorio' = geo,
         'Femmine' = round(etaMed_F, 2),
         'Maschi' = round(etaMed_M, 2),
         'Totale' = round(etaMed_Totale, 2),
         'Giovanile' = round(dipGiov, 2),
         'Vecchiaia' = round(dipVec, 2),
         'Complessivo' = round(dip, 2),
         'Indice di vecchiaia' = round(vec, 2),
         'Rapporto di mascolinità' = round(rapMasc, 2)
  ) |> 
  select(Territorio, Femmine, Maschi, Totale, Giovanile, Vecchiaia, Complessivo, `Indice di vecchiaia`, `Rapporto di mascolinità`) |> 
  gt(rowname_col = 'Territorio') |> 
  tab_stubhead(
    label = 'Anno'
  ) |> 
  tab_row_group(
    label = '2000',
    rows = c(1,3)
  ) |> 
  tab_row_group(
    label = '2024',
    rows = c(2,4)
  ) |> 
  tab_header(
    title = md('## Indici di popolazione'),
    subtitle = md('Per **anno** e **territorio**')
  ) |> 
  tab_spanner(
    label = 'Età media',
    columns = c(Femmine, Maschi, Totale)
  ) |> 
  tab_spanner(
    label = 'Indice di dipendenza',
    columns = c(Giovanile, Vecchiaia, `Complessivo`)
  ) |> 
  tab_source_note(
    source_note = 'Dati Demo.Istat'
  ) |> 
  cols_align(
    align = 'center',
    columns = 3:9
  ) |>
  cols_align(
    align = 'left',
    columns = 1:2
  )
# Data export ----
write_rds(it00, 'Dati/Export/it00.rds')
write_rds(it24, 'Dati/Export/it24.rds')
write_rds(um00, 'Dati/Export/um00.rds')
write_rds(um24, 'Dati/Export/um24.rds')
write_rds(etadf, 'Dati/Export/etadf.rds')
