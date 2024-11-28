if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(tidyverse, readxl, ggplot2)

# Importazione dati ------------------------------------------------------------
umPrev <- read_excel("Dati/Previsioni della popolazione.xlsx", 
                                           skip = 1)
umPrev <- umPrev |> 
  select(Anno, `Limite inferiore intervallo di confidenza al 90% (5° percentile)`,
         `Scenario mediano`,
         `Limite superiore intervallo di confidenza al 90% (95° percentile)`) |> 
  pivot_longer(cols = !Anno)

# Dataviz ----------------------------------------------------------------------

umPrev |> 
  
  ggplot(aes(x = Anno, y = value, group = name)) +
  geom_line(data = ~. |> filter(name == 'Scenario mediano'), linetype = 1) +
  geom_line(data = ~. |> filter(name == 'Limite inferiore intervallo di confidenza al 90% (5° percentile)'), linetype = 2) +
  geom_line(data = ~. |> filter(name == 'Limite superiore intervallo di confidenza al 90% (95° percentile)'), linetype = 2) +
  labs(title = 'Previsioni di popolazione al 2050',
       subtitle = 'Regione Umbria',
       caption = 'Dati Demo.Istat') +
  theme(legend.position = 'bottom',
        panel.background = element_blank(),
        panel.grid = element_line(colour = 'gray90'),
        legend.title = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

# Data export ----
write_rds(umPrev, 'Dati/Export/umPrev.rds')
