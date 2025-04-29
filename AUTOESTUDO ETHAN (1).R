library(WDI)
library(ggplot2)

# Dados
paises <- c('BR', 'US')

dadosenergia <- WDI(country = 'all', indicator = 'EG.ELC.ACCS.ZS')
dadosenergia2023 <- WDI(country = 'all', indicator ='EG.ELC.ACCS.ZS', start = 2023, end = 2023)
dadosenergiabrus <- WDI(country = paises, indicator ='EG.ELC.ACCS.ZS')
dadosenergiabr <- WDI(country = 'BR', indicator = 'EG.ELC.ACCS.ZS')

# Painel
grafpainel <- ggplot(dadosenergia, mapping = aes(y = EG.ELC.ACCS.ZS, x = year, color = country)) +
  geom_point() 

print(grafpainel)

# Corte transversal para 2023
grafcorte <- ggplot(dadosreceipt2023, mapping = aes(y = EG.ELC.ACCS.ZS, x = year, color = country)) +
  geom_point() +
  labs(title = "Receitas de Turismo Internacional - 2023",
       x = "Ano",
       y = "PIN") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none", 
        plot.title = element_text(family = "Arial", face = "bold", size = 18, color = "#3E3E3E"),
        axis.title = element_text(family = "Arial", size = 14, color = "#3E3E3E"),
        axis.text = element_text(family = "Arial", size = 12, color = "#3E3E3E")) +
  scale_color_viridis_d()  # Alterando para usar uma paleta de cores que se ajusta ao número de países

print(grafcorte)

# Série temporal para o Brasil
grafserie <- ggplot(dadosreceiptbr, mapping = aes(y = EG.ELC.ACCS.ZS, x = year)) +
  geom_line(color = "red") +
  labs(title = "Série Temporal de Receitas de Turismo Internacional - Brasil",
       x = "Ano",
       y = "PIN") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none", 
        plot.title = element_text(family = "Arial", face = "bold", size = 18, color = "#3E3E3E"),
        axis.title = element_text(family = "Arial", size = 14, color = "#3E3E3E"),
        axis.text = element_text(family = "Arial", size = 12, color = "#3E3E3E"))

print(grafserie)
