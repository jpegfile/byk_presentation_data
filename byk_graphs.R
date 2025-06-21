library(tidyverse)
library(scales)
library(patchwork)
library(MexBrewer)
library(paletteer)

#------------mdf - hdf---------------
df <- byk_2

df$m3 <- gsub(".", "", df$m3, fixed = TRUE) %>%
  as.numeric(df$m3)

cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")
names(cbPalette) <- levels(df$Country)
colScale <- scale_color_manual(name = "Country", values = cbPalette)

country_colors <- c(
  "CHINA" = "#E69F00",
  "TURKIYE" = "#56B4E9",
  "BRASIL" = "#009E73",
  "THAILAND" = "#F0E442",
  "RUSSIA" = "#0072B2",
  "GERMANY" = "#D55E00"
)

mdf <- ggplot(df, aes(x = reorder(Country, -m3), y = m3, fill = Country))+
  geom_bar(stat = "identity")+
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
  geom_text(aes(label = label_number(scale = 1e-6, suffix = "M")(m3)), vjust = - 0.4)+
  theme_bw()+
  xlab("")+
  ylab("Production (m3)")+
  theme(legend.position = "none")+
  labs(title = "MDF/HDF Production By Country (m3)")+
  scale_fill_manual(values = country_colors)

#-----------particle-------------
df2 <- byk_3

df2$m3 <- gsub(".", "", df2$m3, fixed = TRUE) %>%
  as.numeric(df2$m3)

cbPalette2 <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")
names(cbPalette2) <- levels(df2$Country)
colScale2 <- scale_color_manual(name = "Country", values = cbPalette2)

pb <- ggplot(df2, aes(x = reorder(Country, -m3), y = m3, fill = Country))+
  geom_bar(stat = "identity")+
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
  geom_text(aes(label = label_number(scale = 1e-6, suffix = "M")(m3)), vjust = - 0.4)+
  theme_bw()+
  xlab("")+
  ylab("Production (m3)")+
  theme(legend.position = "none")+
  labs(title = "MDF/HDF Production By Country (m3)")+
  scale_fill_manual(values = country_colors)

mdf + pb

#-------additives-----
df3 <- byk_4

df3 <- df3 %>%
  rename(Additives = Addetives,
         KG_Usage_Annual = Annual.Usage.kg.year)

df3$Usage  <- gsub(",", "", df3$Usage) %>%
  as.numeric(df3$Usage_Perc)

df3 <- df3 %>%
  mutate(Percentage = round((Usage/100), 2))

add1 <- ggplot(df3, aes(x = reorder(Additives, -KG_Usage_Annual), y = Percentage, fill = Additives))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = Percentage), vjust = -0.4)+
  theme_bw()+
  xlab("")+
  ylab("%")+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  labs(title = "Additive Usage (%)")+
  scale_fill_paletteer_d("MexBrewer::Alacena")

add2 <- ggplot(df3, aes(x = reorder(Additives, -KG_Usage_Annual), y = KG_Usage_Annual, fill = Additives))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = KG_Usage_Annual), vjust = -0.4)+
  theme_bw()+
  xlab("")+
  ylab("KG")+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  labs(title = "Additive Usage (Annual KG)")+
  scale_fill_paletteer_d("MexBrewer::Alacena")

add1 + add2
