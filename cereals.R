library(tidyverse)
library(stringr)
library(ggrepel)
library(scales)
library(viridis)
library(ggtext)
library(showtext) #this is the main package to use for ading my own fonts
library(sysfonts)
theme_set(theme_minimal(base_size = 24))

#upload my custom fonts
font_add(family = "Concourse", regular = "../Statistics/Extra-projects/Fonts/Concourse2Regular.ttf")
font_add(family = "ConcourseB", regular = "../Statistics/Extra-projects/Fonts/Concourse2Bold.ttf")
font_add(family = "HeliotropeI", regular = "../Statistics/Extra-projects/Fonts/Heliotrope6Italic.ttf")
showtext_auto()

#dats is from Our World in Data
cereals <- read.csv(file = "../Statistics/Extra-projects/Colon_cancer/share-of-energy-from-cereals-roots-and-tubers-vs-gdp-per-capita.csv", header = TRUE) %>%
  rename("Location" = 1) %>%
  rename("share_cereal" = 4) %>%
  rename("GDP" = 5) %>%
  filter(Year == 2016)

cereals<- cereals %>%
  mutate(Continent = case_when(
    Continent %in% c("Asia", "Oceania") ~ "Asia and Oceania",
    Continent %in% c("North America", "South America") ~ "Americas",
    TRUE ~ Continent
  )) %>%
  filter(!is.na(GDP) & Location != "World")%>%
  filter(!is.na(share_cereal))

#now we will plot
comb_dot_cer <- cereals %>%
  mutate(Continent = factor(Continent, levels = c("Africa", "Asia and Oceania", "Americas", "Europe")))%>%
  ggplot(aes(x= share_cereal, y = Continent, color = Continent)) +
  geom_jitter(aes(fill = Continent), size = 6, shape = 21, colour = "white", alpha = 0.8, height = 0.05)+
  stat_summary(aes(fill = Continent), fun.data = "mean_cl_boot", geom="point", size = 10, shape=18)+
  annotate(
    "text", x = 55, y = 4.5, family = "HeliotropeI", size = 5, color = "gray50", lineheight = .9,
    label = glue::glue("Cereals and roots make up, on average, 34% of the energy supply in Europe")
  ) +
  annotate(
    "text", x = 60, y = 1.5, family = "HeliotropeI", size = 5, color = "gray50", lineheight = .9,
    label = glue::glue("On average, diets in Africa consist of 60% cereals and roots")
  ) +
  scale_fill_viridis(option = "H", discrete=TRUE) +
  geom_text_repel(
    data=cereals %>% filter((share_cereal < 25 | share_cereal > 74)), # Filter data first
    aes(label = Location), size = 5, color = "black",
    min.segment.length = 0, #if set to Inf then lines are not printed
    box.padding = unit(0.5, "lines"),
    point.padding = unit(0.5, "lines"),
    #arrow = arrow(length = unit(0.01, 'npc')),
    max.overlaps = Inf)+
  ggtitle(str_wrap(
    "Cereal, root, and tuber consumption is <br>higher in <span style='color:#421952FF;'>Africa</span> than in <span style='color:#7A0403FF;'>Europe</span>",
    width = 30))+
  theme_minimal() +
  theme(text = element_text(size = 24, color = "#181716", family = "ConcourseB"),
        plot.title.position = "plot",
        plot.title = element_textbox_simple(size=40, face = "bold", margin = margin(b = 30), halign=0),
        axis.title.y = element_blank(),
        axis.text.y = element_text(margin = margin(l = 30, r = 10)),
        axis.title.x = element_text(hjust = 0.2, margin = margin(b= 20)),
        axis.text.x = element_text(margin = margin(t= -10, b= 30)),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(30, 30, 30, 30),
        plot.background = element_rect(fill = "#F1EFE6"),
        plot.caption.position = "plot",
        plot.caption = element_text(family = "Concourse", size = 12, hjust = 0, margin = margin(t = 20)))+
  scale_x_continuous(labels=function(x) paste0(x,"%"), limits = c(20,90))+
  scale_y_discrete(labels = label_wrap(10))+
  labs(x = "Share of cereals, roots, and tubers in diet",
       caption = str_wrap("Dietary energy supply (does not account for food waste and home-produced food): Food and Agriculture Organization of the United Nations (2023) with major processing by Our World in Data. Data from 2016. Created by Elena Shekhova @EShekhova", 120),
  )
comb_dot_cer

#now let's add png and additional annotations

library(png) #these 2 packages are needed to attach png
library(grid)
img <- readPNG("pngegg.png") #this image is free and from https://www.pngegg.com/
g <- rasterGrob(img, interpolate=TRUE)

arrows <-
  tibble(
    x1 = c(50, 65),
    x2 = c(34, 60),
    y1 = c(4.4, 1.4),
    y2 = c(4, 1)
  )

comb_dot_cer_an <- comb_dot_cer +
  geom_curve(
    data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.07, "inch")), size = 0.4,
    color = "gray20", curvature = -0.3
  )+ annotation_custom(g,xmin = 60, xmax = 85, ymin = 2.1, ymax = 4.5)

comb_dot_cer_an
showtext_auto()
showtext_opts(dpi=300)
ggsave("comb_dot_cer.png", plot = comb_dot_cer_an, width = 3000, height = 3500, units = "px", dpi = 300)
