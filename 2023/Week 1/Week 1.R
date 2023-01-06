# ----- Load libraries ---- #
library(tidyverse)
library(RColorBrewer)
library(extrafont)
library(ggtext)

# ----- This section prepares and cleans our dataframe ---- #
db = read.csv(file.choose() , header = T)

# ----- This section cleans and prepares our dataframe for labels ---- #
db = db %>% mutate(shift = round(X2022 - X2002, 1),
         mn = round(((X2022 + X2002) / 2), 1)) %>% 
  arrange(desc(X2002)) %>% 
  mutate(lbl_2022 = paste0(X2022, ' Years'), 
         lbl_2002 = paste0(X2002, ' Years')) |> 
  mutate('2022' = X2022, '2002' = X2002)
db2 = db %>% slice_min(shift, n = 10) %>%  pivot_longer(8:9, names_to = "year", values_to = "life_expectancy") 

db2 = db2 |> 
  mutate(paired = rep(1:(n()/2),each=2), year=factor(year)) |> mutate(lifexp = life_expectancy) |> head(20)

lab_db = db |> filter(Country %in% db2$Country)
# ----- This section constructs the layers of our viz ---- #

ggplot( data = db2,aes(x= life_expectancy, y = reorder(Country, mn))) +
  # create a line plot
  geom_line(aes(group = paired),color="grey")+
  # create a point plot
  geom_point(aes(color=year), size=4) +
  #  Add labels to the center of each dumbbell
  geom_text(aes(label = Country, x = mn, family = "Tw Cen MT"), nudge_y = 0.3, size = 4.5) + 
  #  Add labels to the left of each dumbbell
  geom_text(data = lab_db, color= "black", size = 4.5, hjust = 1.5, aes(x=X2002, label= lbl_2002, family = "Tw Cen MT"))+
  #  Add labels to the right of each dumbbell
  geom_text(data = lab_db, color= "black", size = 4.5, hjust = -0.5,aes(x=X2022, label= lbl_2022, family = "Tw Cen MT")) +
  # Add a title and caption
  labs(title = "Top 10 African Countries by Life Expectancy Between <span style='color:#1f306e;'>2002</span> and <span style='color: #f5487f;'>2022<span>
       ",
       subtitle = '
       How has life expectancy changed over the last 20 years?
       ',
       caption = " 
       
       Data source : UNICEF | tidyverse R package | #TidyTuesday | @doh_bams ") + 
  # set x-axis label
  xlab('Life Expectancy in Years') +
  # Set a custom color scheme
  scale_color_manual(values = c("#1f306e","#f5487f")) +
  # Set x-axis limits
  xlim(c(44,82)) +
  # use the minimal theme to make it pretty
  theme_minimal()+
  # Set font style, size and color. Remove grid and y axis label. Format legend. Format plot.
  theme(plot.title = element_markdown(family = "Tw Cen MT", size = 20, hjust = 0.5, face = 'bold'),
        plot.caption = element_text(size = 18, face = "italic", hjust = 0.5, vjust = 0,  family = "Tw Cen MT"),
        plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5, vjust = 0,  family = "Tw Cen MT"),
        plot.background = element_rect(fill = "#FBFAF9"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(1,2,2,2, "cm"),
        axis.line = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 12, family = "Tw Cen MT", vjust = -1, face = 'bold'),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 10, family = "Tw Cen MT", vjust = -1),
        axis.ticks = element_blank()) +
  theme(legend.position= 'top',
        legend.title = element_blank(),
        legend.text = element_text(size = 12,family = "Tw Cen MT")) 

# save the plot

ggsave("ttuesday-week1-db-chart.png" , width = 12, height = 15, dpi = 1000)
