library(ComplexHeatmap)
library(ggspatial)
library(sf)
library(tidyverse)
library(ggpubr)
library(cowplot)

F6ASIR <- read_tsv("data-ASIR.txt")

world_map <- map_data("world") %>% filter(region != "Antarctica") %>%
  dplyr::rename(country=region) %>%
  mutate(country=str_replace(country,"Taiwan","China")) %>% 
  mutate(country=str_replace(country,"North Korea","Democratic People's Republic of Korea")) %>% 
  mutate(country=str_replace(country,"South Korea","Republic of Korea")) %>% 
  mutate(country=str_replace(country,"Russia","Russian Federation")) %>% 
  mutate(country=str_replace(country,"USA","United States of America")) %>% 
  mutate(country=str_replace(country,"Iran","Iran (Islamic Republic of)")) %>% 
  mutate(country=str_replace(country,"Vietnam","Viet Nam")) %>% 
  mutate(country=str_replace(country,"Laos","Lao People's Democratic Republic")) %>% 
  mutate(country=str_replace(country,"Syria","Syrian Arab Republic")) %>% 
  mutate(country=str_replace(country,"Moldova","Republic of Moldova")) %>% 
  mutate(country=str_replace(country,"Czech Republic","Czechia")) %>% 
  mutate(country=str_replace(country,"UK","United Kingdom")) %>%
  mutate(country=str_replace(country,"Ivory Coast","Cote d'Ivoire")) %>% 
  mutate(country=str_replace(country,"Democratic Republic of the Congo","Congo")) %>% 
  mutate(country=str_replace(country,"Republic of Congo","Congo")) %>%
  mutate(country=str_replace(country,"Tanzania","United Republic of Tanzania")) %>% 
  mutate(country=str_replace(country,"Venezuela","Venezuela (Bolivarian Republic of)")) %>%
  mutate(country=str_replace(country,"Bolivia","Bolivia (Plurinational State of)"))
#-------------------------------------- ASIR-----------------------------------
# ASIR 高
F6ASIR %>% separate(`2019-2`,into="ASIR",sep=" ") %>%
  select(location,ASIR) %>% mutate(ASIR=as.numeric(ASIR)) %>% arrange(desc(ASIR))

# ASIR 低
F6ASIR %>% separate(`2019-2`,into="ASIR",sep=" ") %>%
  select(location,ASIR) %>% mutate(ASIR=as.numeric(ASIR)) %>% arrange(ASIR)

# 获取相关国家经纬度
m1 <- world_map %>% 
  left_join(.,F6ASIR %>% separate(`2019-2`,into="ASIR",sep=" "),
            by=c("country"="location")) %>%
  mutate(ASIR=as.numeric(ASIR)) %>% 
  arrange(desc(ASIR)) %>% 
  filter(country !="Kiribati") %>% 
  filter(country %in% c("Poland","Australia","United States of America", # 低的
                        "Bangladesh","Bhutan","India")) %>% # 高的
  group_by(country) %>% 
  slice(which.max(ASIR)) %>% select(1,2,country,ASIR) %>% 
  arrange(desc(ASIR)) %>% select(-1,-2) %>% 
  as.data.frame() %>% ggtexttable(rows = NULL,theme = ttheme("mBlue"))

# ASIR 绘制地图
map1 <- world_map %>% 
  left_join(.,F6ASIR %>% separate(`2019-2`,into="ASIR",sep=" "),
            by=c("country"="location")) %>% 
  mutate(ASIR=as.numeric(ASIR)) %>% arrange(desc(ASIR)) %>% 
  filter(country !="Kiribati") %>% 
  ggplot()+
  geom_polygon(aes(x = long, y = lat,group = group,fill=ASIR),
               color = "black",size = 0.2,show.legend =T)+
  annotate("text",x =13,y =-50,hjust = 0.5,size =30,color="#999999",
           label="2019",alpha = .3)+
  scale_fill_gradientn(colours=colorRampPalette(c("#5eaaf5","#f4d963","red"))(10),na.value="grey80")+
  theme_void()+
  theme(plot.margin=unit(c(0,0,0,0),units="cm"),
        legend.title=element_blank(),
        legend.text=element_text(size=8),
        legend.position="bottom",legend.justification = c(0.5,1))+
  annotation_north_arrow(location="bl",pad_x=unit(0.15,"in"),pad_y=unit(4.6,"in"),
                         style=north_arrow_nautical(fill=c("grey40","white"),line_col="grey20"))+
  guides(fill=guide_colorbar(direction = "horizontal",
                             reverse = F,barwidth = unit(10,"cm"),
                             barheight = unit(0.5,"cm")))

ggdraw(map1) + draw_plot(m1,x=-0.37,y=-0.25,scale=0.001)

ggsave("ASIR-map.pdf",width=9.22,height=5.75,units="in",dpi=300)

