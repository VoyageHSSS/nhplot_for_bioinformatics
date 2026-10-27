rm(list=ls())
setwd("D:\\桌面\\SCI论文写作与绘图\\R语言绘图\\基础图形绘制\\平滑曲线折线图")
#加载R包
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(ggalt) # Extra Coordinate Systems, 'Geoms', Statistical Transformations,
library(RColorBrewer) # ColorBrewer Palettes
library(grid) # The Grid Graphics Package
library(scales) # Scale Functions for Visualization
#读取数据
data <- read.delim("data.txt",
                       header=T, 
                       row.names=1, 
                       sep="\t",
                       stringsAsFactors = FALSE,
                       check.names = FALSE)

data
#赋予因子水平
data$Genus<-factor(
  data$Genus,
  levels=c("Bacillus","Cronobacter",
           "Enterobacterales",
           "Klebsiella","Pantoea",
           "Pseudomonas","Rosenbergiella"), 
  labels = c("Bacillus","Cronobacter",
             "Enterobacterales",
             "Klebsiella","Pantoea",
             "Pseudomonas","Rosenbergiella"))

data
#准备配色
col <- c("#85BA8F", "#A3C8DC",
              "#349839","#EA5D2D",
              "black","#F09594","#2072A8")
#背景色
color <- colorRampPalette(brewer.pal(11,"PuOr"))(30)
#普通折线图绘制
p=ggplot(data=data,
         aes(x=Compartment,y=RA,
             group=Genus,color=Genus))+
  geom_point(size=2.5)+
  labs(x="Compartments", y="Relative abundance (%)")+
  geom_line()+
  scale_x_discrete(limits=c("RS","RE","VE","SE","LE","P","BS"))+
  scale_colour_manual(values=col)+
  theme_bw() +
  theme(axis.text.x = element_text(size = 8),axis.text.y = element_text(size = 8))+
  theme(axis.title.y= element_text(size=12))+theme(axis.title.x = element_text(size = 12))+
  theme(legend.title=element_text(size=5),legend.text=element_text(size=5))+theme(legend.position = "bottom")
p
#平滑曲线的绘制
p2<-ggplot(data=data,aes(x=Compartment,y=RA,
                              group=Genus,color=Genus))+
  geom_point(size=2.5)+
  labs(x="Compartments", y="Relative abundance (%)")+
  geom_xspline(spline_shape = -0.5)+
  scale_x_discrete(limits=c("RS","RE","VE","SE","LE","P"))+
  scale_colour_manual(values=col)+
  theme_bw() +
  theme(axis.text.x = element_text(size = 8),axis.text.y = element_text(size = 8))+
  theme(axis.title.y= element_text(size=12))+theme(axis.title.x = element_text(size = 12))+
  theme(legend.title=element_text(size=5),legend.text=element_text(size=5))+theme(legend.position = "bottom")
p2

#拼图
library(patchwork) # The Composer of Plots

p+ p2 +
  plot_layout(guides = "collect")+
  plot_annotation(theme = theme(legend.position = "bottom"))


########拓展
##绘图模板
ggplot(data=data,aes(x=Compartment,y=RA,
                     group=Genus,color=Genus))+
  geom_point(size=3)+
  labs(x="Compartments", y="Relative abundance (%)")+
  geom_xspline(spline_shape = -0.25)+
  scale_x_discrete(limits=c("RS","RE","VE","SE","LE","P"))+
  scale_color_manual(values=col)+
  theme_bw() +
  theme(panel.grid=element_blank(),
        axis.text=element_text(color='#333c41',size=10),
        legend.text = element_text(color='#333c41',size=10),
        legend.title = element_blank(),
        legend.position = "bottom",
        axis.title= element_text(size=12))
#添加背景
grid.raster(alpha(color, 0.2), 
            width = unit(1, "npc"), 
            height = unit(1,"npc"),
            interpolate = T)
