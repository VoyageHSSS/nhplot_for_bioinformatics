rm(list=ls())#clear Global Environment
setwd('D:/桌面/SCI论文写作与绘图/R语言绘图/基础图形绘制/散点图+拟合曲线+分面')#设置工作路径

#加载R包
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(ggpmisc) # Miscellaneous Extensions to 'ggplot2'
library(ggpubr) # 'ggplot2' Based Publication Ready Plots
library(RColorBrewer) # ColorBrewer Palettes
library(grid) # The Grid Graphics Package
library(scales) # Scale Functions for Visualization
#读取数
df <- read.table(file="data.txt",sep="\t",header=T,check.names=FALSE)

######绘图#######
#自定义颜色
col<-c("#1e90ff", "#cb78a6","#e6cf00","#fe0076","#28e5a1")
#构建背景色
color1 <- colorRampPalette(brewer.pal(11,"PiYG"))(30)
#绘图
p1 <- ggplot()+
  geom_point(df,mapping=aes(x,y,color=group),size=2,alpha=0.7)+
  geom_smooth(df,mapping=aes(x,y),method = "lm",
              formula = y ~ x,se=F,
              linetype=1,alpha=0.5)+
  stat_cor(df,mapping=aes(x,y),method = "pearson",label.x = 0, label.y = 15,color="black",size=4)+
  stat_poly_eq(df,mapping=aes(x,y,label = ..eq.label..),
               formula = y ~ x, parse = T,color="black",
               geom = "text",label.x = 0,label.y = 10, hjust = 0,size=4)+
  scale_color_manual(values = col)+
  theme_classic()+
  theme(axis.text=element_text(color='black',size=12),
        legend.text = element_text(color='black',size=12),
        legend.title = element_blank(),
        legend.position = c(0.95,0.85),
        legend.background = element_blank())+
  labs(x=NULL,y=NULL)
p1
##分面
p2 <- ggplot()+
  geom_point(df,mapping=aes(x,y,color=group),size=2,alpha=0.5)+
  geom_smooth(df,mapping=aes(x,y,color=group),method = "lm",
              formula = y ~ x,se=F,
              linetype=1)+
  stat_cor(df,mapping=aes(x,y),color="black",method = "pearson",label.x = 0, label.y = 15,size=4)+
  stat_poly_eq(df,mapping=aes(x,y,label = ..eq.label..),color="black",
               formula = y ~ x, parse = T,
               geom = "text",label.x = 0,label.y = 10, hjust = 0,size=4)+
  scale_color_manual(values = col)+
  theme_classic()+
  theme(axis.text=element_text(color='black',size=12),
        legend.text = element_text(color='black',size=12),
        legend.title = element_blank())+
  labs(x=NULL,y=NULL)+
  facet_grid(~group, scales = "fixed")+
  theme(legend.position = "none",
        strip.background=element_blank(),
        strip.text = element_blank())
  
p2
#拼图
cowplot::plot_grid(p2,p1,ncol = 1)
#添加背景
grid.raster(alpha(color1, 0.1), 
            width = unit(1, "npc"), 
            height = unit(1,"npc"),
            interpolate = T)


