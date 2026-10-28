rm(list=ls())#clear Global Environment
setwd('D:\\桌面\\SCI论文写作与绘图\\R语言绘图\\基础图形绘制\\火山图')#设置工作路径

#安装所需R包
# install.packages("ggplot2")
# install.packages('ggrepel')
#加载包
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(ggrepel) # Automatically Position Non-Overlapping Text Labels with'
library(RColorBrewer) # ColorBrewer Palettes
library(grid) # The Grid Graphics Package
library(scales) # Scale Functions for Visualization

# 读取数据
df <- read.table(file="data.txt",sep="\t",header=T,check.names=FALSE)

#数据分类
df$group<-as.factor(ifelse(df$pvalue < 0.05 & abs(df$log2FoldChange) >= 2, 
                           ifelse(df$log2FoldChange>= 2 ,'up','down'),'NS'))
#标签
df$label<-ifelse(df$pvalue<0.05&abs(df$log2FoldChange)>=4,"Y","N")
df$label<-ifelse(df$label == 'Y', as.character(df$gene), '')
####绘制火山图
p <- ggplot(df, aes(log2FoldChange, -log10(pvalue),fill = group)) +
  geom_point(color="black",alpha=0.6, size=3,shape=21)+
  theme_bw()+
  theme(panel.grid=element_blank(),
        axis.text=element_text(color='#333c41',size=10),
        legend.text = element_text(color='#333c41',size=10),
        legend.title = element_blank(),
        axis.title= element_text(size=12))+
  geom_vline(xintercept = c(-2, 2), lty=3,color = 'black', lwd=0.8) + #辅助线
  geom_vline(xintercept = c(-4, 4), lty=3,color = 'red', lwd=0.8)+
  geom_hline(yintercept = -log10(0.05), lty=3,color = 'black', lwd=0.8) +
  scale_fill_manual(values = c('blue','grey','red'))+
  labs(title="volcanoplot",
       x = 'log2 fold change',
       y = '-log10 pvalue')+
  geom_text_repel(aes(x = log2FoldChange,#标签
                      y = -log10(pvalue),          
                      label=label),                       
                  max.overlaps = 10000,
                  size=3,
                  box.padding=unit(0.8,'lines'),
                  point.padding=unit(0.8, 'lines'),
                  segment.color='black',
                  show.legend=FALSE)
p

#背景色
color <- colorRampPalette(brewer.pal(11,"BrBG"))(30)
#添加背景
grid.raster(alpha(color, 0.2), 
            width = unit(1, "npc"), 
            height = unit(1,"npc"),
            interpolate = T)

