#设置工作环境
rm(list = ls())
setwd("D:/桌面/SCI论文写作与绘图/R语言绘图/基础图形绘制/气泡图+分组+边际数量统计")

##加载R包（没有安装相关包的同学可以先安装相应的R包）
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(reshape2) # Flexibly Reshape Data: A Reboot of the Reshape Package
library(stringr) # Simple, Consistent Wrappers for Common String Operations
library(aplot) # Decorate a 'ggplot' with Associated Information

##加载数据（随机编写，无实际意义）
data <- read.table("data.txt", header = 1, check.names = F, sep = "\t")
#整理数据
df <- melt(data)
df$sample <- factor(df$sample, levels = data$sample)
#利用正则表达式重新将分组信息添加进去
df$group <- str_sub(df$variable, 1, 1)

##绘制主体气泡图
p1 <- ggplot(df, aes(variable, sample))+
  #利用散点图绘制方式绘制气泡图
  geom_point(aes(fill = group, size = value), shape = 21, color = "black")+
  #调整气泡图大小范围
  scale_size_continuous(range = c(2, 15), guide = "none")+
  #调整图例
  guides(fill=guide_legend(override.aes = list(size=6,alpha=1)))+
  #主题相关设置
  theme_bw()+
  theme(axis.text = element_text(size = 10, color = "black"))+
  labs(fill = NULL, x = NULL, y = NULL)+
  #自定义颜色
  scale_fill_manual(values = c("#e64b50", "#dbc65d"))
p1

##绘制边际条形图
#使用aggregate函数统计X轴与Y轴上的数量
df1 <- aggregate(value ~ variable, df, sum)
#利用正则表达式重新将分组信息添加进去
df1$group <- str_sub(df1$variable, 1, 1)
df2 <- aggregate(value ~ sample, df, sum)
#绘图——X轴上的边际条形图
p2 <- ggplot(df1, aes(variable, value))+
  #绘制条形图
  geom_col(aes(fill = group))+
  #添加文字注释
  geom_text(aes(y = value-20, label = value))+
  #主题设置
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10, color = "black", vjust = 0),
        axis.title.y = element_text(size = 12, color = "black"),
        axis.ticks.x = element_blank())+
  labs(x = NULL, y = "number of x")+
  scale_y_continuous(expand = c(0,0))+
  #自定义颜色
  scale_fill_manual(values = c("#e64b50", "#dbc65d"))
p2
#绘图——Y轴上的边际条形图
p3 <- ggplot(df2, aes(sample, value))+
  #绘制条形图
  geom_col(fill = "#56c1ab")+
  #添加文字注释
  geom_text(aes(y = value-30, label = value))+
  #主题设置
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 10, color = "black", vjust = 0, angle = 270),
        axis.title.x = element_text(size = 12, color = "black"),
        axis.ticks.y = element_blank())+
  labs(x = NULL, y = "number of y")+
  scale_y_continuous(expand = c(0,0))+
  #翻转X轴与Y轴位置
  coord_flip()
p3

##拼图
p1%>%insert_top(p2, height = 0.3) %>% 
  insert_right(p3, width = 0.3)
