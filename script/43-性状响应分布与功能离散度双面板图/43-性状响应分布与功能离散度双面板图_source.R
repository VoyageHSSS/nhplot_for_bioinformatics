rm(list=ls())#clear Global Environment
setwd('D:/桌面/SCI论文写作与绘图/R语言绘图/基础图形绘制/箱线图+点线图+显著性+分组')#设置工作路径

#加载R包
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(reshape2) # Flexibly Reshape Data: A Reboot of the Reshape Package
library(dplyr) # A Grammar of Data Manipulation
#加载数据
data1 <- read.table("data1.txt",header = 1,check.names = F,sep = "\t")
data2 <- read.table("data2.txt",header = 1,check.names = F,sep = "\t")

#数据清洗及处理
df1 <- melt(data1,id.vars = c("sample","group"))
df1$sample <- factor(df1$sample,levels = rev(data1$sample))
df1$group <- factor(df1$group,levels = c("A","B","C","D","E","F"))
df1$facet <- rep("Community-weighted trait means",times=132)
df2 <- melt(data2,id.vars = "sample")
df2$sample <- factor(df2$sample,levels = rev(data1$sample))
df2$group <- rep(c("Control","Warming"),each=66)#添加一列数据以区分Control和Warming组

###绘制第一张箱线图
#绘制基础箱线图
p <- ggplot(df1,aes(sample,value))+
  stat_boxplot(aes(color=group),geom = "errorbar", width=0.3,size=0.5)+#添加误差线
  geom_boxplot(aes(fill=group,color=group),outlier.shape = 18,size=0.5)#绘制箱线图
p
#计算均值位置并根据基础箱线图确定位置
df1 %>% 
  group_by(sample) %>% 
  summarise(mean_value=mean(value)) %>%
  cbind(ggplot_build(p)$data[[1]]) -> mean
#绘图
p1 <- ggplot(df1,aes(sample,value))+
  #在y轴为0的位置添加辅助线
  geom_hline(yintercept = 0, linetype = 2, color = "grey60",linewidth=0.8)+
  stat_boxplot(aes(color=group),geom = "errorbar", width=0.3,size=0.6)+#添加误差线
  geom_boxplot(aes(fill=group,color=group),outlier.shape = 18,size=0.6)+#绘制箱线图
  geom_segment(mean,
               mapping=aes(x=xmin-0.25,xend=xmax+0.25,y=mean_value,yend=mean_value),
               color="white",size=0.5)+
  #转变x轴与y轴位置
  coord_flip()+
  #自定义颜色
  scale_fill_manual(values = c("#e3ac6d","#9d7bb8","#6caf83","#d9586e","#3c74bb","#f85b2b"))+
  scale_color_manual(values = c("#e3ac6d","#9d7bb8","#6caf83","#d9586e","#3c74bb","#f85b2b"))+
  #y轴范围设置
  scale_y_continuous(limits = c(-100, 100))+
  #主题设置
  theme_bw()+
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.text = element_text(color = "black",size=10),
        strip.background = element_rect(fill = "grey", color = "transparent"),
        strip.text = element_text(color="black",size=10))+
  #标题位置
  labs(y="Response to warming (%)",x=NULL)+
  #添加分组矩形
  annotate("rect", xmin = 0, xmax = 6.5, ymin = -Inf, ymax = Inf, alpha = 0.2,fill="#d7ebce") +
  annotate("rect", xmin = 6.5, xmax = 20.5, ymin = -Inf, ymax = Inf, alpha = 0.2,fill="#bcced6") +
  annotate("rect", xmin = 20.5, xmax = 23, ymin = -Inf, ymax = Inf, alpha = 0.2,fill="#ffdc80")+
  #手动添加显著性标记，图中列出部分，具体根据个人数据进行调整
  annotate('text', label = '**', x =21, y =5, angle=-90, size =5,color="black")+
  annotate('text', label = '***', x =2, y =55, angle=-90, size =5,color="black")+
  annotate('text', label = '***', x =19, y =65, angle=-90, size =5,color="black")+
  annotate('text', label = '***', x =16, y =35, angle=-90, size =5,color="black")+
  annotate('text', label = '***', x =9, y =35, angle=-90, size =5,color="black")+
  annotate('text', label = '***', x =5, y =100, angle=-90, size =5,color="black")+
  facet_grid(~ facet)#基于分面函数添加图顶部标题
p1

###绘制第二张点线图
#计算均值
library(Rmisc) # Ryan Miscellaneous
mean2 <- summarySE(df2, measurevar = "value", groupvars = c("sample", "group"))
mean2
mean2$sample <- factor(mean2$sample,levels = rev(data2$sample))
mean2$facet <- rep("Functional diversity",times=44)
#绘图
p2 <- ggplot(mean2, aes(sample,value, color = group)) + 
  geom_errorbar(aes(ymin = value- se, ymax = value + se), 
                width = 0,position = position_dodge(0.8),linewidth=0.5) + 
  geom_point(position = position_dodge(0.8),shape=18,size=3)+
  #转变x轴与y轴位置
  coord_flip()+
  #y轴范围设置
  scale_y_continuous(limits = c(30, 110))+
  #主题设置
  theme_bw()+
  theme(legend.position = c(0.8,0.95),
        legend.background = element_blank(),
        legend.key = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_text(color = "black",size=10),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.background = element_rect(fill = "grey", color = "transparent"),
        strip.text = element_text(color="black",size=10))+
  #标题位置
  labs(y="Functional dispersion values",x=NULL,color=NULL)+
  scale_color_manual(values = c("#7fc190","#efb684"))+
  #添加分组矩形
  annotate("rect", xmin = 0, xmax = 6.5, ymin = -Inf, ymax = Inf, alpha = 0.2,fill="#d7ebce") +
  annotate("rect", xmin = 6.5, xmax = 20.5, ymin = -Inf, ymax = Inf, alpha = 0.2,fill="#bcced6") +
  annotate("rect", xmin = 20.5, xmax = 23, ymin = -Inf, ymax = Inf, alpha = 0.2,fill="#ffdc80")+
  #手动添加显著性标记，图中列出部分，具体根据个人数据进行调整
  annotate('text', label = '**', x =11, y =80, angle=-90, size =5,color="black")+
  geom_segment(x = 10.5, xend = 11.5, y = 78, yend = 78,color = "black", size = 0.8)+
  facet_grid(~ facet)#基于分面函数添加图顶部标题
p2
#拼图
library(aplot) # Decorate a 'ggplot' with Associated Information
p1%>%insert_right(p2,width = 1)

###最后用AI进行微调即可

