
setwd("G:/BaiduNetdiskDownload/桑基图+气泡图展示富集分析结果")

library(tidyverse)
devtools::install_github("davidsjoberg/ggsankey")
library(ggsankey)
library(ggplot2)
install.packages("cols4all")
library(cols4all)


#====================================================================================
df <- read.csv("df.csv", header = T)
df1 <- df[,-1]
colnames(df1)
# [1] "gene"    "pathway" "freq" 
#将数据转换为绘图所需格式:
df1_trans <- df1 %>%make_long(gene, pathway)


#指定因子，调整显示顺序：
df1_trans$node <- factor(df1_trans$node,levels = c(df1$pathway %>% unique()%>% rev(),
                                     df1$gene %>% unique() %>% rev()))

colnames(df1_trans)


#作图
ggplot(df1_trans, aes(x = x,#相当于两列，一个是基因一个是pathway
                      next_x= next_x,
                      node= node,
                      next_node= next_node,
                      fill= node,
                      label= node)) +
  #设置桑葚图设置,没有dittoSeq包需要先安装一下
  #if (!require("BiocManager", quietly = TRUE))
  #install.packages("BiocManager")
  #BiocManager::install("SingleCellExperiment")
  #BiocManager::install("dittoSeq")
  geom_sankey(flow.fill="#DFDFDF",#连线颜色
              flow.color="grey60",#连线边框颜色
              node.fill=dittoColors()[1:44],#节点颜色
              width=0.15) + #node的宽度
  #设置桑葚图文字
  geom_sankey_text(size = 3,#文字大小
                   color= "black",#文字颜色
                   hjust=1) + #文字位置，右对齐
  theme_void()


#修饰
p1 = ggplot(df1_trans, aes(x = x,#相当于两列，一个是基因一个是pathway
                      next_x= next_x,
                      node= node,
                      next_node= next_node,
                      fill= node,
                      label= node)) +
  #设置桑葚图设置
  geom_sankey(flow.fill="#DFDFDF",#连线颜色
              flow.color="grey60",#连线边框颜色
              node.fill=dittoColors()[1:44],#节点颜色
              width=0.15) + #node的宽度
  #设置桑葚图文字
  geom_sankey_text(size = 3,#文字大小
                   color= c(rep("black",30),"red","red",rep("black",12)),#文字颜色
                   hjust=1) + #文字位置，右对齐
  theme_void()



#展示通路
df2 <- df[,-2]
df2_trans <- df2 %>%make_long(sub_path, pathway)
#作图
ggplot(df2_trans, aes(x = x,
                      next_x= next_x,
                      node= node,
                      next_node= next_node,
                      fill= node,
                      label= node)) +
  scale_fill_manual(values = dittoColors())+
  geom_sankey(width=0.15) + #node的宽度
  geom_sankey_text(size = 3,
                   color= "black") + 
  theme_void()+
  theme(legend.position = "none")



#====================================================================================
#气泡图
enrich <- read.csv("Enrichment.csv", header = T)
colnames(enrich)
enrich$Log.q.value. <- -enrich$Log.q.value.
enrich$Description <- factor(enrich$Description, levels = enrich$Description %>% rev())

p2  = ggplot(enrich, aes(Generatio, Description, color=Log.q.value.))+
  geom_point(aes(size=Count))+
  scale_color_gradient(low='#14B3FF',high='#E42A2A',name = "-Log(q-value)")+
  theme_bw()+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(colour = 'black',size = 10),
        axis.title.x = element_text(colour = 'black',size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
  



