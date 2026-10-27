pacman::p_load(tidyverse,ggrepel,FactoMineR,magrittr,factoextra,RColorBrewer,vegan)
library(multcompView)
library(patchwork)

df <- read_tsv("data.xls") %>% dplyr::rename(sample="Sample_id") %>% 
  mutate(across("Subtype",str_replace,"-","_"))

pcoa <- vegdist(df %>% column_to_rownames(var="sample") %>% 
                  dplyr::select(-Subtype),method = "bray") %>% 
  pcoa(correction = "none", rn = NULL)

pcoadata <- data.frame(pcoa$vectors[,1],pcoa$vectors[,2]) %>% 
  set_colnames(c("PC1","PC2")) %>% 
  rownames_to_column(var="sample") %>% 
  left_join(.,df %>% dplyr::select(sample,Subtype),by="sample")
  
plot <- ggplot(pcoadata, aes(PC1, PC2)) +
  geom_point(aes(colour=Subtype,fill=Subtype),size=4)+
  scale_color_manual(values = colorRampPalette(brewer.pal(12,"Paired"))(4))+
  labs(x=paste0("(PC1: ",round(pcoa$values$Relative_eig[1]*100,2),"%)"),
       y=paste0("(PC2: ",round(pcoa$values$Relative_eig[2]*100,2),"%)"))+
  geom_vline(aes(xintercept = 0),linetype="dotted")+
  geom_hline(aes(yintercept = 0),linetype="dotted")+
  theme_bw()+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.x = element_text(colour="black",size = 12,margin = margin(t=5),face = "bold"),
        axis.title.y = element_text(colour="black",size = 12,margin = margin(r=5),face = "bold"),
        axis.text=element_text(color="black",face = "bold"),
        plot.title =element_blank(),
        legend.title = element_blank(),
        legend.key=element_blank(),   # 图例键为空
        legend.text = element_text(color="black",size=9,face = "bold"), # 定义图例文本
        legend.spacing.x=unit(0.1,'cm'), # 定义文本书平距离
        legend.key.width=unit(0.5,'cm'), # 定义图例水平大小
        legend.key.height=unit(0.5,'cm'), # 定义图例垂直大小
        legend.background=element_blank(), # 设置背景为空
        legend.box.background=element_rect(colour="black"), # 图例绘制边框
        legend.position=c(0.001,0.999),legend.justification=c(0.0001,1))

# 分别对PC1,PC2两个水平数据进行方差分析，并构建文本位置信息数据

cld1 <- multcompLetters4(aov(PC1 ~ Subtype,data=pcoadata),TukeyHSD(aov(PC1 ~ Subtype,data=pcoadata)))

dt1 <- pcoadata %>% group_by(Subtype) %>%
  dplyr::summarise(value_max=max(PC1),sd=sd(PC1)) %>% ungroup() %>% 
  arrange(desc(value_max))

text1 <- as.data.frame.list(cld1$Subtype) %>% rownames_to_column(var="Subtype") %>% 
  left_join(.,dt1,by="Subtype")


cld2 <- multcompLetters4(aov(PC2 ~ Subtype,data=pcoadata),TukeyHSD(aov(PC2 ~ Subtype,data=pcoadata)))

dt2 <- pcoadata %>% group_by(Subtype) %>%
  dplyr::summarise(value_max=max(PC2)) %>% ungroup() %>% 
  arrange(desc(value_max))

text2 <- as.data.frame.list(cld2$Subtype) %>% rownames_to_column(var="Subtype") %>% 
  left_join(.,dt2,by="Subtype")

# 绘制箱线图

p2 <- ggplot(pcoadata,aes(Subtype,PC1,fill=Subtype)) +
  geom_boxplot(outlier.shape = NA,width=0.5,color="black",linetype="dotted")+
  stat_boxplot(aes(ymin = ..lower.., ymax = ..upper..),outlier.shape = NA,width=0.5)+
  stat_boxplot(geom = "errorbar", aes(ymin = ..ymax..),width=0.2,size=0.35) +
  stat_boxplot(geom = "errorbar", aes(ymax = ..ymin..),width=0.2,size=0.35) +
  labs(x=NULL,y=NULL)+
  geom_text(data=text1,aes(label=Letters,y=value_max+0.0082),angle=-90,color="black",size=4)+
  scale_fill_manual(values = colorRampPalette(brewer.pal(12,"Paired"))(4))+
  theme_bw()+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        plot.title =element_blank(),
        legend.position = "non")+ coord_flip()

p3 <- ggplot(pcoadata,aes(Subtype,PC2,fill=Subtype)) +
  geom_boxplot(outlier.shape = NA,width=0.5,color="black",linetype="dotted")+
  stat_boxplot(aes(ymin = ..lower.., ymax = ..upper..),outlier.shape = NA,width=0.5)+
  stat_boxplot(geom = "errorbar", aes(ymin = ..ymax..),width=0.2,size=0.35) +
  stat_boxplot(geom = "errorbar", aes(ymax = ..ymin..),width=0.2,size=0.35) +
  labs(x=NULL,y=NULL)+
  geom_text(data=text2,aes(label=Letters,y=value_max+0.0055),angle=0,color="black",size=4)+
  scale_fill_manual(values = colorRampPalette(brewer.pal(12,"Paired"))(4))+
  theme_bw()+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        plot.title =element_blank(),
        legend.position = "non")


# adonis 分析
otu.adonis <- adonis2(df %>% dplyr::select(-Subtype) %>% column_to_rownames(var="sample")
                    ~ Subtype,data = pcoadata,distance = "bray")


p4 <- ggplot()+
  geom_text(aes(x=0,y = 0.1,label = paste("PERMANOVA:\ndf = ",otu.adonis$Df[1],
                                          "\nR2 =",round(otu.adonis$R2[1],5),
                              "\np-value = ",otu.adonis$`Pr(>F)`[1],sep="")),
            size=3.5,color="black",fontface="bold")+
  theme_bw()+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title=element_blank(),
        axis.ticks = element_blank(),
        axis.text=element_blank(),
        plot.title =element_blank(),
        legend.position = "non")

# 拼图
p2+p4+plot+p3 + 
  plot_layout(heights = c(1,4),widths = c(4,1),ncol = 2,nrow = 2)



