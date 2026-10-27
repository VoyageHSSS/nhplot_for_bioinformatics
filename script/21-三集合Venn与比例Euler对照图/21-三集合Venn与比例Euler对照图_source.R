rm(list=ls())#clear Global Environment
setwd('D:/桌面/SCI论文写作与绘图/R语言绘图/基础图形绘制/成比例Venn图')#设置工作路径

#安装包
# devtools::install_github("jolars/eulerr")
#加载R包
library(ggvenn) # Draw Venn Diagram by 'ggplot2' 
library(eulerr) # Area-Proportional Euler and Venn Diagrams with Ellipses


###读取数据,以OTU水平的丰度表为例
data <- read.table(file="otu.txt",sep="\t",header=T,check.names=FALSE,row.names = 1)
#查看前6行
head(data)
#组内合并
df <- data.frame(A=rowSums(data[,c(1:3)]),
                 B=rowSums(data[,c(4:6)]),
                 C=rowSums(data[,c(7:9)]))
head(df)
#创建空列表
df1 <- list()
#获取每个样本（组）所有的OTU
for (i in 1:length(colnames(df))){
  group<- colnames(df)[i]
  df1[[group]] <- rownames(df)[which(df[,i]!= 0)]
}

###如果个人数据并非OTU类型数据，可按照过程中产生的df或者df1准备数据


###基于ggvenn包绘制常规Venn图
ggvenn(df1,#数据列表
       fill_color = c("#0099e5", "#ff4c4c", "#34bf49"), # 填充色
       fill_alpha = 0.5,# 填充透明度
       show_percentage = T,#是否显示百分比
       digits = 1,#百分比的小数点位数
       set_name_color = "black", #标签颜色
       set_name_size = 5,# 标签字体大小
       text_color = "black",# 交集个数颜色
       text_size = 4, # 交集个数文字大小
       stroke_color = "white",#边缘线条颜色
       stroke_alpha = 0.5,# 边缘线条透明度
       stroke_size = 0.5,#边缘线条粗细
       stroke_linetype = "solid", #边缘线条,实线：solid;虚线：twodash longdash;点：dotdash dotted dashed;无：blank
       # columns = NULL,# 指定列名绘图，最多选择4个，NULL为默认全选
       # show_elements = F,# 当为TRUE时，显示具体的交集情况，而不是交集个数
       # label_sep = "\n"# 当show_elements = T时生效
       )->p1
p1

####基于eulerr包绘制成比例Venn图
plot(
  euler(
    df1,#数据
    shape = "circle"#图案的形状，ellipse、circle
  ),
  fills = list(fill = c( "#ff4c4c", "#0099e5", "#34bf49"),alpha=0.6), # 填充的颜色和透明度
  labels=list(col="black",#组名标签的颜色
              font=2, fontfamily = "serif",#组名标签的字体设置
              cex=1.5),#组名标签的大小
  edges = list(col = "white", lwd=2, lty=1), #图形边缘线条颜色及粗细设置
  quantities = list(type = c("counts","percent"),#标签显示类型，百分比和数字
                    cex=1),#数字大小
)->p2
p2

###拼图
library(patchwork)
p1+p2+
  plot_annotation(
    tag_levels = c('A', '1'), 
    tag_prefix = 'Fig. ',
    tag_sep = '.',
  ) & theme(plot.tag.position = c(0, 0.98),
            plot.tag = element_text(size = 15,
                                    hjust = 0, 
                                    vjust = 0,
                                    color="black")
  )



##参考：https://cran.r-project.org/web/packages/eulerr/vignettes/gallery.html