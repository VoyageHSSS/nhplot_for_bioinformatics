#设置工作环境
rm(list=ls())
setwd("D:/桌面/SCI论文写作与绘图/R语言绘图/基础图形绘制/3D曲线图+多分组")

##加载包
library(scatterplot3d) # 3D Scatter Plot
library(reshape2) # Flexibly Reshape Data: A Reboot of the Reshape Package

##加载数——以origin绘图软件模板快中的数据为例
df <- read.table("data.txt",header = T, check.names = F)

##数据格式转换
df1 <- melt(df, id.vars = c("Wavelength"), 
            measure.vars = c('Amplitude_1','Amplitude_2','Amplitude_3',
                             'Amplitude_4','Amplitude_5','Amplitude_6','Amplitude_7'))
#添加轨道列
df1$y <- rep(c(0.5,1.5,2.5,3.5,4.5,5.5,6.5),each = 512)


##绘图
#在第y=0.5上绘制Amplitude_1数据
p <- scatterplot3d(x=df1$Wavelength[1:512], y=df1$y[1:512], z=df1$value[1:512],
                   type = 'l',#图形类型
                   lwd=2,#线条粗细
                   scale.y=0.9,#y轴相对于x轴和z轴的刻度
                   color = "#0099e5",#颜色
                   y.ticklabs=c("",'Amplitude_1','Amplitude_2','Amplitude_3',
                                'Amplitude_4','Amplitude_5','Amplitude_6',
                                'Amplitude_7'),#更改y轴刻度标签
                   xlim = c(min(df1$Wavelength),max(df1$Wavelength)),
                   ylim = c(0, 7),zlim=c(0,2500),#设置各轴范围
                   y.axis.offset=0.5,#y轴刻度标签相对于轴的偏移位置
                   box = F,#是否显示框
                   grid = T,#是否显示网格
                   angle = 40,#调整角度,x轴和y轴之间的角度
                   xlab = paste0(colnames(df1)[1]," (nm)"),
                   ylab = '', zlab = 'Amplitude (mV)'#轴标题设置
                   )

#在第y=1.5上绘制Amplitude_2数据
p$points3d(df1$Wavelength[513:1024], df1$y[513:1024], df1$value[513:1024],
           type = 'l',col="#0099e5",lwd=2)#线条粗细
##同理，添加其他数据
p$points3d(df1$Wavelength[1025:1536], df1$y[1025:1536], df1$value[1025:1536],
           type = 'l',col="#ff4c4c",lwd=2)#线条粗细
p$points3d(df1$Wavelength[1537:2048], df1$y[1537:2048], df1$value[1537:2048],
           type = 'l',col="#ff4c4c",lwd=2)#线条粗细
p$points3d(df1$Wavelength[2049:2560], df1$y[2049:2560], df1$value[2049:2560],
           type = 'l',col="#34bf49",lwd=2)#线条粗细
p$points3d(df1$Wavelength[2561:3072], df1$y[2561:3072], df1$value[2561:3072],
           type = 'l',col="#34bf49",lwd=2)#线条粗细
p$points3d(df1$Wavelength[3073:3584], df1$y[3073:3584], df1$value[3073:3584],
           type = 'l',col="#7d3f98",lwd=2)#线条粗细

##添加图例
legend('topright',c('groupA','groupB','groupC','groupD'),
       col=c("#0099e5","#ff4c4c","#34bf49","#7d3f98"),
       lty=1,bty = 'n',lwd=2)


###后续细节需要借助AI或者PS进行调整
#如：y轴标签及其位置、刻度样式、文字大小等


######通过散点图方式一次性绘制
#根据分组添加颜色
colors <- c("#0099e5","#0099e5", "#ff4c4c", "#ff4c4c","#34bf49","#34bf49","#7d3f98")
colors <- colors[df1$variable]
p1 <- scatterplot3d(x=df1$Wavelength, y=df1$y, z=df1$value,
                    pch = 16,#散点类型
                    color = colors,#颜色
                    cex.symbols = 0.5,#散点大小
                    scale.y=0.9,#y轴相对于x轴和z轴的刻度
                    y.ticklabs=c("",'Amplitude_1','Amplitude_2','Amplitude_3',
                                 'Amplitude_4','Amplitude_5','Amplitude_6',
                                 'Amplitude_7'),#更改y轴刻度标签
                    xlim = c(min(df1$Wavelength),max(df1$Wavelength)),
                    ylim = c(0, 7),zlim=c(0,2500),#设置各轴范围
                    y.axis.offset=0.5,#y轴刻度标签相对于轴的偏移位置
                    box = F,#是否显示框
                    grid = F,#是否显示网格
                    angle = 40,#调整角度,x轴和y轴之间的角度
                    xlab = paste0(colnames(df1)[1]," (nm)"),
                    ylab = '', zlab = 'Amplitude (mV)'#轴标题设置
)
##添加图例
legend('topright',c('groupA','groupB','groupC','groupD'),
       col=c("#0099e5","#ff4c4c","#34bf49","#7d3f98"),
       lty=1,bty = 'n',lwd=2)
