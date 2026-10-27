rm(list=ls())#clear Global Environment
setwd("D:/桌面/SCI论文写作与绘图/R语言绘图/基础图形绘制/圆环图&旭日图")

### 基于webr包中的PieDonut函数进行绘制
## 安装R包
# install.packages("webr")
## 加载R包
library(webr) # Data and Functions for Web-Based Analysis
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics

##加载数据（随即编写，无实际意义）
df <- read.table("data.txt", header = 1, check.names = F, sep = "\t")
head(df)
##绘制一张简单的环状图
PieDonut(df,aes(group1,count=value),
         r0 = getOption("PieDonut.r0", 0.8),
         r1 = getOption("PieDonut.r1", 1.3),#设置中间镂空的比例
         labelpositionThreshold=0.1)#设置比例小于10%的数据标签显示在外侧

##绘制旭日图
PieDonut(df,aes(pies = group2, donuts = group1),
         r0 = 0.4,
         r1 = 1,
         r2 = 1.6,
         color = "white",#分割各区域的线条颜色
         pieAlpha = 0.8,#内层填充色的透明度
         donutAlpha = 0.8,#外层填充色的透明度
         start = pi/2,#起始位置
         addPieLabel = T,#是否显示内心圆的标签
         addDonutLabel = T,#是否显示外层的标签
         showRatioDonut = T,#外层标签的比例
         showRatioPie = T,#内层圆标签的比例
         ratioByGroup = F,#是否按分组计算外层比例
         showRatioThreshold = 0.02,#将标签显示为占总数比率的阈值，默认值为0.02。
         labelposition = 2,#设置外层标签位置的数字
         labelpositionThreshold = 0.1,#设置外层比例小于10%的数据标签显示在图形外
         explode = 1,#pies to explode,默认为NULL
         selected = 1,#donuts to explode,默认为NULL
         explodePie = T,#Whether or not explode pies
         explodeDonut = T,#Whether or not explode donuts
         explodePos = 0.1,#分裂的位置设置
         showPieName = T,#是否显示内层饼图名称
         showDonutName = F,#是否显示外层名称
         title = "This is a title",#标题,默认为NULL
         pieLabelSize = 4,#内层标签大小
         donutLabelSize = 3,#外层标签大小
         titlesize = 7,#标题字体大小
         family = getOption("PieDonut.family", "")#字体设置
         )

###PieDonut中没有直接修改颜色的参数，所以直接在原始函数定义中修改，以RColorBrewer中的调色板为例
##在其中新增一个修改调色板名称的参数man_color
library(moonBook) # Functions and Datasets for the Book by Keon-Woong Moon
library(grid) # The Grid Graphics Package
library(ggforce) # Accelerating 'ggplot2'
PieDonut2 <- function (data, mapping, start = getOption("PieDonut.start", 
                                                        0), addPieLabel = TRUE, addDonutLabel = TRUE, showRatioDonut = TRUE, 
                       showRatioPie = TRUE, ratioByGroup = TRUE, showRatioThreshold = getOption("PieDonut.showRatioThreshold", 
                                                                                                0.02), labelposition = getOption("PieDonut.labelposition", 
                                                                                                                                 2), labelpositionThreshold = 0.1, r0 = getOption("PieDonut.r0", 
                                                                                                                                                                                  0.3), r1 = getOption("PieDonut.r1", 1), r2 = getOption("PieDonut.r2", 
                                                                                                                                                                                                                                         1.2), explode = NULL, selected = NULL, explodePos = 0.1, 
                       color = "white", pieAlpha = 0.8, donutAlpha = 1, maxx = NULL, 
                       showPieName = TRUE, showDonutName = FALSE, title = NULL, 
                       pieLabelSize = 4, donutLabelSize = 3, titlesize = 5, explodePie = TRUE, 
                       explodeDonut = FALSE, use.label = TRUE, use.labels = TRUE, 
                       family = getOption("PieDonut.family", ""),
                       man_color="Set1")#输入调色板名称，具体可通过RColorBrewer::display.brewer.all()查看
{
  (cols = colnames(data))
  if (use.labels) 
    data = addLabelDf(data, mapping)
  count <- NULL
  if ("count" %in% names(mapping)) 
    count <- getMapping(mapping, "count")
  count
  pies <- donuts <- NULL
  (pies = getMapping(mapping, "pies"))
  if (is.null(pies)) 
    (pies = getMapping(mapping, "pie"))
  if (is.null(pies)) 
    (pies = getMapping(mapping, "x"))
  (donuts = getMapping(mapping, "donuts"))
  if (is.null(donuts)) 
    (donuts = getMapping(mapping, "donut"))
  if (is.null(donuts)) 
    (donuts = getMapping(mapping, "y"))
  if (!is.null(count)) {
    df <- data %>% group_by(.data[[pies]]) %>% dplyr::summarize(Freq = sum(.data[[count]]))
    df
  }
  else {
    df = data.frame(table(data[[pies]]))
  }
  colnames(df)[1] = pies
  df$end = cumsum(df$Freq)
  df$start = dplyr::lag(df$end)
  df$start[1] = 0
  total = sum(df$Freq)
  df$start1 = df$start * 2 * pi/total
  df$end1 = df$end * 2 * pi/total
  df$start1 = df$start1 + start
  df$end1 = df$end1 + start
  df$focus = 0
  if (explodePie) 
    df$focus[explode] = explodePos
  df$mid = (df$start1 + df$end1)/2
  df$x = ifelse(df$focus == 0, 0, df$focus * sin(df$mid))
  df$y = ifelse(df$focus == 0, 0, df$focus * cos(df$mid))
  df$label = df[[pies]]
  df$ratio = df$Freq/sum(df$Freq)
  if (showRatioPie) {
    df$label = ifelse(df$ratio >= showRatioThreshold, paste0(df$label, 
                                                             "\n(", scales::percent(df$ratio), ")"), as.character(df$label))
  }
  df$labelx = (r0 + r1)/2 * sin(df$mid) + df$x
  df$labely = (r0 + r1)/2 * cos(df$mid) + df$y
  if (!is.factor(df[[pies]])) 
    df[[pies]] <- factor(df[[pies]])
  df
  mainCol = RColorBrewer::brewer.pal(nrow(df), name=man_color)
  df$radius = r1
  df$radius[df$focus != 0] = df$radius[df$focus != 0] + df$focus[df$focus != 
                                                                   0]
  df$hjust = ifelse((df$mid%%(2 * pi)) > pi, 1, 0)
  df$vjust = ifelse(((df$mid%%(2 * pi)) < (pi/2)) | (df$mid%%(2 * 
                                                                pi) > (pi * 3/2)), 0, 1)
  df$segx = df$radius * sin(df$mid)
  df$segy = df$radius * cos(df$mid)
  df$segxend = (df$radius + 0.05) * sin(df$mid)
  df$segyend = (df$radius + 0.05) * cos(df$mid)
  df
  if (!is.null(donuts)) {
    subColor = makeSubColor(mainCol, no = length(unique(data[[donuts]])))
    subColor
    data
    if (!is.null(count)) {
      df3 <- as.data.frame(data[c(donuts, pies, count)])
      colnames(df3) = c("donut", "pie", "Freq")
      df3
      df3 <- eval(parse(text = "complete(df3,donut,pie)"))
      df3$Freq[is.na(df3$Freq)] = 0
      if (!is.factor(df3[[1]])) 
        df3[[1]] = factor(df3[[1]])
      if (!is.factor(df3[[2]])) 
        df3[[2]] = factor(df3[[2]])
      df3 <- df3 %>% arrange(.data$pie, .data$donut)
      a <- df3 %>% spread(.data$pie, value = .data$Freq)
      a = as.data.frame(a)
      a
      rownames(a) = a[[1]]
      a = a[-1]
      a
      colnames(df3)[1:2] = c(donuts, pies)
    }
    else {
      df3 = data.frame(table(data[[donuts]], data[[pies]]), 
                       stringsAsFactors = FALSE)
      colnames(df3)[1:2] = c(donuts, pies)
      a = table(data[[donuts]], data[[pies]])
      a
    }
    a
    df3
    df3$group = rep(colSums(a), each = nrow(a))
    df3$pie = rep(1:ncol(a), each = nrow(a))
    total = sum(df3$Freq)
    total
    df3$ratio1 = df3$Freq/total
    df3
    if (ratioByGroup) {
      df3$ratio = scales::percent(df3$Freq/df3$group)
    }
    else {
      df3$ratio <- scales::percent(df3$ratio1)
    }
    df3$end = cumsum(df3$Freq)
    df3
    df3$start = dplyr::lag(df3$end)
    df3$start[1] = 0
    df3$start1 = df3$start * 2 * pi/total
    df3$end1 = df3$end * 2 * pi/total
    df3$start1 = df3$start1 + start
    df3$end1 = df3$end1 + start
    df3$mid = (df3$start1 + df3$end1)/2
    df3$focus = 0
    if (!is.null(selected)) {
      df3$focus[selected] = explodePos
    }
    else if (!is.null(explode)) {
      selected = c()
      for (i in 1:length(explode)) {
        start = 1 + nrow(a) * (explode[i] - 1)
        selected = c(selected, start:(start + nrow(a) - 
                                        1))
      }
      selected
      df3$focus[selected] = explodePos
    }
    df3
    df3$x = 0
    df3$y = 0
    df
    if (!is.null(explode)) {
      explode
      for (i in 1:length(explode)) {
        xpos = df$focus[explode[i]] * sin(df$mid[explode[i]])
        ypos = df$focus[explode[i]] * cos(df$mid[explode[i]])
        df3$x[df3$pie == explode[i]] = xpos
        df3$y[df3$pie == explode[i]] = ypos
      }
    }
    df3$no = 1:nrow(df3)
    df3$label = df3[[donuts]]
    if (showRatioDonut) {
      if (max(nchar(levels(df3$label))) <= 2) 
        df3$label = paste0(df3$label, "(", df3$ratio, 
                           ")")
      else df3$label = paste0(df3$label, "\n(", df3$ratio, 
                              ")")
    }
    df3$label[df3$ratio1 == 0] = ""
    df3$label[df3$ratio1 < showRatioThreshold] = ""
    df3$hjust = ifelse((df3$mid%%(2 * pi)) > pi, 1, 0)
    df3$vjust = ifelse(((df3$mid%%(2 * pi)) < (pi/2)) | 
                         (df3$mid%%(2 * pi) > (pi * 3/2)), 0, 1)
    df3$no = factor(df3$no)
    df3
    labelposition
    if (labelposition > 0) {
      df3$radius = r2
      if (explodeDonut) 
        df3$radius[df3$focus != 0] = df3$radius[df3$focus != 
                                                  0] + df3$focus[df3$focus != 0]
      df3$segx = df3$radius * sin(df3$mid) + df3$x
      df3$segy = df3$radius * cos(df3$mid) + df3$y
      df3$segxend = (df3$radius + 0.05) * sin(df3$mid) + 
        df3$x
      df3$segyend = (df3$radius + 0.05) * cos(df3$mid) + 
        df3$y
      if (labelposition == 2) 
        df3$radius = (r1 + r2)/2
      df3$labelx = (df3$radius) * sin(df3$mid) + df3$x
      df3$labely = (df3$radius) * cos(df3$mid) + df3$y
    }
    else {
      df3$radius = (r1 + r2)/2
      if (explodeDonut) 
        df3$radius[df3$focus != 0] = df3$radius[df3$focus != 
                                                  0] + df3$focus[df3$focus != 0]
      df3$labelx = df3$radius * sin(df3$mid) + df3$x
      df3$labely = df3$radius * cos(df3$mid) + df3$y
    }
    df3$segx[df3$ratio1 == 0] = 0
    df3$segxend[df3$ratio1 == 0] = 0
    df3$segy[df3$ratio1 == 0] = 0
    df3$segyend[df3$ratio1 == 0] = 0
    if (labelposition == 0) {
      df3$segx[df3$ratio1 < showRatioThreshold] = 0
      df3$segxend[df3$ratio1 < showRatioThreshold] = 0
      df3$segy[df3$ratio1 < showRatioThreshold] = 0
      df3$segyend[df3$ratio1 < showRatioThreshold] = 0
    }
    df3
    del = which(df3$Freq == 0)
    del
    if (length(del) > 0) 
      subColor <- subColor[-del]
    subColor
  }
  p <- ggplot() + theme_no_axes() + coord_fixed()
  if (is.null(maxx)) {
    r3 = r2 + 0.3
  }
  else {
    r3 = maxx
  }
  p1 <- p + geom_arc_bar(aes_string(x0 = "x", y0 = "y", r0 = as.character(r0), 
                                    r = as.character(r1), start = "start1", end = "end1", 
                                    fill = pies), alpha = pieAlpha, color = color, data = df) + 
    transparent() + scale_fill_manual(values = mainCol) + 
    xlim(r3 * c(-1, 1)) + ylim(r3 * c(-1, 1)) + guides(fill = FALSE)
  if ((labelposition == 1) & (is.null(donuts))) {
    p1 <- p1 + geom_segment(aes_string(x = "segx", y = "segy", 
                                       xend = "segxend", yend = "segyend"), data = df) + 
      geom_text(aes_string(x = "segxend", y = "segyend", 
                           label = "label", hjust = "hjust", vjust = "vjust"), 
                size = pieLabelSize, data = df, family = family)
  }
  else if ((labelposition == 2) & (is.null(donuts))) {
    p1 <- p1 + geom_segment(aes_string(x = "segx", y = "segy", 
                                       xend = "segxend", yend = "segyend"), data = df[df$ratio < 
                                                                                        labelpositionThreshold, ]) + geom_text(aes_string(x = "segxend", 
                                                                                                                                          y = "segyend", label = "label", hjust = "hjust", 
                                                                                                                                          vjust = "vjust"), size = pieLabelSize, data = df[df$ratio < 
                                                                                                                                                                                             labelpositionThreshold, ], family = family) + geom_text(aes_string(x = "labelx", 
                                                                                                                                                                                                                                                                y = "labely", label = "label"), size = pieLabelSize, 
                                                                                                                                                                                                                                                     data = df[df$ratio >= labelpositionThreshold, ], 
                                                                                                                                                                                                                                                     family = family)
  }
  else {
    p1 <- p1 + geom_text(aes_string(x = "labelx", y = "labely", 
                                    label = "label"), size = pieLabelSize, data = df, 
                         family = family)
  }
  if (showPieName) 
    p1 <- p1 + annotate("text", x = 0, y = 0, label = pies, 
                        size = titlesize, family = family)
  p1 <- p1 + theme(text = element_text(family = family))
  if (!is.null(donuts)) {
    if (explodeDonut) {
      p3 <- p + geom_arc_bar(aes_string(x0 = "x", y0 = "y", 
                                        r0 = as.character(r1), r = as.character(r2), 
                                        start = "start1", end = "end1", fill = "no", 
                                        explode = "focus"), alpha = donutAlpha, color = color, 
                             data = df3)
    }
    else {
      p3 <- p + geom_arc_bar(aes_string(x0 = "x", y0 = "y", 
                                        r0 = as.character(r1), r = as.character(r2), 
                                        start = "start1", end = "end1", fill = "no"), 
                             alpha = donutAlpha, color = color, data = df3)
    }
    p3 <- p3 + transparent() + scale_fill_manual(values = subColor) + 
      xlim(r3 * c(-1, 1)) + ylim(r3 * c(-1, 1)) + guides(fill = FALSE)
    p3
    if (labelposition == 1) {
      p3 <- p3 + geom_segment(aes_string(x = "segx", y = "segy", 
                                         xend = "segxend", yend = "segyend"), data = df3) + 
        geom_text(aes_string(x = "segxend", y = "segyend", 
                             label = "label", hjust = "hjust", vjust = "vjust"), 
                  size = donutLabelSize, data = df3, family = family)
    }
    else if (labelposition == 0) {
      p3 <- p3 + geom_text(aes_string(x = "labelx", y = "labely", 
                                      label = "label"), size = donutLabelSize, data = df3, 
                           family = family)
    }
    else {
      p3 <- p3 + geom_segment(aes_string(x = "segx", y = "segy", 
                                         xend = "segxend", yend = "segyend"), data = df3[df3$ratio1 < 
                                                                                           labelpositionThreshold, ]) + geom_text(aes_string(x = "segxend", 
                                                                                                                                             y = "segyend", label = "label", hjust = "hjust", 
                                                                                                                                             vjust = "vjust"), size = donutLabelSize, data = df3[df3$ratio1 < 
                                                                                                                                                                                                   labelpositionThreshold, ], family = family) + 
        geom_text(aes_string(x = "labelx", y = "labely", 
                             label = "label"), size = donutLabelSize, data = df3[df3$ratio1 >= 
                                                                                   labelpositionThreshold, ], family = family)
    }
    if (!is.null(title)) 
      p3 <- p3 + annotate("text", x = 0, y = r3, label = title, 
                          size = titlesize, family = family)
    else if (showDonutName) 
      p3 <- p3 + annotate("text", x = (-1) * r3, y = r3, 
                          label = donuts, hjust = 0, size = titlesize, 
                          family = family)
    p3 <- p3 + theme(text = element_text(family = family))
    grid.newpage()
    print(p1, vp = viewport(height = 1, width = 1))
    print(p3, vp = viewport(height = 1, width = 1))
  }
  else {
    p1
  }
}
##输入数据并复制已经设置好的参数进行绘图
PieDonut2(df,aes(pies = group2, donuts = group1),
               r0 = 0.4,
               r1 = 1,
               r2 = 1.6,
               color = "white",#分割各区域的线条颜色
               pieAlpha = 0.8,#内层填充色的透明度
               donutAlpha = 0.8,#外层填充色的透明度
               start = pi/2,#起始位置
               addPieLabel = T,#是否显示内心圆的标签
               addDonutLabel = T,#是否显示外层的标签
               showRatioDonut = T,#外层标签的比例
               showRatioPie = T,#内层圆标签的比例
               ratioByGroup = F,#是否按分组计算外层比例
               showRatioThreshold = 0.02,#将标签显示为占总数比率的阈值，默认值为0.02。
               labelposition = 2,#设置外层标签位置的数字
               labelpositionThreshold = 0.1,#设置外层比例小于10%的数据标签显示在图形外
               explode = 1,#pies to explode,默认为NULL
               selected = 1,#donuts to explode,默认为NULL
               explodePie = T,#Whether or not explode pies
               explodeDonut = T,#Whether or not explode donuts
               explodePos = 0.1,#分裂的位置设置
               showPieName = T,#是否显示内层饼图名称
               showDonutName = F,#是否显示外层名称
               title = "This is a title",#标题,默认为NULL
               pieLabelSize = 4,#内层标签大小
               donutLabelSize = 3,#外层标签大小
               titlesize = 7,#标题字体大小
               family = getOption("PieDonut.family", ""),#字体设置
          man_color="Set1"##修改颜色
               )

##外边框基于AI去除即可

####修改颜色测试
RColorBrewer::display.brewer.all()
PieDonut2(df,aes(pies = group2, donuts = group1),
          r0 = 0.4,
          r1 = 1,
          r2 = 1.6,
          pieAlpha = 0.8,
          donutAlpha = 0.8,
          man_color="Paired")##修改颜色
