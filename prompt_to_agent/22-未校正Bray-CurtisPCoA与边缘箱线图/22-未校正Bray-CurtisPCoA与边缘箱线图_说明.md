# 22-未校正Bray-CurtisPCoA与边缘箱线图

[数据](../../data/22-未校正Bray-CurtisPCoA与边缘箱线图/) · [图片](../../figure/22-未校正Bray-CurtisPCoA与边缘箱线图/) · [代码](../../script/22-未校正Bray-CurtisPCoA与边缘箱线图/) · [Agent提示词](22-未校正Bray-CurtisPCoA与边缘箱线图_提示词.md)

## 图义与数据口径

72个样本×17个非负特征，Subtype分组SCNC-A/N/P/Y计数30/20/1/21。未校正Bray-Curtis PCoA，主图xy单位长度相同，边缘箱线使用同一轴坐标。现成ordination坐标表原样保留。

含38个负特征值，绝对和占总绝对和14.49%；轴39.24%/17.20%使用原有带符号特征值总和分母，不能直接称常规解释方差。单例P组没有组内分布估计；Tukey共享字母不代表等效。已有PERMANOVA采用999自由置换/种子20261003，但研究交换性、离散度及来源未知，不作因果解释。本次未重算统计。

## 输入自检与来源

- `22-未校正Bray-CurtisPCoA与边缘箱线图_样本特征.tsv`：72数据行、19列；空字段0；字面NA按原表保留，不自动当0。

模板ID：`example__014_110_sci_pcoa_e9784345`；源目录：`app/templates/imported/example__014_110_sci_pcoa_e9784345`。本次从本地现有模板读取、复制日期2026-10-28，不宣称这是原数据库下载日期。源数据库版本、accession、测量单位和许可未核实时保留未知，不补造。现成结果表来自下述同套成图归档，本次未重算其统计。临床表仅导出指定工作表，不复制无关工作表；雷达表仅运行原固定种子生成器。

## 代码使用

以仓库根目录为工作根，参数示例`script/22-未校正Bray-CurtisPCoA与边缘箱线图/22-未校正Bray-CurtisPCoA与边缘箱线图_配置.json`指向data/；优先检查`script/22-未校正Bray-CurtisPCoA与边缘箱线图/22-未校正Bray-CurtisPCoA与边缘箱线图_template.R`。R依赖：readr, dplyr, ggplot2, vegan, ape, patchwork, RColorBrewer, multcompView, jsonlite, ragg, svglite。显式用PLOT_NH_COLOR_MODULE指向`script/22-未校正Bray-CurtisPCoA与边缘箱线图/22-未校正Bray-CurtisPCoA与边缘箱线图_plot_nh_colors.R`，避免前缀导致共享模块搜索失败。

原data.xls实为制表符文本，按字节复制为.tsv。原模板内含PCoA/ANOVA/Tukey/PERMANOVA，请在新工作副本拆成独立分析与只读绘图两步；不能把未校正坐标悄悄换成校正坐标或擅自删除单例组。

代码原样复制，source.R可能是兼容入口、旧教程或生成器，不能据文件名称为论文原代码。前缀改变了文件名，模板/包装入口中对source.R、template.R或assets的引用须在新副本适配。含旧setwd/绝对路径/设备调用的教程不直接执行。配置是未执行的路径示例，不是已验证的环境锁；本次没有执行绘图模板。新分析与绘图分离，原资料只读，使用renv/conda并保存sessionInfo，随机步骤固定种子，结果另存日期目录。

## 图片复制验证

成图归档：`app/runtime/audit/visual_20261002/pcoa_marginals/schema-default`。PNG SHA-256与当前preview完全相同：`6f5652e4c9fd572b0db388e3e86bcb61f5c8ef41675427aafc8b85613143fabf`。三格式由同目录原样复制，无封面、裁切、水印、换色或重绘。PNG有效、PDF单页无栅格XObject、SVG无image将按实际文件核验。历史来源审计不是本次脚本执行，搬移后运行及跨设备接收未验证。

上游数据/图片/代码的独立再分发许可记录不完整，不据收藏或代码存在宣称统一开源/可商用。此处为绘图资料分享，不代表研究结论认证；小红书尚未发布。
