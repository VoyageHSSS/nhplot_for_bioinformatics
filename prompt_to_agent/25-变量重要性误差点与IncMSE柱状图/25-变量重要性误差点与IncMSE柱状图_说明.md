# 25-变量重要性误差点与IncMSE柱状图

[数据](../../data/25-变量重要性误差点与IncMSE柱状图/) · [图片](../../figure/25-变量重要性误差点与IncMSE柱状图/) · [代码](../../script/25-变量重要性误差点与IncMSE柱状图/) · [Agent提示词](25-变量重要性误差点与IncMSE柱状图_提示词.md)

## 图义与数据口径

68行即17个group×4个type，35个mean点与同一行35个配对区间、46个有限IncMSE值。mean/error/IncMSE分别33/33/22个NA，固定类型位置使两面板逐行对齐，真零与缺失区分。

仅展示提供的mean±error和IncMSE，不重新拟合模型。error是SE/SD/CI及样本n未知，不能称95%CI或显著性；系数标准化、IncMSE归一化和置换设置未核实。负值保留、NA空位保留，不能补成0或把重要性当因果。

## 输入自检与来源

- `25-变量重要性误差点与IncMSE柱状图_变量汇总.tsv`：68数据行、5列；空字段0；字面NA按原表保留，不自动当0。

模板ID：`example__141_22_sci_17f61073`；源目录：`app/templates/collections/我的收藏/example__141_22_sci_17f61073`。本次从本地现有模板读取、复制日期2026-10-28，不宣称这是原数据库下载日期。源数据库版本、accession、测量单位和许可未核实时保留未知，不补造。现成结果表来自下述同套成图归档，本次未重算其统计。临床表仅导出指定工作表，不复制无关工作表；雷达表仅运行原固定种子生成器。

## 代码使用

以仓库根目录为工作根，参数示例`script/25-变量重要性误差点与IncMSE柱状图/25-变量重要性误差点与IncMSE柱状图_配置.json`指向data/；优先检查`script/25-变量重要性误差点与IncMSE柱状图/25-变量重要性误差点与IncMSE柱状图_template.R`。R依赖：jsonlite, ggplot2, cowplot, ragg, svglite, readxl。显式用PLOT_NH_COLOR_MODULE指向`script/25-变量重要性误差点与IncMSE柱状图/25-变量重要性误差点与IncMSE柱状图_plot_nh_colors.R`，避免前缀导致共享模块搜索失败。

源data.xls是TSV文本，按字节复制为.tsv。先确认group/type唯一、mean和error来自同一记录，缺一项不能错配区间。需要新模型时单独分析并记录模型/误差定义，绘图只读汇总表。

代码原样复制，source.R可能是兼容入口、旧教程或生成器，不能据文件名称为论文原代码。前缀改变了文件名，模板/包装入口中对source.R、template.R或assets的引用须在新副本适配。含旧setwd/绝对路径/设备调用的教程不直接执行。配置是未执行的路径示例，不是已验证的环境锁；本次没有执行绘图模板。新分析与绘图分离，原资料只读，使用renv/conda并保存sessionInfo，随机步骤固定种子，结果另存日期目录。

## 图片复制验证

成图归档：`app/runtime/audit/example__141_22_sci_17f61073/output`。PNG SHA-256与当前preview完全相同：`2bb09a3c8a6b063a0edf16f3be053df7f2df52ea17ca65c5e9df139e761437c8`。三格式由同目录原样复制，无封面、裁切、水印、换色或重绘。PNG有效、PDF单页无栅格XObject、SVG无image将按实际文件核验。历史来源审计不是本次脚本执行，搬移后运行及跨设备接收未验证。

上游数据/图片/代码的独立再分发许可记录不完整，不据收藏或代码存在宣称统一开源/可商用。此处为绘图资料分享，不代表研究结论认证；小红书尚未发布。
