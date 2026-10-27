# 19-GWAS曼哈顿图

[数据](../../data/19-GWAS曼哈顿图/) · [图片](../../figure/19-GWAS曼哈顿图/) · [代码](../../script/19-GWAS曼哈顿图/) · [Agent提示词](19-GWAS曼哈顿图_提示词.md)

## 图义与数据口径

44575个唯一SNP，20条染色体（1–18、X、Y）。CHR/SNP/POS/P_value/type/gene保留原值；横轴按提供的chr_len累计偏移，纵轴真实-log10(P)。严格高于4的10个位点被标注。

4是显示cutoff（P<1e-4），不是自动认证的全基因组显著性阈值。type是提供的注释，Gene编号不能视为已认证基因；物种、assembly、P检验来源未知。Y唯一POS=0按原值保留，正式使用前确认坐标约定。不重新做关联、多重校正或LD分析，P=0不能伪造替代值。

## 输入自检与来源

- `19-GWAS曼哈顿图_关联输入.csv`：44575数据行、6列；空字段0；字面NA按原表保留，不自动当0。
- `19-GWAS曼哈顿图_染色体跨度.tsv`：20数据行、2列；空字段0；字面NA按原表保留，不自动当0。

模板ID：`example__199_283_sci_manhattan_452acbb0`；源目录：`app/templates/imported/example__199_283_sci_manhattan_452acbb0`。本次从本地现有模板读取、复制日期2026-10-28，不宣称这是原数据库下载日期。源数据库版本、accession、测量单位和许可未核实时保留未知，不补造。现成结果表来自下述同套成图归档，本次未重算其统计。临床表仅导出指定工作表，不复制无关工作表；雷达表仅运行原固定种子生成器。

## 代码使用

以仓库根目录为工作根，参数示例`script/19-GWAS曼哈顿图/19-GWAS曼哈顿图_配置.json`指向data/；优先检查`script/19-GWAS曼哈顿图/19-GWAS曼哈顿图_template.R`。R依赖：jsonlite, svglite, ragg, ggplot2, readxl。显式用PLOT_NH_COLOR_MODULE指向`script/19-GWAS曼哈顿图/19-GWAS曼哈顿图_plot_nh_colors.R`，避免前缀导致共享模块搜索失败。

配置必须同时显式传入karyotype_path，避免文件改名后被识别为custom输入并改用观察跨度。新数据先确认物种、build、长度和阈值来源，保持SNP唯一，不随意用人类阈值替代。

代码原样复制，source.R可能是兼容入口、旧教程或生成器，不能据文件名称为论文原代码。前缀改变了文件名，模板/包装入口中对source.R、template.R或assets的引用须在新副本适配。含旧setwd/绝对路径/设备调用的教程不直接执行。配置是未执行的路径示例，不是已验证的环境锁；本次没有执行绘图模板。新分析与绘图分离，原资料只读，使用renv/conda并保存sessionInfo，随机步骤固定种子，结果另存日期目录。

## 图片复制验证

成图归档：`app/runtime/audit/visual_20261002/item236/default`。PNG SHA-256与当前preview完全相同：`11bcc1899f4b85740e8d8dad761808ecfe54d2d75be7a6e10e0cc1fe24bbbaa3`。三格式由同目录原样复制，无封面、裁切、水印、换色或重绘。PNG有效、PDF单页无栅格XObject、SVG无image将按实际文件核验。历史来源审计不是本次脚本执行，搬移后运行及跨设备接收未验证。

上游数据/图片/代码的独立再分发许可记录不完整，不据收藏或代码存在宣称统一开源/可商用。此处为绘图资料分享，不代表研究结论认证；小红书尚未发布。
