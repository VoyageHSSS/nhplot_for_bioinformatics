# 39-预计算UMAP细胞注释图

[数据](../../data/39-预计算UMAP细胞注释图) · [图片](../../figure/39-预计算UMAP细胞注释图) · [代码](../../script/39-预计算UMAP细胞注释图) · [Agent提示词](39-预计算UMAP细胞注释图_提示词.md)

## 图形表达

输入中提供1231个细胞的UMAP1/UMAP2与15类cluster标签；图按等比例显示全部点，标签引线指向每类坐标算术中心。没有重跑Seurat。

## 数据来源与范围

1231提供细胞坐标/15注释组，等比例保留全部点，标签避开每点且引线连接真实均值；不重跑Seurat。

来源状态：selection notes not provided。选择记录未标注额外数据导出需求；本次仅复制选择指定的文件。

- `39-预计算UMAP细胞注释图_预计算UMAP坐标与细胞注释.txt`：来源`app/templates/imported/example__242_323_sci_seurat_umap_2fc76302/assets/GSE133345_Annotations.txt`，110986字节，SHA-256 `20ec929e2bbc90e0a0c43841ffa8abacf494594579db82804fa82410c26170b7`。
- `39-预计算UMAP细胞注释图_umap_cells.csv`：来源`app/runtime/audit/visual_20261002/item244/default/umap_cells.csv`，133211字节，SHA-256 `d4e494db2202eb6371bdffeb3c0f3d5b66877948fe565f5dcf32ce90c186dfb2`。
- `39-预计算UMAP细胞注释图_cluster_centroids.csv`：来源`app/runtime/audit/visual_20261002/item244/default/cluster_centroids.csv`，2461字节，SHA-256 `4225827dd74aca548c335bd2e36fcf780859b6d3936a65cd97fc332f9586779d`。

## 解释边界

UMAP嵌入坐标及15类标注来自供给文件；建模参数、批次校正和注释证据未在本次独立核实。中心点不是实际细胞、发育轨迹或显著性。不要将细胞数视为独立生物学重复，也不要根据图推断谱系。

## 代码与适配

收藏模板ID：`example__242_323_sci_seurat_umap_2fc76302`；视觉形式：`precomputed_umap_cell_annotation`。原模板目录：`app/templates/imported/example__242_323_sci_seurat_umap_2fc76302`。本次只复制已有PNG、PDF、SVG和代码/数据，不运行绘图或统计。

先从`script/39-预计算UMAP细胞注释图/39-预计算UMAP细胞注释图_配置.json`读取明确参数（当前选择参数：`{}`），再检查`script/39-预计算UMAP细胞注释图/39-预计算UMAP细胞注释图_template.R`与`script/39-预计算UMAP细胞注释图/39-预计算UMAP细胞注释图_source.R`。配置与源码按仓库/图名目录中的相对文件路径整理；配置中登记的输入路径：`data/39-预计算UMAP细胞注释图/39-预计算UMAP细胞注释图_预计算UMAP坐标与细胞注释.txt`。依赖参考：jsonlite, svglite, ragg, readxl。

新数据要有一行一个cell的唯一ID、两个已计算坐标和一个明确注释字段。检查所有坐标与标签非缺失、ID唯一、类别覆盖；若需重新降维，单独记录输入、预处理、随机种子与批次方法。

原始代码字节保持不变；旧教程、模拟器、兼容入口与真正绘图入口需以源码为准。只有复制的格式文件及路径配置经本次检查，R脚本尚未执行；不据此承诺代码开箱即运行。没有记录的单位、样本来源、统计检验、accession、下载日期、作者/许可不补写。

## 成图校验

现成输出目录：`app/runtime/audit/visual_20261002/item244/default`。PNG、PDF、SVG均与该输出目录对应并按字节复制；PNG SHA-256 `54d1b0e2e419e53b1e41fd123e2114ea9b5cc3c05a54bc131d73ab6ff013e46d`，PDF SHA-256 `4773d8c71232993592109a58d321c727b2b3325e7e407ee245c5fa4f67ad6b7a`，SVG SHA-256 `230c43973c3847137c9b8215c23918424d62c68829e89027a4772d2c1bb36264`。没有重绘、裁切、水印或封面处理。
