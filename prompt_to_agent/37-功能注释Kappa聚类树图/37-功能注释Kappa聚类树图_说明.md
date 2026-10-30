# 37-功能注释Kappa聚类树图

[数据](../../data/37-功能注释Kappa聚类树图) · [图片](../../figure/37-功能注释Kappa聚类树图) · [代码](../../script/37-功能注释Kappa聚类树图) · [Agent提示词](37-功能注释Kappa聚类树图_提示词.md)

## 图形表达

30个功能term的Kappa矩阵（900个值）以平均链接法按1−Kappa聚类，并与376行供给注释中的adj_pval与zscore条带按term对齐；默认树切分k=4。

## 数据来源与范围

供给30term Kappa平均链接树，统一物理row与原adj_pval/zscore；376注释/900矩阵值保留，k4人为切分而非DAVID模糊算法。

来源状态：selection notes not provided。选择记录未标注额外数据导出需求；本次仅复制选择指定的文件。

- `37-功能注释Kappa聚类树图_功能注释统计.csv`：来源`app/templates/imported/example__159_247_sci_davidkappa_34fbf82a/assets/easy_input.csv`，26237字节，SHA-256 `8fccc04b13bd70b1476aae3e5ab26fb860f006175fd773a06f9b6351329b47b0`。
- `37-功能注释Kappa聚类树图_Kappa相似矩阵.txt`：来源`app/templates/imported/example__159_247_sci_davidkappa_34fbf82a/assets/kappa_matrix.txt`，18110字节，SHA-256 `ec7a05a0a84a567307427431b7de3afbb77c88a8100edfa80cf206270fd40113`。
- `37-功能注释Kappa聚类树图_cluster-assignments.tsv`：来源`app/runtime/audit/visual_20261002/item202/default/cluster-assignments.tsv`，1434字节，SHA-256 `0e1d121491921b2d049021491cc7502d053de7d65f5c6cc6312efd2817677724`。
- `37-功能注释Kappa聚类树图_supplied-matrix.tsv`：来源`app/runtime/audit/visual_20261002/item202/default/supplied-matrix.tsv`，20032字节，SHA-256 `2f8bd55659d7cf6c2182b0b60c5cea6a2f30eb43795e39129fee5a78ab4b75d1`。
- `37-功能注释Kappa聚类树图_term-statistics.tsv`：来源`app/runtime/audit/visual_20261002/item202/default/term-statistics.tsv`，2840字节，SHA-256 `178bef34b54155284610e937082e63320021f36f6892163ced58e423bda1b93b`。
- `37-功能注释Kappa聚类树图_tree-segments.tsv`：来源`app/runtime/audit/visual_20261002/item202/default/tree-segments.tsv`，5411字节，SHA-256 `63999190cdab4f903635095b076e2c8c6c2e9576fdfd39baa793502e62975b8d`。
- `37-功能注释Kappa聚类树图_supplied-annotations.tsv`：来源`app/runtime/audit/visual_20261002/item202/default/supplied-annotations.tsv`，30008字节，SHA-256 `1252350391d2ec93e30938ab4c01be32a00fc19b9981952fd9f6c2434306f516`。

## 解释边界

k=4是人工描述性分段，不是DAVID模糊聚类或生物clade。原字段adj_pval不在本轮重新定义为q/FDR；多重校正方法、gene背景全集、zscore定义未核实。树只呈现供给矩阵相似性，不作功能通路或因果结论。

## 代码与适配

收藏模板ID：`example__159_247_sci_davidkappa_34fbf82a`；视觉形式：`average_linkage_kappa_term_dendrogram`。原模板目录：`app/templates/imported/example__159_247_sci_davidkappa_34fbf82a`。本次只复制已有PNG、PDF、SVG和代码/数据，不运行绘图或统计。

先从`script/37-功能注释Kappa聚类树图/37-功能注释Kappa聚类树图_配置.json`读取明确参数（当前选择参数：`{}`），再检查`script/37-功能注释Kappa聚类树图/37-功能注释Kappa聚类树图_template.R`与`script/37-功能注释Kappa聚类树图/37-功能注释Kappa聚类树图_source.Rmd`。配置与源码按仓库/图名目录中的相对文件路径整理；配置中登记的输入路径：`data/37-功能注释Kappa聚类树图/37-功能注释Kappa聚类树图_功能注释统计.csv`、`data/37-功能注释Kappa聚类树图/37-功能注释Kappa聚类树图_Kappa相似矩阵.txt`。依赖参考：jsonlite, svglite, ragg。

按精确term ID对齐统计表与矩阵；核对Kappa矩阵对称性、唯一term和每个term的统计记录。确认adj_pval定义与调整家族后再解释显著性；不要悄悄改k值、矩阵或去重term。

原始代码字节保持不变；旧教程、模拟器、兼容入口与真正绘图入口需以源码为准。只有复制的格式文件及路径配置经本次检查，R脚本尚未执行；不据此承诺代码开箱即运行。没有记录的单位、样本来源、统计检验、accession、下载日期、作者/许可不补写。

## 成图校验

现成输出目录：`app/runtime/audit/visual_20261002/item202/default`。PNG、PDF、SVG均与该输出目录对应并按字节复制；PNG SHA-256 `8b2d894d5c2d05c3ef5d89747d21bfce1a89dfa772085bd02b3646562414d376`，PDF SHA-256 `45f12d5f10e87c8e6e914e753a99264e01e4d5a5fcb031e16f89969266c2c1fe`，SVG SHA-256 `f46ef317e2e7f9ae2dc4e20ff0c7d71747795658958fef98dc6bd202ac02da42`。没有重绘、裁切、水印或封面处理。
