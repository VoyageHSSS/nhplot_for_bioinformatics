# 41-模拟MAG状态点阵与相对丰度柱图

[数据](../../data/41-模拟MAG状态点阵与相对丰度柱图) · [图片](../../figure/41-模拟MAG状态点阵与相对丰度柱图) · [代码](../../script/41-模拟MAG状态点阵与相对丰度柱图) · [Agent提示词](41-模拟MAG状态点阵与相对丰度柱图_提示词.md)

## 图形表达

明确标为tutorial simulation的9×12状态矩阵与12个MAG的A/B组相对丰度柱图；空白保留为未记录状态，顶部柱图按MAG ID对齐。

## 数据来源与范围

教程明确模拟的9基因×12MAG/108状态、12对应丰度；严格按MAG对齐并保留空白未记录，真实状态和A/B图例，三格式native矢量。

来源状态：validated。选择记录未标注额外数据导出需求；本次仅复制选择指定的文件。

- `41-模拟MAG状态点阵与相对丰度柱图_MAG状态矩阵.txt`：来源`app/templates/imported/example__335_64_sci_8ef875f0/assets/data-1.txt`，853字节，SHA-256 `23499f1b3cf2e780f4e3267f36b7f039ac83b04fdf1e37542ef69700e769669b`。
- `41-模拟MAG状态点阵与相对丰度柱图_MAG相对丰度供给表.txt`：来源`app/templates/imported/example__335_64_sci_8ef875f0/assets/data-2.txt`，399字节，SHA-256 `7a0c60cf9b7fb8f2a359d3481012aa5c99873247e33a0fc5cb05a8112080292e`。
- `41-模拟MAG状态点阵与相对丰度柱图_actual_cells.csv`：来源`app/runtime/audit/visual_20261002/item308/default/actual_cells.csv`，7041字节，SHA-256 `2d733a9c3b50f28a51c55e0fc4999d200872d40b5561227b304dd10f10ea4219`。
- `41-模拟MAG状态点阵与相对丰度柱图_actual_bars.csv`：来源`app/runtime/audit/visual_20261002/item308/default/actual_bars.csv`，1033字节，SHA-256 `60c0775429a7fffe91455ca8637df79f8a98dd883536b9990e20e31b84fd3957`。
- `41-模拟MAG状态点阵与相对丰度柱图_actual_points.csv`：来源`app/runtime/audit/visual_20261002/item308/default/actual_points.csv`，5007字节，SHA-256 `a98f3e854f07f534e60a60fd204b6a6f589d02a47530295064ee9bbe9bbc15ed`。

## 解释边界

输入是教学模拟数据，不代表真实野火、基因功能或独立样本。相对丰度的来源/计算未独立核实；状态编码及A/B含义沿用原表，不作统计推断或生物学结论。

## 代码与适配

收藏模板ID：`example__335_64_sci_8ef875f0`；视觉形式：`simulated_mag_state_matrix_and_abundance_bars`。原模板目录：`app/templates/imported/example__335_64_sci_8ef875f0`。本次只复制已有PNG、PDF、SVG和代码/数据，不运行绘图或统计。

先从`script/41-模拟MAG状态点阵与相对丰度柱图/41-模拟MAG状态点阵与相对丰度柱图_配置.json`读取明确参数（当前选择参数：`{}`），再检查`script/41-模拟MAG状态点阵与相对丰度柱图/41-模拟MAG状态点阵与相对丰度柱图_template.R`与`script/41-模拟MAG状态点阵与相对丰度柱图/41-模拟MAG状态点阵与相对丰度柱图_source.R`。配置与源码按仓库/图名目录中的相对文件路径整理；配置中登记的输入路径：`data/41-模拟MAG状态点阵与相对丰度柱图/41-模拟MAG状态点阵与相对丰度柱图_MAG状态矩阵.txt`、`data/41-模拟MAG状态点阵与相对丰度柱图/41-模拟MAG状态点阵与相对丰度柱图_MAG相对丰度供给表.txt`。依赖参考：jsonlite, svglite, ragg, readxl, showtext, sysfonts, systemfonts。

真实数据使用前确认MAG唯一ID、状态编码与缺失语义，逐ID核对丰度列/行和A/B分组来源；分清空白、未记录、阴性和数值零。不要沿用演示值或推断检验。

原始代码字节保持不变；旧教程、模拟器、兼容入口与真正绘图入口需以源码为准。只有复制的格式文件及路径配置经本次检查，R脚本尚未执行；不据此承诺代码开箱即运行。没有记录的单位、样本来源、统计检验、accession、下载日期、作者/许可不补写。

## 成图校验

现成输出目录：`app/runtime/audit/visual_20261002/item308/default`。PNG、PDF、SVG均与该输出目录对应并按字节复制；PNG SHA-256 `78350e7b95a8bd1576f6f668846d0a54e81b9631ee709d06fd1cd377432bf897`，PDF SHA-256 `a9f8c102206b325890d99cbdefec673927d73a230a1be4c60071e57cfde5eeea`，SVG SHA-256 `2c163b6bbfc90627d6d571150ab0be33831b2ead3d7baf056ad8fdb79c3b3fc5`。没有重绘、裁切、水印或封面处理。
