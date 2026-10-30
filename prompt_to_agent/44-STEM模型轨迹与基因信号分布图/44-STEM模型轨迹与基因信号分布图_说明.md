# 44-STEM模型轨迹与基因信号分布图

[数据](../../data/44-STEM模型轨迹与基因信号分布图) · [图片](../../figure/44-STEM模型轨迹与基因信号分布图) · [代码](../../script/44-STEM模型轨迹与基因信号分布图) · [Agent提示词](44-STEM模型轨迹与基因信号分布图_提示词.md)

## 图形表达

图叠加10条供给STEM模型轨迹（红线）及对应供给基因轨迹（灰线），并按5个时点展示基因信号分布箱线图。矩阵按Gene Symbol精确匹配；两份供给矩阵中的测量先按基因/时点汇总，再作为匹配信号分布输入。

## 数据来源与范围

1270源轨迹/10STEM模型与438匹配基因signal medians；nT/nB分开、全122离群点、无未经检验星号。

来源状态：selection notes not provided。选择记录未标注额外数据导出需求；本次仅复制选择指定的文件。

- `44-STEM模型轨迹与基因信号分布图_STEM供给模型轨迹表.txt`：来源`app/templates/imported/example__262_342_sci_stembox_c402aea7/assets/defaultsGuilleminSample_profiletable.txt`，2260字节，SHA-256 `a9fd47fd45799cabba2cbd80f10ba85ef0e8748786811e05d3c02ffed0f16b59`。
- `44-STEM模型轨迹与基因信号分布图_STEM供给基因注释表.txt`：来源`app/templates/imported/example__262_342_sci_stembox_c402aea7/assets/defaultsGuilleminSample_genetable.txt`，99983字节，SHA-256 `6852e43afcd9a14613ec2937983b4b4a3782fe1c6400f0c3c4c10b459ea3a1c3`。
- `44-STEM模型轨迹与基因信号分布图_供给信号矩阵_1.txt`：来源`app/templates/imported/example__262_342_sci_stembox_c402aea7/assets/g27_1.txt`，1013696字节，SHA-256 `8a39308ae1732c59336ad993a1da05f3099c151677d564efb714c194a11685ba`。
- `44-STEM模型轨迹与基因信号分布图_供给信号矩阵_2.txt`：来源`app/templates/imported/example__262_342_sci_stembox_c402aea7/assets/g27_2.txt`，1016920字节，SHA-256 `a5a5e09d2fdf8f8c1da2e9880880fed16910188f09c8a549f77353253ae72057`。
- `44-STEM模型轨迹与基因信号分布图_selected_profiles.csv`：来源`app/runtime/audit/visual_20261002/item251/default/selected_profiles.csv`，672字节，SHA-256 `58915c370f089e4a753a2e78a92e91500d0a2b5984fb1ccd0d78a8fa077cf3ee`。
- `44-STEM模型轨迹与基因信号分布图_profile_lines.csv`：来源`app/runtime/audit/visual_20261002/item251/default/profile_lines.csv`，180230字节，SHA-256 `c1174417db68a7db0ca28fc2146739ad01df05e995834040350eb80f76c52a4e`。
- `44-STEM模型轨迹与基因信号分布图_model_df.csv`：来源`app/runtime/audit/visual_20261002/item251/default/model_df.csv`，654字节，SHA-256 `9881740dad5bf3882189d5848d8341f7c3de1e05bd81df5e43ccf93ace23ad63`。
- `44-STEM模型轨迹与基因信号分布图_line_df.csv`：来源`app/runtime/audit/visual_20261002/item251/default/line_df.csv`，180230字节，SHA-256 `c1174417db68a7db0ca28fc2146739ad01df05e995834040350eb80f76c52a4e`。
- `44-STEM模型轨迹与基因信号分布图_box_df.csv`：来源`app/runtime/audit/visual_20261002/item251/default/box_df.csv`，73797字节，SHA-256 `0a6ce2956f305573c337adf50e6586e14464c01f5fe2392507245ebaaa6c16d9`。
- `44-STEM模型轨迹与基因信号分布图_box_stats.csv`：来源`app/runtime/audit/visual_20261002/item251/default/box_stats.csv`，2553字节，SHA-256 `6b1af59660c30b7bbb26fb32260cd9196d320c220d2d662fc3814411a1508381`。
- `44-STEM模型轨迹与基因信号分布图_profile_box_values.csv`：来源`app/runtime/audit/visual_20261002/item251/default/profile_box_values.csv`，73797字节，SHA-256 `0a6ce2956f305573c337adf50e6586e14464c01f5fe2392507245ebaaa6c16d9`。
- `44-STEM模型轨迹与基因信号分布图_med.csv`：来源`app/runtime/audit/visual_20261002/item251/default/med.csv`，106212字节，SHA-256 `cbefc184447c0667e5a8cb235a8647d78e22333616d38ceba764566f22c1087c`。
- `44-STEM模型轨迹与基因信号分布图_summary.csv`：来源`app/runtime/audit/visual_20261002/item251/default/summary.csv`，388字节，SHA-256 `44006b6ae1f0e8a0fb9fcf753c42e59816781ffb2e8fb586e744a84d73575df1`。
- `44-STEM模型轨迹与基因信号分布图_outliers.csv`：来源`app/runtime/audit/visual_20261002/item251/default/outliers.csv`，4230字节，SHA-256 `6ac96d184d4489dc7132873412165f25c18a8abe4ee656b473020f4a1e356d6c`。
- `44-STEM模型轨迹与基因信号分布图_omitted.csv`：来源`app/runtime/audit/visual_20261002/item251/default/omitted.csv`，160475字节，SHA-256 `fc64d5e91652d3e266aecdac0dec7e3192394d64f8a60c8e22f4bc493ffa3e37`。
- `44-STEM模型轨迹与基因信号分布图_sig.csv`：来源`app/runtime/audit/visual_20261002/item251/default/sig.csv`，672字节，SHA-256 `58915c370f089e4a753a2e78a92e91500d0a2b5984fb1ccd0d78a8fa077cf3ee`。
- `44-STEM模型轨迹与基因信号分布图_positions.csv`：来源`app/runtime/audit/visual_20261002/item251/default/positions.csv`，461字节，SHA-256 `5237d8171d5365821e49c6d96d0db2357807c7f80b5cf59b4995084d915653bd`。
- `44-STEM模型轨迹与基因信号分布图_analysis_metadata.json`：来源`app/runtime/audit/visual_20261002/item251/default/analysis_metadata.json`，1297字节，SHA-256 `9fcc4d87371eaa474da0ffb5b016e8bc0b5bfb447275fbc6288fe2cce40a5fa2`。

## 解释边界

STEM分组/模型及其来源p值直接沿用输入，没有重新运行STEM；时间点按等距类别排布，不能解读为实际时间间隔相等。箱线汇总的是供给基因信号而非独立样本重复；来源和实验条件未独立核实。没有时间比较检验，不能解释为时间效应或因果结论。1405个未匹配标识、20个缺测匹配值与122个离群点均有导出记录。

## 代码与适配

收藏模板ID：`example__262_342_sci_stembox_c402aea7`；视觉形式：`stem_model_trajectories_with_matched_signal_boxplots`。原模板目录：`app/templates/imported/example__262_342_sci_stembox_c402aea7`。本次只复制已有PNG、PDF、SVG和代码/数据，不运行绘图或统计。

先从`script/44-STEM模型轨迹与基因信号分布图/44-STEM模型轨迹与基因信号分布图_配置.json`读取明确参数（当前选择参数：`{}`），再检查`script/44-STEM模型轨迹与基因信号分布图/44-STEM模型轨迹与基因信号分布图_template.R`与`script/44-STEM模型轨迹与基因信号分布图/44-STEM模型轨迹与基因信号分布图_source.R`。配置与源码按仓库/图名目录中的相对文件路径整理；配置中登记的输入路径：`data/44-STEM模型轨迹与基因信号分布图/44-STEM模型轨迹与基因信号分布图_STEM供给模型轨迹表.txt`、`data/44-STEM模型轨迹与基因信号分布图/44-STEM模型轨迹与基因信号分布图_STEM供给基因注释表.txt`、`data/44-STEM模型轨迹与基因信号分布图/44-STEM模型轨迹与基因信号分布图_供给信号矩阵_1.txt`、`data/44-STEM模型轨迹与基因信号分布图/44-STEM模型轨迹与基因信号分布图_供给信号矩阵_2.txt`。依赖参考：jsonlite, svglite, ragg, readxl。

核对STEM模型表、基因注释和两份表达矩阵的版本及Gene Symbol映射；确认两矩阵的测量单位、转换方法与重复单位。说明模型轨迹和箱线信号的不同分母；如需推断时间变化，另行指定生物重复和统计检验，不能从这幅描述图读取显著性。

原始代码字节保持不变；旧教程、模拟器、兼容入口与真正绘图入口需以源码为准。只有复制的格式文件及路径配置经本次检查，R脚本尚未执行；不据此承诺代码开箱即运行。没有记录的单位、样本来源、统计检验、accession、下载日期、作者/许可不补写。

## 成图校验

现成输出目录：`app/runtime/audit/visual_20261002/item251/default`。PNG、PDF、SVG均与该输出目录对应并按字节复制；PNG SHA-256 `5dbe477bf53b08790fd2514d02ad0ddbedaa6811a16c1b709f39481d710270d5`，PDF SHA-256 `0e24d9caad2be274d1055c116bcb49a2a75d9d6205566b8328928cbd7d138b7a`，SVG SHA-256 `f4a0cead859af9acb3026d4f54ac7a7d1d0e8bc2e4425b28bc7b8fb128e7ccc1`。没有重绘、裁切、水印或封面处理。
