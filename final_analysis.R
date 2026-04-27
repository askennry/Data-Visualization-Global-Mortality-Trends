library(tidyverse) # 包含了 dplyr, tidyr, ggplot2, stringr
library(scales)    # 用于百分比和美元格式
library(ggrepel)   # 用于散点图标签
library(sf)        # 用于地理空间分析
library(rnaturalearth) # 用于获取世界地图数据

death_file_path <- "cause.csv"
pop_file_path <- "population.csv"
gdp_file_path <- "gdp.csv"
income_file_path <- "gdp_rank.csv"

########################################################################
# 预处理：合并四个数据集
########################################################################
# 导入死亡数据 (cause.csv)
df_deaths <- read_csv(death_file_path) %>%
  rename(Country = `Country/Territory`) %>%
  mutate(Year = as.integer(Year))

# 导入人口数据 (population.csv)
df_pop_wide <- read_csv(pop_file_path, skip = 4)

# 清洗和转换人口数据为长格式
df_pop_long <- df_pop_wide %>%
  select(`Country Code`, starts_with("19"), starts_with("20")) %>%
  pivot_longer(
    cols = as.character(1990:2019),
    names_to = "Year",
    values_to = "Population"
  ) %>%
  mutate(
    Year = as.integer(Year),
    Population = as.numeric(gsub(",", "", Population))
  ) %>%
  filter(!is.na(Population)) %>%
  select(`Country Code`, Year, Population)

# 合并死亡数据与人口数据
df_final <- df_deaths %>%
  left_join(df_pop_long, by = c("Code" = "Country Code", "Year" = "Year"))

# 派生指标计算 (生成 df_analysis)
original_cause_cols <- names(df_final)[4:32]

df_analysis <- df_final %>%
  mutate(Total_Deaths = rowSums(select(., all_of(original_cause_cols)), na.rm = TRUE)) %>%
  mutate(across(
    all_of(original_cause_cols),
    .fns = list(
      Rate_Per_100k = ~ (. / Population) * 100000, # 计算死亡率
      Proportion = ~ (. / Total_Deaths)           # 计算构成比
    ),
    .names = "{.col}_{.fn}" # 命名新列
  ))

# 导入 GDP 数据
df_gdp_wide <- read_csv(gdp_file_path, skip = 4)

# 清洗和转换 GDP 数据为长格式
df_gdp_long <- df_gdp_wide %>%
  select(`Country Code`, starts_with("19"), starts_with("20")) %>%
  pivot_longer(
    cols = as.character(1990:2019),
    names_to = "Year",
    values_to = "GDP_Per_Capita"
  ) %>%
  mutate(
    Year = as.integer(Year),
    GDP_Per_Capita = as.numeric(GDP_Per_Capita)
  ) %>%
  filter(!is.na(GDP_Per_Capita)) %>%
  select(Code = `Country Code`, Year, GDP_Per_Capita)

# 将 GDP 数据合并到分析数据框 df_analysis 中
df_analysis_final <- df_analysis %>%
  left_join(df_gdp_long, by = c("Code", "Year"))

# 导入收入分类数据 (我们只需用到第一列和第三列)
df_income_groups_raw <- read_csv(income_file_path, skip = 0)

# 清洗和简化收入分类数据
df_income_groups <- df_income_groups_raw %>%
  select(Code = 1, Income_Group = 3) %>%
  filter(!is.na(Code) & Code != "" & Code != "Country Code") %>%
  distinct(Code, .keep_all = TRUE)

# 将收入分组数据合并到最终分析数据框 df_analysis_final 中
df_analysis_with_groups <- df_analysis_final %>%
  left_join(df_income_groups, by = c("Code"))

# 为所有后续的分类分析预先创建 Category 列
df_categories_fixed <- df_analysis_with_groups %>% 
  # 选取关键列：Country, Year, Population, Total_Deaths, 所有原始死因
  select(Country, Year, Population, Total_Deaths, all_of(names(df_analysis)[4:32])) %>%
  pivot_longer(
    cols = all_of(names(df_analysis)[4:32]),
    names_to = "Cause",
    values_to = "Deaths_Absolute"
  ) %>%
  mutate(
    Category = case_when(
      # 1. 伤害 (Injuries)
      str_detect(Cause, "Violence|Self-harm|Drowning|Poisonings|Fire|Conflict|Terrorism|Road.Injuries") ~ "Injuries",
      # 2. 传染病, 孕产妇, 新生儿和营养相关 (CMNN)
      str_detect(Cause, "Meningitis|Malaria|Tuberculosis|Diarrheal|Lower.Respiratory|Acute.Hepatitis|Neonatal|Nutritional.Deficiencies|Protein.Energy.Malnutrition|Maternal") ~ "CMNN Diseases",
      # 3. 非传染性疾病 (NCDs)
      TRUE ~ "Non-Communicable Diseases (NCDs)" 
    )
  )


########################################################################
# 全世界 29 种死因堆叠图
########################################################################
df_global_structure <- df_analysis_with_groups %>% # 使用最终数据集
  filter(!is.na(Population)) %>%
  group_by(Year) %>%
  summarise(across(all_of(original_cause_cols), sum, .names = "Global_{.col}_Absolute"),
            .groups = 'drop') %>%
  mutate(Global_Total_Deaths = rowSums(select(., starts_with("Global_") & ends_with("_Absolute")))) %>%
  mutate(across(starts_with("Global_") & ends_with("_Absolute"),
                ~ .x / Global_Total_Deaths,
                .names = "{.col}_Proportion"))

df_global_long <- df_global_structure %>%
  select(Year, ends_with("_Proportion")) %>%
  pivot_longer(
    cols = ends_with("_Proportion"),
    names_to = "Cause",
    values_to = "Proportion"
  ) %>%
  mutate(
    Cause = str_remove_all(Cause, "Global_|_Absolute_Proportion$") %>% 
      str_replace_all("\\.", " ") %>% 
      str_replace_all(" and Other", " & Other")
  )

# 绘制图表：29种死因的全球趋势堆叠图
plot_macro_1 <- ggplot(df_global_long, aes(x = Year, y = Proportion, fill = Cause)) +
  geom_area(alpha = 0.8) +
  scale_y_continuous(labels = scales::percent) + 
  labs(
    title = "Global Evolution of Major Causes of Death (1990-2019)",
    subtitle = "Revealing trends in epidemiological transition from infectious to chronic diseases",
    x = "Year",
    y = "Proportion of Total Global Deaths",
    fill = "Cause of Death"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray30"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  scale_fill_viridis_d(option = "magma", begin = 0.1, end = 0.9)

print(plot_macro_1)

########################################################################
# 全世界 3 大类死因堆叠图
########################################################################
df_global_category_summary_fixed <- df_categories_fixed %>%
  filter(!is.na(Deaths_Absolute)) %>%
  group_by(Year, Category) %>%
  summarise(
    Global_Category_Deaths = sum(Deaths_Absolute, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  
  mutate(
    Category = factor(Category, levels = c(
      "Non-Communicable Diseases (NCDs)", 
      "CMNN Diseases", 
      "Injuries"
    ))
  ) %>%
  
  group_by(Year) %>%
  mutate(
    Global_Total_Deaths = sum(Global_Category_Deaths),
    # 计算按类别划分的构成比
    Proportion = Global_Category_Deaths / Global_Total_Deaths
  ) %>%
  ungroup()

# 绘制图表
plot_macro_2 <- ggplot(df_global_category_summary_fixed, aes(x = Year, y = Proportion, fill = Category)) +
  geom_area(alpha = 0.9) +
  scale_y_continuous(labels = scales::percent) +
  
  scale_fill_brewer(palette = "Set2") + 
  
  labs(
    title = "Global Burden of Disease Shift by Official Category (1990-2019)",
    subtitle = "Standard GBD classification: NCDs, CMNN, and Injuries.",
    x = "Year",
    y = "Proportion of Total Global Deaths",
    fill = "GBD Disease Category" 
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom", 
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray30"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

print(plot_macro_2)

########################################################################
# 死因相关性的 Heatmap
########################################################################
# 选取所有以 "_Rate_Per_100k" 结尾的列
rate_cols <- names(df_analysis) %>% 
  str_subset("_Rate_Per_100k$")

df_rates <- df_analysis %>%
  select(all_of(rate_cols)) %>%
  na.omit() # 确保在计算相关性前移除 NA 值

# 计算 Spearman 秩相关系数矩阵
correlation_matrix <- cor(df_rates, method = "spearman")

# 将相关性矩阵转换为长格式，以便用 ggplot2 绘图
df_corr_long <- correlation_matrix %>%
  as.data.frame() %>%
  rownames_to_column(var = "Cause_X") %>%
  pivot_longer(
    cols = -Cause_X,
    names_to = "Cause_Y",
    values_to = "Correlation"
  ) %>%
  # 清理病因名称，移除 "_Rate_Per_100k" 后缀
  mutate(
    Cause_X = str_remove_all(Cause_X, "_Rate_Per_100k"),
    Cause_Y = str_remove_all(Cause_Y, "_Rate_Per_100k")
  )

# 绘制图表：相关性热力图
plot_macro_3 <- ggplot(df_corr_long, aes(x = Cause_X, y = Cause_Y, fill = Correlation)) +
  geom_tile(color = "white") + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), 
                       space = "Lab", name="Spearman\nCorrelation") +
  theme_minimal(base_size = 12) +
  theme(
    # 调整X轴标签角度和对齐方式以防止重叠
    axis.text.x = element_text(angle = 45, vjust = 1, 
                               size = 10, hjust = 1),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(), 
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()
  ) +
  coord_fixed() + # 保持方格为正方形
  labs(
    title = "Correlation Heatmap of Global Disease Mortality Rates",
    subtitle = "Spearman correlation between 29 causes of death across all countries and years (1990-2019)."
  )

print(plot_macro_3)

########################################################################
# 三大代表性疾病的全球死亡率趋势 (折线图)
# 核心疾病选择：Cardiovascular Diseases, Malaria, Road Injuries
########################################################################
representative_causes_keywords <- c(
  "Cardiovascular.Diseases", 
  "Malaria", 
  "Road.Injuries"
)

# 1. 计算全球平均死亡率
df_global_rate_trend <- df_analysis %>%
  filter(!is.na(Total_Deaths)) %>%
  group_by(Year) %>%
  summarise(
    Global_Population = sum(Population, na.rm = TRUE),
    # 汇总所有死因的绝对死亡人数
    across(all_of(original_cause_cols), sum, .names = "Global_{.col}_Deaths"),
    .groups = 'drop'
  ) %>%
  # 重新计算全球标准化死亡率 
  mutate(across(starts_with("Global_") & ends_with("_Deaths"),
                ~ (.x / Global_Population) * 100000,
                .names = "{.col}_Rate_Per_100k")) %>%
  
  select(Year, starts_with("Global_") & ends_with("_Rate_Per_100k")) %>%
  
  # 筛选代表性疾病并转为长格式
  select(Year, all_of(names(.) %>% str_subset(paste(representative_causes_keywords, collapse="|")))) %>%
  pivot_longer(
    cols = -Year,
    names_to = "Cause",
    values_to = "Mortality_Rate"
  ) %>%
  mutate(Cause = str_remove_all(Cause, "Global_|_Deaths_Rate_Per_100k") %>% str_replace_all("\\.", " "))

# 2. 绘制折线图
plot_macro_4 <- ggplot(df_global_rate_trend, aes(x = Year, y = Mortality_Rate, color = Cause)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  
  labs(
    title = "Global Mortality Trends of Three Representative Diseases (1990-2019)",
    subtitle = "Age-Crude Rate per 100,000 population, demonstrating the epidemiological shift.",
    x = "Year",
    y = "Mortality Rate (Per 100,000)",
    color = "Disease Category"
  ) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 16)
  )

print(plot_macro_4)

########################################################################
# 2019 死亡率 Top 10 与 1990 对比 (并排柱状图)
########################################################################
# 重新计算全球标准化死亡率 (与折线图计算方式一致)
df_global_rate_ranking <- df_analysis %>%
  filter(Year %in% c(1990, 2019), !is.na(Total_Deaths)) %>%
  group_by(Year) %>%
  summarise(
    Global_Population = sum(Population, na.rm = TRUE),
    # 计算每个疾病的全球总死亡人数
    across(all_of(original_cause_cols), sum, .names = "Global_{.col}_Deaths"),
    .groups = 'drop'
  ) %>%
  # 计算全球标准化死亡率
  mutate(across(starts_with("Global_") & ends_with("_Deaths"),
                ~ (.x / Global_Population) * 100000,
                .names = "{.col}_Rate_Per_100k"))

# 将数据转为长格式并清洗列名
df_global_rate_long <- df_global_rate_ranking %>%
  select(Year, starts_with("Global_") & ends_with("_Rate_Per_100k")) %>%
  pivot_longer(
    cols = -Year,
    names_to = "Cause",
    values_to = "Mortality_Rate"
  ) %>%
  mutate(Cause = str_remove_all(Cause, "Global_|_Deaths_Rate_Per_100k") %>% str_replace_all("\\.", " ")) %>%
  # 排除总和项或 NA (如果有)
  filter(Cause != "Total_Deaths")


# 分别获取 1990 和 2019 年的 Top 10 排名
top_10_1990 <- df_global_rate_long %>%
  filter(Year == 1990) %>%
  arrange(desc(Mortality_Rate)) %>%
  slice_head(n = 10) %>%
  pull(Cause)

top_10_2019 <- df_global_rate_long %>%
  filter(Year == 2019) %>%
  arrange(desc(Mortality_Rate)) %>%
  slice_head(n = 10) %>%
  pull(Cause)


# 筛选并合并 1990 和 2019 的 Top 10 疾病，用于绘图
df_ranking_plot_combined <- df_global_rate_long %>%
  filter(Cause %in% union(top_10_1990, top_10_2019)) %>%
  
  # 创建 Year Factor 以确保 2019 在前/后
  mutate(Year = factor(Year, levels = c(2019, 1990))) %>%
  
  # 统一排序：使用 2019 年的死亡率作为排序基础
  left_join(df_global_rate_long %>% filter(Year == 2019) %>% select(Cause, Rate_2019 = Mortality_Rate), by = "Cause") %>%
  mutate(Cause = reorder(Cause, Rate_2019))


# 绘制哑铃柱状图 (或并排柱状图)
plot_macro_5 <- ggplot(df_ranking_plot_combined, aes(x = Mortality_Rate, y = Cause, fill = Year)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  
  # 美化：1990 年用柔和色，2019 年用强调色
  scale_fill_manual(values = c("1990" = "skyblue3", "2019" = "darkred")) +
  
  labs(
    title = "Global Top Mortality Causes: Standardized Rate Comparison (1990 vs 2019)",
    subtitle = "Rate per 100,000 population. Diseases are ranked by 2019 mortality rate.",
    x = "Mortality Rate (Per 100,000)",
    y = "Cause of Death",
    fill = "Year"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.major.y = element_blank()
  )

print(plot_macro_5)


########################################################################
# 2019 年三类死因构成比地图
########################################################################
# 准备疾病构成比数据 (2019年)
df_category_2019 <- df_analysis_final %>%
  filter(Year == 2019, !is.na(Total_Deaths), Total_Deaths > 0) %>%
  select(Code, Country, Total_Deaths, ends_with("_Proportion")) %>% 
  
  pivot_longer(
    cols = ends_with("_Proportion"),
    names_to = "Cause",
    values_to = "Proportion"
  ) %>%
  
  mutate(
    Clean_Cause = str_remove_all(Cause, "_Proportion$") %>% str_replace_all("\\.", " "),
    # 映射到 GBD 三大类
    Category = case_when(
      str_detect(Clean_Cause, "Violence|Self-harm|Drowning|Poisonings|Fire|Conflict|Terrorism|Road Injuries") ~ "Injuries",
      str_detect(Clean_Cause, "Meningitis|Malaria|Tuberculosis|Diarrheal|Lower Respiratory|Acute Hepatitis|Neonatal|Nutritional Deficiencies|Protein Energy Malnutrition|Maternal") ~ "CMNN Diseases",
      TRUE ~ "NCD Diseases"
    )
  ) %>%
  
  # 按三大类汇总构成比
  group_by(Code, Country, Category) %>%
  summarise(
    Category_Proportion = sum(Proportion, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  filter(Category_Proportion > 0)

# 获取世界地图地理数据
world_map <- ne_countries(scale = "medium", returnclass = "sf") %>%
  select(iso_a3, geometry)

# 合并疾病数据与地理数据
df_map_data_full <- world_map %>%
  left_join(df_category_2019, by = c("iso_a3" = "Code"))

# 绘图一：CMNN 疾病构成比地图
df_map_cmnn <- df_map_data_full %>%
  filter(Category == "CMNN Diseases") %>%
  na.omit()

plot_macro_6_1 <- ggplot(data = df_map_cmnn) +
  geom_sf(aes(fill = Category_Proportion), color = "gray80", linewidth = 0.1) +
  
  scale_fill_viridis_c(
    option = "plasma", direction = -1, limits = c(0, 1),
    labels = scales::percent, name = "Proportion of Total Deaths"
  ) +
  
  labs(
    title = "Global Burden of Communicable, Maternal, Neonatal, and Nutritional (CMNN) Diseases (2019)",
    subtitle = "Highlighting the high proportion of CMNN diseases in Africa and parts of South Asia.",
    caption = "Countries with missing data are colored gray."
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 16),
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "aliceblue", color = NA)
  )

print(plot_macro_6_1)

# 绘图二：NCD 疾病构成比地图
df_map_ncd <- df_map_data_full %>%
  filter(Category == "NCD Diseases") %>%
  na.omit() 

plot_macro_6_2 <- ggplot(data = df_map_ncd) +
  geom_sf(aes(fill = Category_Proportion), color = "gray80", linewidth = 0.1) +
  
  scale_fill_viridis_c(
    option = "plasma", direction = -1, limits = c(0, 1),
    labels = scales::percent, name = "Proportion of Total Deaths"
  ) +
  
  labs(
    title = "Global Burden of Non-Communicable Diseases (NCDs) (2019)",
    subtitle = "NCDs dominate the death structure in developed economies and rapidly aging countries.",
    caption = "Countries with missing data are colored gray."
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 16),
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "aliceblue", color = NA)
  )

print(plot_macro_6_2)

# 绘图三：Injuries 疾病构成比地图
df_map_injuries <- df_map_data_full %>%
  filter(Category == "Injuries") %>%
  na.omit()

plot_macro_6_3 <- ggplot(data = df_map_injuries) +
  geom_sf(aes(fill = Category_Proportion), color = "gray80", linewidth = 0.1) +
  
  scale_fill_viridis_c(
    option = "plasma", direction = -1, limits = c(0, 0.4), # Injuries 占比最高可能在 30% 左右
    labels = scales::percent, name = "Proportion of Total Deaths"
  ) +
  
  labs(
    title = "Global Burden of Injuries (2019)",
    subtitle = "Injuries, including road traffic accidents and violence, often dominate in high-risk regions.",
    caption = "Countries with missing data are colored gray."
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 16),
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "aliceblue", color = NA)
  )

print(plot_macro_6_3)

########################################################################
# 按收入分组的疾病结构演变堆叠图（地域分组）
########################################################################
# 准备数据：计算每个收入组别每年的平均构成比
df_groups_evolution <- df_analysis_with_groups %>%
  # 滤除没有收入分组或总死亡缺失的行
  filter(!is.na(Income_Group) & !is.na(Total_Deaths) & Total_Deaths > 0) %>%
  
  # 按 Year 和 Income_Group 分组，计算每个组别的总死亡人数
  group_by(Year, Income_Group) %>%
  summarise(
    # 计算组别的总死亡人数
    Group_Total_Deaths = sum(Total_Deaths, na.rm = TRUE),
    # 计算每个疾病的总死亡人数
    across(all_of(original_cause_cols), sum, .names = "Total_{.col}_Deaths"),
    .groups = 'drop_last'
  ) %>%
  
  # 计算每个疾病在组别总死亡人数中的比例
  mutate(across(starts_with("Total_") & ends_with("_Deaths"),
                ~ .x / Group_Total_Deaths,
                .names = "{.col}_Proportion")) %>%
  
  # 转换为长格式以便分类和绘图
  pivot_longer(
    cols = ends_with("_Proportion"),
    names_to = "Cause",
    values_to = "Proportion"
  ) %>%
  
  # 清洗列名，并映射到 GBD 三大类
  mutate(
    Clean_Cause = str_remove_all(Cause, "Total_|_Deaths_Proportion$") %>% str_replace_all("\\.", " "),
    Category = case_when(
      str_detect(Clean_Cause, "Violence|Self-harm|Drowning|Poisonings|Fire|Conflict|Terrorism|Road Injuries") ~ "Injuries",
      str_detect(Clean_Cause, "Meningitis|Malaria|Tuberculosis|Diarrheal|Lower Respiratory|Acute Hepatitis|Neonatal|Nutritional Deficiencies|Protein Energy Malnutrition|Maternal") ~ "CMNN Diseases",
      TRUE ~ "NCD Diseases"
    ),
    # 确保分类顺序一致
    Category = factor(Category, levels = c("NCD Diseases", "Injuries", "CMNN Diseases"))
  ) %>%
  
  # 最终按 Year, Income_Group 和 Category 聚合
  group_by(Year, Income_Group, Category) %>%
  summarise(Category_Proportion = sum(Proportion, na.rm = TRUE), .groups = 'drop') %>%
  
  # 【关键】设置收入组别的因子顺序
  mutate(Income_Group = factor(Income_Group, levels = c(
    "Low income", "Lower middle income", 
    "Upper middle income", "High income"
  )))


# 定义绘图函数：接收收入组别，输出堆叠图
plot_income_group_structure <- function(group_name) {
  df_plot <- filter(df_groups_evolution, Income_Group == group_name)
  
  ggplot(df_plot, aes(x = Year, y = Category_Proportion, fill = Category)) +
    geom_area(alpha = 0.9) +
    scale_y_continuous(labels = scales::percent) +
    
    scale_fill_brewer(palette = "Set2") +
    
    labs(
      title = paste0("Disease Burden Structure: ", group_name, " (1990-2019)"),
      subtitle = "Evolution of death proportion across three GBD categories.",
      x = "Year",
      y = "Proportion of Total Deaths",
      fill = "GBD Category"
    ) +
    
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 16)
    )
}

# 绘制四张图
print(plot_income_group_structure("Low income"))
print(plot_income_group_structure("Lower middle income"))
print(plot_income_group_structure("Upper middle income"))
print(plot_income_group_structure("High income"))










########################################################################
# 宏观因素与疾病死亡率散点图矩阵 (1990-2019)
# 依赖于前一步骤中计算的 df_analysis_for_modeling 数据集。
########################################################################
install.packages("GGally")
library(GGally)

# 假设以下变量和数据准备步骤已在您的环境中运行，
# 确保 df_analysis_for_modeling 存在且包含以下列：
# Year, GDP_Per_Capita, CMNN_Rate_Per_100k, NCD_Rate_Per_100k, Injuries_Rate_Per_100k

# 1. 定义所需变量
analysis_vars <- c(
  "Year", 
  "GDP_Per_Capita", 
  "CMNN_Rate_Per_100k", 
  "NCD_Rate_Per_100k", 
  "Injuries_Rate_Per_100k"
)

# 2. 数据准备：选择所需变量并处理缺失值
# 筛选出用于定量分析的变量，并移除含有 NA 的行
df_ggpairs <- df_analysis_for_modeling %>%
  select(all_of(analysis_vars)) %>%
  drop_na() 

# 3. 设置英文标签（用于显示在图表轴上）
# 注意：ggpairs使用列名作为轴标签。我们创建了一个临时的带英文名称的数据框。
names(df_ggpairs) <- c(
  "Year", 
  "GDP", 
  "CMNN Rate", 
  "NCD Rate", 
  "Injuries Rate"
)


# 4. 生成散点图矩阵
plot_ggpairs_matrix <- ggpairs(
  df_ggpairs,
  
  # 设置图表各区域的显示内容
  lower = list(
    # 下三角：散点图 + 线性拟合线 (Smooth line)
    continuous = wrap("points", alpha = 0.3, size = 0.8),
    combo = wrap("smooth", color = "red", method = "lm", se = FALSE)
  ),
  upper = list(
    # 上三角：相关系数 + 显著性星号
    continuous = wrap("cor", size = 4, adjust = TRUE) # size调整字体大小
  ),
  diag = list(
    # 对角线：密度图
    continuous = wrap("densityDiag", alpha = 0.5)
  ),
  
  # 调整图表标题
  title = "Scatterplot Matrix: Macro Factors and Disease Mortality Rates (1990-2019)"
) +
  # 统一主题设置
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    strip.background = element_rect(fill = "gray90"),
    panel.grid.major = element_line(colour = "gray95")
  )

print(plot_ggpairs_matrix)

library(tidyverse)

# --- 新的清新配色方案 ---
COLOR_NCD <- "#B3DE69" # 浅绿色
COLOR_CMNN <- "#80B1D3" # 浅蓝色/天蓝色
COLOR_INJURIES <- "#FB8072" # 浅珊瑚色/柔和红色
# -------------------------

# 1. 数据准备：选取 2019 年数据，转为长格式，并打上三大类标签
df_boxplot_data <- df_analysis_final %>%
  filter(Year == 2019) %>%
  select(Country, ends_with("_Rate_Per_100k")) %>%
  pivot_longer(
    cols = ends_with("_Rate_Per_100k"),
    names_to = "Cause",
    values_to = "Rate"
  ) %>%
  mutate(
    # 清洗死因名称
    Cause = str_remove(Cause, "_Rate_Per_100k") %>% str_replace_all("\\.", " "),
    
    # 重新打上 GBD 三大类标签
    Category = case_when(
      str_detect(Cause, "Violence|Self-harm|Drowning|Poisonings|Fire|Conflict|Terrorism|Road Injuries") ~ "Injuries",
      str_detect(Cause, "Meningitis|Malaria|Tuberculosis|Diarrheal|Lower Respiratory|Acute Hepatitis|Neonatal|Nutritional Deficiencies|Protein Energy Malnutrition|Maternal") ~ "CMNN Diseases",
      TRUE ~ "Non-Communicable Diseases (NCDs)"
    )
  )

# 2. 定义一个通用的绘图函数
plot_specific_boxplot <- function(data, category_name, color_theme) {
  
  # 筛选特定类别的数据
  plot_data <- data %>% filter(Category == category_name)
  
  ggplot(plot_data, aes(x = reorder(Cause, Rate, FUN = median), y = Rate)) +
    # 绘制箱线图，注意 alpha = 0.7 增加了透明度，有助于画面清透
    geom_boxplot(fill = color_theme, alpha = 0.7, outlier.size = 1.5, outlier.alpha = 0.4) +
    
    # 翻转坐标轴，让死因名称显示在 Y 轴
    coord_flip() +
    
    labs(
      title = paste0("Global Variance in ", category_name, " (2019)"),
      subtitle = "Distribution of mortality rates across all countries.",
      x = NULL, # Y轴现在是死因，不需要标题
      y = "Mortality Rate (Per 100,000)"
    ) +
    
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      panel.grid.major.y = element_blank(), 
      axis.text.y = element_text(size = 11)
    )
}

# 3. 分别绘制三张图

# 图 1: NCDs (浅绿色)
plot_box_ncd <- plot_specific_boxplot(df_boxplot_data, "Non-Communicable Diseases (NCDs)", COLOR_NCD)
print(plot_box_ncd)

# 图 2: CMNN (浅蓝色)
plot_box_cmnn <- plot_specific_boxplot(df_boxplot_data, "CMNN Diseases", COLOR_CMNN)
print(plot_box_cmnn)

# 图 3: Injuries (浅珊瑚色)
plot_box_injuries <- plot_specific_boxplot(df_boxplot_data, "Injuries", COLOR_INJURIES)
print(plot_box_injuries)



# 选取 4 个代表性疾病
selected_causes <- c(
  "Diarrheal.Diseases", 
  "Malaria", 
  "Cardiovascular.Diseases", 
  "Alzheimer.s.Disease.and.Other.Dementias"
)

# 准备数据 (选取 2019 年数据，避免时间序列干扰)
df_scatter_specific <- df_analysis_final %>%
  filter(Year == 2019, !is.na(GDP_Per_Capita)) %>%
  select(Country, GDP_Per_Capita, ends_with("_Rate_Per_100k")) %>%
  pivot_longer(
    cols = ends_with("_Rate_Per_100k"),
    names_to = "Cause",
    values_to = "Rate"
  ) %>%
  mutate(Cause = str_remove(Cause, "_Rate_Per_100k")) %>%
  filter(Cause %in% selected_causes) %>%
  # 优化标签名称
  mutate(Cause_Label = case_when(
    Cause == "Diarrheal.Diseases" ~ "Diarrheal Diseases (Poverty)",
    Cause == "Malaria" ~ "Malaria (Tropical/Poverty)",
    Cause == "Cardiovascular.Diseases" ~ "Cardiovascular (Lifestyle)",
    Cause == "Alzheimer.s.Disease.and.Other.Dementias" ~ "Alzheimer's (Aging/Affluence)"
  ))

# 绘图
plot_scatter_facet <- ggplot(df_scatter_specific, aes(x = GDP_Per_Capita, y = Rate)) +
  geom_point(alpha = 0.5, color = "steelblue", size = 1.5) +
  geom_smooth(method = "loess", color = "darkred", se = FALSE) + # 添加趋势线
  scale_x_log10(labels = scales::dollar_format()) + # GDP 用对数坐标
  facet_wrap(~Cause_Label, scales = "free_y") + # 独立 Y 轴刻度
  labs(
    title = "Economic Determinants of Specific Mortality Causes (2019)",
    subtitle = "Comparing typical diseases of poverty vs. diseases of affluence against GDP per capita.",
    x = "GDP Per Capita (Log Scale)",
    y = "Mortality Rate (Per 100,000)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.border = element_rect(color = "grey", fill = NA)
  )

print(plot_scatter_facet)






















########################################################################
# 目标国家特定分析的预处理
########################################################################
target_country <- "China"

df_country_structure <- df_analysis_final %>%
  filter(Country == target_country) %>%
  
  # 选取原始死因比例列
  select(Year, starts_with("Meningitis_Proportion"):last_col()) %>%
  select(Year, ends_with("_Proportion")) %>% 
  
  # 转为长格式
  pivot_longer(
    cols = ends_with("_Proportion"),
    names_to = "Cause",
    values_to = "Proportion"
  ) %>%
  
  # 清洗列名
  mutate(
    Cause = str_remove_all(Cause, "_Proportion$") %>% str_replace_all("\\.", " ")
  )


########################################################################
# 目标国家 GBD 三大类死因堆叠图
########################################################################

# 将细分的死因映射到 GBD 三大类
df_country_category <- df_country_structure %>%
  mutate(
    Category = case_when(
      str_detect(Cause, "Violence|Self-harm|Drowning|Poisonings|Fire|Conflict|Terrorism|Road Injuries") ~ "Injuries",
      str_detect(Cause, "Meningitis|Malaria|Tuberculosis|Diarrheal|Lower Respiratory|Acute Hepatitis|Neonatal|Nutritional Deficiencies|Protein Energy Malnutrition|Maternal") ~ "CMNN Diseases",
      TRUE ~ "Non-Communicable Diseases (NCDs)"
    )
  ) %>%
  
  # 按 GBD 三大类汇总构成比
  group_by(Year, Category) %>%
  summarise(
    Category_Proportion = sum(Proportion, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  
  # 确保 NCDs 在底部，CMNN 在中间，Injuries 在顶部
  mutate(
    Category = factor(Category, levels = c(
      "Non-Communicable Diseases (NCDs)", 
      "CMNN Diseases", 
      "Injuries"
    ))
  )


# 绘制堆叠面积图
plot_category_stack <- ggplot(df_country_category, aes(x = Year, y = Category_Proportion, fill = Category)) +
  geom_area(alpha = 0.9) +
  scale_y_continuous(labels = scales::percent) +
  
  scale_fill_brewer(palette = "Set2") +
  
  labs(
    title = paste0(target_country, ": Rapid Epidemiological Transition (1990-2019)"),
    subtitle = "Dramatic shift from Communicable (CMNN) to Non-Communicable Diseases (NCDs).",
    x = "Year",
    y = "Proportion of Total Deaths",
    fill = "GBD Disease Category"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom", 
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

print(plot_category_stack)


########################################################################
# 目标国家 29 类细分死因堆叠图
########################################################################

# df_country_structure 已在上一步中生成

# 绘制 29 种细分死因堆叠面积图
plot_cause_stack <- ggplot(df_country_structure, aes(x = Year, y = Proportion, fill = Cause)) +
  geom_area(alpha = 0.8) +
  scale_y_continuous(labels = scales::percent) +
  
  scale_fill_viridis_d(option = "magma", begin = 0.1, end = 0.9) +
  
  labs(
    title = paste0(target_country, ": Evolution of 29 Specific Causes of Death (1990-2019)"),
    subtitle = "Highlighting the rise of chronic non-communicable diseases at a granular level.",
    x = "Year",
    y = "Proportion of Total Deaths",
    fill = "Specific Cause of Death"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right", 
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

print(plot_cause_stack)

########################################################################
# 目标国家 GDP 与心脑血管死因变化 (双 Y 轴)
########################################################################

# 准备数据：筛选目标国家数据并提取关键列
df_country_gdp_health <- df_analysis_final %>%
  filter(Country == target_country) %>%
  # 提取 GDP 和心血管疾病死亡率
  mutate(
    Cardio_Rate = .[[names(.) %>% str_subset("Cardiovascular.Diseases_Rate_Per_100k")]]
  ) %>%
  select(Year, GDP_Per_Capita, Cardio_Rate) %>%
  na.omit()


rate_max <- max(df_country_gdp_health$Cardio_Rate, na.rm = TRUE)
gdp_max <- max(df_country_gdp_health$GDP_Per_Capita, na.rm = TRUE)

# 缩放因子：用于将 Cardio_Rate 调整到与 GDP 相似的绘图范围
scaling_factor <- gdp_max / rate_max


# 绘制双 Y 轴折线图
plot_gdp_rate_trend <- ggplot(df_country_gdp_health, aes(x = Year)) +
  
  # GDP 趋势线 (左 Y 轴)
  geom_line(aes(y = GDP_Per_Capita, color = "GDP Per Capita"), linewidth = 1.2) +
  
  # 心血管疾病死亡率趋势线 (右 Y 轴)
  geom_line(aes(y = Cardio_Rate * scaling_factor, color = "Cardiovascular Rate"), linewidth = 1.2) +
  
  # 设置 X 轴
  scale_x_continuous(breaks = seq(1990, 2019, 5)) +
  
  # 设置左 Y 轴 (GDP)
  scale_y_continuous(
    name = "GDP Per Capita (Current US$)",
    labels = scales::dollar_format(prefix = "$", scale = 1/1000, suffix = "k"),
    
    # 设置右 Y 轴 (Cardio Rate)
    sec.axis = sec_axis(
      ~ . / scaling_factor, # 将绘图值除以缩放因子，恢复到原始死亡率数值
      name = "Cardiovascular Mortality Rate (Per 100,000)"
    )
  ) +
  
  # 自定义颜色和图例
  scale_color_manual(
    values = c("GDP Per Capita" = "darkgreen", "Cardiovascular Rate" = "darkred"),
    name = "Trend"
  ) +
  
  labs(
    title = paste0(target_country, ": Economic Growth vs. Core NCD Burden (1990-2019)"),
    subtitle = "Comparing the growth trajectory of GDP with the change in the largest cause of death.",
    x = "Year"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 16),
    # 调整 Y 轴标题颜色
    axis.title.y.left = element_text(color = "darkgreen"),
    axis.title.y.right = element_text(color = "darkred")
  )

print(plot_gdp_rate_trend)


########################################################################
# 全球散点图 (突出显示目标国家)
########################################################################

# 1. 准备数据：筛选 2019 年数据并提取关键指标
df_gdp_scatter <- df_analysis_final %>%
  filter(Year == 2019) %>%
  # 提取 GDP 和心血管疾病死亡率
  mutate(
    Cardio_Rate = .[[names(.) %>% str_subset("Cardiovascular.Diseases_Rate_Per_100k")]],
    # 为突出显示目标国家做准备
    Highlight = ifelse(Country == target_country, target_country, "Other Countries")
  ) %>%
  select(Country, Highlight, GDP_Per_Capita, Cardio_Rate) %>%
  na.omit()


# 2. 绘制散点图
plot_scatter <- ggplot(df_gdp_scatter, aes(x = GDP_Per_Capita, y = Cardio_Rate, color = Highlight)) +
  
  # 绘制所有国家点
  geom_point(alpha = 0.6, size = 3) +
  
  # 添加平滑曲线 (可选，用于展示总体趋势)
  geom_smooth(method = "loess", se = FALSE, color = "gray50", linetype = "dashed") +
  
  # 使用 ggrepel 标记目标国家（避免标签重叠）
  geom_text_repel(
    data = subset(df_gdp_scatter, Country == target_country),
    aes(label = Country), 
    size = 5, 
    box.padding = unit(0.5, "lines"),
    point.padding = unit(0.5, "lines"),
    segment.color = 'gray50'
  ) +
  
  # 颜色设置：动态突出目标国家
  scale_color_manual(
    values = setNames(c("darkred", "skyblue3"), c(target_country, "Other Countries")),
    name = NULL # 移除图例名称
  ) +
  
  # 轴标签和格式化
  scale_x_continuous(
    name = "GDP Per Capita (Current US$)",
    labels = scales::dollar_format(prefix = "$", scale = 1/1000, suffix = "k")
  ) +
  scale_y_continuous(
    name = "Cardiovascular Mortality Rate (Per 100,000)"
  ) +
  
  labs(
    title = paste0(target_country, "'s Position on the Global Health-Wealth Curve (2019)"),
    subtitle = "Cardiovascular Disease Mortality Rate vs. GDP Per Capita.",
    caption = paste0("Source: GBD and World Bank Data. ", target_country, " is highlighted in red.")
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none", # 在图中已标记，无需图例
    plot.title = element_text(face = "bold", size = 16)
  )

print(plot_scatter)

########################################################################
# 宏观因素与疾病死亡率相关性矩阵 (1990-2019)
# 首先计算三大类疾病的汇总死亡率 (Rate Per 100k)
########################################################################
library(corrplot)

# 1. 定义三大类疾病的原始列名子集（使用用户代码中的 original_cause_cols）
# 这些列名来自 df_analysis_with_groups 的原始死因列
cmnn_causes <- original_cause_cols[str_detect(original_cause_cols, "Meningitis|Malaria|Tuberculosis|Diarrheal|Lower.Respiratory|Acute.Hepatitis|Neonatal|Nutritional.Deficiencies|Protein.Energy.Malnutrition|Maternal")]
injuries_causes <- original_cause_cols[str_detect(original_cause_cols, "Violence|Self.harm|Drowning|Poisonings|Fire|Conflict|Terrorism|Road.Injuries")]
# NCDs 是除去 CMNN 和 Injuries 的所有其他疾病
ncd_causes <- original_cause_cols[!original_cause_cols %in% c(cmnn_causes, injuries_causes)]


# 2. 增加定量分析所需的三大类死亡率 (Rate Per 100k)
df_analysis_for_modeling <- df_analysis_with_groups %>%
  # 确保 Population 和 GDP_Per_Capita 存在且非空
  filter(!is.na(Population) & !is.na(GDP_Per_Capita) & Population > 0) %>%
  mutate(
    # A. 计算三大类疾病的绝对死亡人数
    CMNN_Deaths_Absolute = rowSums(select(., all_of(cmnn_causes)), na.rm = TRUE),
    NCD_Deaths_Absolute = rowSums(select(., all_of(ncd_causes)), na.rm = TRUE),
    Injuries_Deaths_Absolute = rowSums(select(., all_of(injuries_causes)), na.rm = TRUE),
    
    # B. 计算三大类疾病的标准化死亡率 (Rate Per 100k)
    CMNN_Rate_Per_100k = (CMNN_Deaths_Absolute / Population) * 100000,
    NCD_Rate_Per_100k = (NCD_Deaths_Absolute / Population) * 100000,
    Injuries_Rate_Per_100k = (Injuries_Deaths_Absolute / Population) * 100000
  )


# 3. 定义所需变量 (使用新创建的列名)
analysis_vars <- c(
  "Year", 
  "GDP_Per_Capita", # 使用实际存在的 GDP 列名
  "CMNN_Rate_Per_100k", 
  "NCD_Rate_Per_100k", 
  "Injuries_Rate_Per_100k"
)

# 4. 数据准备：选择所需变量并处理缺失值
df_corr <- df_analysis_for_modeling %>%
  select(all_of(analysis_vars)) %>%
  # 删除任一关键变量缺失的行，以进行准确的相关性计算
  drop_na() 

# 5. 计算相关性矩阵
correlation_matrix <- cor(df_corr)

# 6. 绘制相关性矩阵热图
colnames(correlation_matrix) <- c("Year", "GDP Per Capita", "CMNN Mortality Rate", "NCD Mortality Rate", "Injuries Mortality Rate")
rownames(correlation_matrix) <- c("Year", "GDP Per Capita", "CMNN Mortality Rate", "NCD Mortality Rate", "Injuries Mortality Rate")

# 使用 corrplot 绘制
corrplot(correlation_matrix, 
         method = "color",       # 颜色展示相关性强度
         type = "upper",         # 只展示上半部分
         order = "hclust",       # 按聚类顺序排列
         addCoef.col = "black",  # 添加相关系数数值
         tl.col = "black",       # 标签颜色
         tl.srt = 45,            # 标签旋转角度
         diag = FALSE,           # 不显示对角线
         title = "Correlation Matrix: Macro Factors and Major Disease Mortality (1990-2019)",
         mar=c(0,0,1,0)          # 调整边距以容纳标题
)


library(tidyverse)
library(ggrepel)

# 1. 准备数据：筛选中国，仅取 1990 和 2019 年
df_china_rank <- df_analysis_final %>%
  filter(Country == "China", Year %in% c(1990, 2019)) %>%
  select(Year, ends_with("_Rate_Per_100k")) %>%
  pivot_longer(
    cols = ends_with("_Rate_Per_100k"),
    names_to = "Cause",
    values_to = "Rate"
  ) %>%
  mutate(Cause = str_remove(Cause, "_Rate_Per_100k") %>% str_replace_all("\\.", " ")) %>%
  
  # 按年份分组，计算排名 (Rank 1 是死亡率最高的)
  group_by(Year) %>%
  mutate(Rank = rank(-Rate)) %>% 
  ungroup()

# 2. 找出 1990 年的前 10 和 2019 年的前 10 (取并集)
# 这样不仅能看到谁还在榜单上，还能看到谁掉下去了，谁冲上来了
top_causes_union <- df_china_rank %>%
  filter(Rank <= 10) %>%
  pull(Cause) %>%
  unique()

# 3. 筛选绘图数据
df_plot_slope <- df_china_rank %>%
  filter(Cause %in% top_causes_union) %>%
  mutate(
    Year = as.factor(Year),
    # 为了绘图美观，给不同类别的病定义大致的颜色逻辑（可选）
    Category = case_when(
      str_detect(Cause, "Meningitis|Malaria|Tuberculosis|Diarrheal|Lower Respiratory|Neonatal|Nutritional|Maternal") ~ "CMNN",
      str_detect(Cause, "Violence|Self-harm|Road Injuries|Drowning") ~ "Injuries",
      TRUE ~ "NCDs"
    )
  )

# 4. 绘制坡度图 (Slope Chart)
plot_china_slope <- ggplot(df_plot_slope, aes(x = Year, y = Rank, group = Cause)) +
  # 画连接线
  geom_line(aes(color = Category, alpha = 0.8), size = 1.2) +
  # 画端点
  geom_point(aes(color = Category), size = 3) +
  
  # 左侧标签 (1990年排名)
  geom_text_repel(
    data = subset(df_plot_slope, Year == "1990"),
    aes(label = paste0(Rank, ". ", Cause)),
    nudge_x = -0.3, # 向左偏移
    direction = "y",
    hjust = 1,
    size = 3.5,
    segment.size = 0.2
  ) +
  
  # 右侧标签 (2019年排名)
  geom_text_repel(
    data = subset(df_plot_slope, Year == "2019"),
    aes(label = paste0(Rank, ". ", Cause)),
    nudge_x = 0.3, # 向右偏移
    direction = "y",
    hjust = 0,
    size = 3.5,
    segment.size = 0.2
  ) +
  
  # 翻转Y轴，让Rank 1在最上面
  scale_y_reverse(breaks = 1:15) +
  
  # 设置颜色：区分三大类
  scale_color_manual(values = c("CMNN" = "#8DA0CB", "NCDs" = "#66C2A5", "Injuries" = "#FC8D62")) +
  
  # 调整坐标轴范围，给文字留空间
  scale_x_discrete(expand = expansion(mult = 0.5)) +
  
  labs(
    title = "Shifting Killers: The Evolution of Top Death Causes in China",
    subtitle = "Comparing rank changes between 1990 and 2019 (Slope Chart).",
    x = NULL,
    y = "Rank (Lower is more deadly)",
    color = "Disease Category"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    axis.text.y = element_blank(), # 隐藏Y轴数字，因为标签里有了
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90")
  )

print(plot_china_slope)





library(tidyverse)
library(ggrepel)

# 1. 准备全球数据：汇总 1990 和 2019 年的所有国家数据
df_global_rank_slope <- df_analysis %>%
  filter(Year %in% c(1990, 2019), !is.na(Population)) %>%
  group_by(Year) %>%
  summarise(
    Global_Pop = sum(Population, na.rm = TRUE),
    across(all_of(original_cause_cols), sum, .names = "{.col}"),
    .groups = 'drop'
  ) %>%
  # 转长格式
  pivot_longer(
    cols = -c(Year, Global_Pop),
    names_to = "Cause",
    values_to = "Deaths"
  ) %>%
  # 计算全球死亡率
  mutate(Rate = (Deaths / Global_Pop) * 100000) %>%
  mutate(Cause = str_replace_all(Cause, "\\.", " ")) %>%
  
  # 计算排名
  group_by(Year) %>%
  mutate(Rank = rank(-Rate)) %>%
  ungroup()

# 2. 找出 1990 和 2019 的 Top 15 (取并集，以免漏掉以前很强或现在很强的病)
top_global_causes <- df_global_rank_slope %>%
  filter(Rank <= 15) %>%
  pull(Cause) %>%
  unique()

# 3. 筛选绘图数据
df_plot_global_slope <- df_global_rank_slope %>%
  filter(Cause %in% top_global_causes) %>%
  mutate(
    Year = as.factor(Year),
    # 定义颜色分类
    Category = case_when(
      str_detect(Cause, "Meningitis|Malaria|Tuberculosis|Diarrheal|Lower Respiratory|Neonatal|Nutritional|Maternal") ~ "CMNN",
      str_detect(Cause, "Violence|Self-harm|Road Injuries|Drowning") ~ "Injuries",
      TRUE ~ "NCDs"
    )
  )

# 4. 绘制全球坡度图
plot_global_slope <- ggplot(df_plot_global_slope, aes(x = Year, y = Rank, group = Cause)) +
  geom_line(aes(color = Category, alpha = 0.8), size = 1.2) +
  geom_point(aes(color = Category), size = 3) +
  
  # 1990 标签
  geom_text_repel(
    data = subset(df_plot_global_slope, Year == "1990"),
    aes(label = paste0(Rank, ". ", Cause)),
    nudge_x = -0.3, direction = "y", hjust = 1, size = 3.5, segment.size = 0.2
  ) +
  
  # 2019 标签
  geom_text_repel(
    data = subset(df_plot_global_slope, Year == "2019"),
    aes(label = paste0(Rank, ". ", Cause)),
    nudge_x = 0.3, direction = "y", hjust = 0, size = 3.5, segment.size = 0.2
  ) +
  
  scale_y_reverse(breaks = 1:15) +
  scale_color_manual(values = c("CMNN" = "#8DA0CB", "NCDs" = "#66C2A5", "Injuries" = "#FC8D62")) +
  scale_x_discrete(expand = expansion(mult = 0.5)) +
  
  labs(
    title = "Global Shifting Hierarchy of Death (1990 vs 2019)",
    subtitle = "Comparing the ranking of top 15 causes of death.",
    x = NULL, y = "Global Rank", color = "GBD Category"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray90")
  )

print(plot_global_slope)
