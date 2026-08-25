#Script for creating all manuscript figures

pacman::p_load(dplyr, tidyverse, ggplot2, patchwork, colorblindcheck, 
               DiagrammeR, DiagrammeRsvg, rsvg, vcd) #ggpubr

#read in cleaned data
td_bu_final <- read.csv("data/final_td_bu_df.csv")

#-------------------------------------------------------------------------------------------------#
#### FIGURES ####
year_eco_sum <- td_bu_final |>
  mutate(ecosystem = str_replace_all(ecosystem, ";", ",")) |>
  separate_rows(ecosystem, sep = ",") |>
  mutate(ecosystem = str_trim(ecosystem)) |> #split ecosystem into multiple rows when applicable 
  group_by(year, ecosystem) |>
  summarise(n = n(), .groups = "drop") |>
  mutate(ecosystem = factor(ecosystem, levels = c("freshwater", "marine", "estuary")))

fg_eco_sum <- td_bu_final |>
  mutate(ecosystem = str_replace_all(ecosystem, ";", ",")) |>
  separate_rows(ecosystem, sep = ",") |>
  mutate(ecosystem = stringr::str_trim(ecosystem)) |>
  distinct(study, ecosystem, .keep_all = TRUE) |>  # keeps first instance only
  group_by(ecosystem) |>
  summarise(n = n(), .groups = "drop") |>
  mutate(ecosystem = factor(ecosystem, levels = c("freshwater", "marine", "estuary")))

#Historical progression of use of functional groups across ecosystems
p1 <- ggplot(year_eco_sum |> filter(!ecosystem %in% "aquatic" & !is.na(ecosystem)),
             aes(x = year, y = n, color = ecosystem)) +
  geom_line(linewidth = 1) + geom_point() + theme_bw() +
  scale_color_manual(values = c("#3F6C51","#9DC5BB","#A9714B")) +
  labs(x = "", y = "Number of studies", color = "") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.box.spacing = unit(0.001, "cm"))

#total number of studies in each ecosystem
p2 <- ggplot(fg_eco_sum |> filter(!ecosystem %in% "aquatic" & !is.na(ecosystem)),
             aes(x = ecosystem, y = n, fill = ecosystem)) +
  geom_col(width = 0.7) + theme_bw() +
  scale_fill_manual(values = c("#3F6C51","#9DC5BB","#A9714B")) +
  labs(x = "", y = "Number of studies", fill = "") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

# Figure 1
fig1 <- p1 / p2 +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag.location = "panel",       
        plot.tag.position  = c(-0.06,1), 
        plot.tag = element_text(size = 14, face = "bold")) 
#ggsave("figures/phyto_func_group_combined.jpg", fig1, width = 5, height = 6)

#Fig 2: stacked bar plot of functional group types across ecosystems
fg_type_eco <- td_bu_final |>
  filter(!is.na(func_group_type)) |>
  mutate(ecosystem = stringr::str_replace_all(ecosystem, ";", ","),
         func_group_type = stringr::str_replace_all(func_group_type, ";", ",")) |>
  tidyr::separate_rows(ecosystem, sep = ",\\s*") |>
  tidyr::separate_rows(func_group_type, sep = ",\\s*") |>
  mutate(ecosystem = stringr::str_trim(ecosystem),
         func_group_type = stringr::str_trim(func_group_type)) |>
  distinct(study, ecosystem, func_group_type, .keep_all = TRUE) |>
  group_by(ecosystem, func_group_type) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(ecosystem) |>
  mutate(prop = n / sum(n),
         func_group_type = factor(func_group_type, levels = c(
           "taxonomic","omics", "morphological", "physiological")),
         ecosystem = factor(ecosystem, levels = c("estuary","marine","freshwater"))) |>  
  ungroup()

#heatmap to see how prevalent different functional group definitions are across ecosystems
ggplot(fg_type_eco |> filter(!ecosystem %in% "aquatic", !is.na(ecosystem)), 
       aes(x = func_group_type, y = ecosystem, fill = prop)) +
  geom_tile(color = "white") +
  geom_text(aes(label = scales::percent(prop, accuracy = 1)), color = "black", size = 3) +
  scale_fill_gradient(low = "white", high = "#C4B1AE") +
  theme_minimal() + theme(axis.text = element_text(size=8),
                          axis.text.x = element_text(angle = 45, hjust = 1), 
                          panel.grid = element_blank()) +
  labs(fill = "Proportion of studies", x = "", y = "")
#ggsave("figures/phyto_func_group_by_ecosystem_heatmapl.jpg", width = 5, height = 4)

#fig 3: td/bu emphasis across ecosystems
fig3_df <- td_bu_final |>
  filter(!is.na(ecosystem), !is.na(func_group_type)) |> #, !is.na(importance_td_vs_bu)
  mutate(ecosystem = str_replace_all(ecosystem, ";", ","),
         func_group_type = str_replace_all(func_group_type, ";", ",")) |>
  separate_rows(ecosystem, sep = ",\\s*") |>
  separate_rows(func_group_type, sep = ",\\s*") |>
  mutate(ecosystem = str_trim(ecosystem),
         func_group_type = str_trim(func_group_type),
         importance_td_vs_bu = if_else(is.na(importance_td_vs_bu),
                                       "NA",importance_td_vs_bu)) |>
  distinct(study, ecosystem, func_group_type, .keep_all = TRUE) |>
  count(ecosystem, func_group_type, importance_td_vs_bu) |>   
  group_by(ecosystem, func_group_type) |>
  mutate(prop = n / sum(n)) |>
  ungroup() |>
  mutate(importance_td_vs_bu = factor(importance_td_vs_bu, levels = c("td", "bu", "both", "NA")),
         func_group_type = factor(func_group_type, levels = c(
           "taxonomic","omics", "morphological", "physiological")),
         ecosystem = factor(ecosystem, levels = c("freshwater", "marine", "estuary")))


ggplot(fig3_df |> filter(!ecosystem %in% "aquatic", !is.na(ecosystem)),
                   aes(x = func_group_type, y = n, fill = importance_td_vs_bu)) +
  geom_col(position = position_dodge(width = 0.8, preserve = "single"), width = 0.7) +
  facet_wrap(~ ecosystem, scales = "free_y") +
  theme_bw(base_size = 7) + labs(x = "", y = "Number of studies",
                    fill = "Process emphasis") +
  scale_fill_manual(values = c("#586BA4","#F76C5E", "#F5DD90", "grey70")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.key.size = unit(0.4, "cm"))
#ggsave("figures/td_bu_emphasis_by_func_groups_raw.jpg", width = 4, height = 3)

#Figure S1
ggplot(fig3_df |> filter(!ecosystem %in% "aquatic", !is.na(ecosystem)),
       aes(x = func_group_type, y = prop, fill = importance_td_vs_bu)) +
  geom_col(position = position_dodge(width = 0.8, preserve = "single"), width = 0.7) +
  facet_wrap(~ ecosystem) +
  theme_bw(base_size = 7) + labs(x = "", y = "Proportion of studies", 
                                 fill = "Process emphasis") +
  scale_fill_manual(values = c("#586BA4","#F76C5E", "#F5DD90", "grey70")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.key.size = unit(0.4, "cm"))
#ggsave("figures/td_bu_emphasis_by_func_groups_prop.jpg", width = 4, height = 3)

#fig 4 - td vs bu across ecosystems
td_eco_df <- td_bu_final |>
  mutate(ecosystem = str_replace_all(ecosystem, ";", ",")) |>
  separate_rows(ecosystem, sep = ",\\s*") |>
  mutate(ecosystem = str_trim(ecosystem),
         importance_td_vs_bu = if_else(is.na(importance_td_vs_bu), 
                                       "Not specified", importance_td_vs_bu)) |>
  distinct(study, ecosystem, .keep_all = TRUE) |>
  group_by(ecosystem, importance_td_vs_bu) |>
  summarise(n = n(), .groups = "drop") |>
  mutate(importance_td_vs_bu = factor(importance_td_vs_bu, levels = c("td", "bu", "both", "NA")),
          ecosystem = factor(ecosystem, levels = c("freshwater","marine", "estuary")))

ggplot(td_eco_df |> filter(!ecosystem %in% "aquatic" , !is.na(ecosystem)),
       aes(x = ecosystem, y = n, fill = importance_td_vs_bu)) +
  geom_col(position = position_dodge(width = 0.8, preserve = "single"), width = 0.7) +
  theme_bw(base_size = 7) +
  scale_fill_manual(values = c("#586BA4","#F76C5E", "#F5DD90", "grey70")) +
  labs(x = "", y = "Number of studies", fill = "Process emphasis") +
  theme(panel.grid = element_blank(),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.key.size = unit(0.4, "cm"))
#ggsave("figures/phyto_func_group_process_emphasis_by_ecosystem_raw.jpg", width = 4, height = 3)

#proportions
td_eco_prop <- td_eco_df |>
  group_by(ecosystem) |>
  mutate(prop = n / sum(n))

#Figure S2
ggplot(td_eco_prop |> filter(!ecosystem %in% "aquatic", !is.na(ecosystem)),
       aes(x = ecosystem, y = prop, fill = importance_td_vs_bu)) +
  geom_col(position = position_dodge(width = 0.8, preserve = "single")) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("#586BA4","#F76C5E", "#F5DD90", "grey70")) +
  theme_bw(base_size = 7) +
  labs(x = "", y = "Proportion of studies", fill = "Process emphasis") +
  theme(panel.grid = element_blank(),
        legend.position = "top",
        legend.direction = "horizontal")
#ggsave("figures/phyto_func_group_process_emphasis_by_ecosystem_prop.jpg", width = 4, height = 3)

#------------------------------------------------------------------------------#
#statistical tests for each research question 



# ---- RQ1: ecosystem x process emphasis (secondary check) ----
eco_proc_mat <- td_eco_df |>
  filter(!ecosystem %in% "aquatic", !is.na(ecosystem)) |>
  tidyr::pivot_wider(names_from = importance_td_vs_bu, values_from = n, values_fill = 0) |>
  tibble::column_to_rownames("ecosystem") |>
  as.matrix()

set.seed(123)
fisher.test(eco_proc_mat, simulate.p.value = TRUE, B = 10000) # p = 0.3844
DescTools::CramerV(eco_proc_mat)                              # 0.111

# ---- RQ2: functional group type x ecosystem ----
fg_eco_table <- xtabs(~ ecosystem + func_group_type, data = stats_df)

set.seed(123)
fisher.test(fg_eco_table, simulate.p.value = TRUE, B = 10000) # p = 0.4985
DescTools::CramerV(fg_eco_table)                              # 0.084

# ---- RQ3: functional group type x process emphasis ----
proc_fg_table <- xtabs(~ func_group_type + importance_td_vs_bu,
                       data = filter(stats_df, importance_td_vs_bu != "NA"))

set.seed(123)
fisher.test(proc_fg_table, simulate.p.value = TRUE, B = 10000) # p = 0.9529
DescTools::CramerV(proc_fg_table)                              # 0.059

#no significant associations!!

#------------------------------------------------------------------------------#
# flowchard Figure 1
prisma <- grViz("
digraph prisma {
  graph [layout = dot, rankdir = TB, nodesep = 0.4, ranksep = 0.5]
  node [shape = box, style = filled, fillcolor = '#EFEFEF', fontname = Helvetica, fontsize = 11, width = 3]

  A [label = 'Papers identified through\\nWeb of Science search\\n(n = 855)']
  B [label = 'Duplicate papers removed\\n(n = 2)']
  C [label = 'Papers screened\\n(title/abstract)\\n(n = 853)']
  D [label = 'Papers excluded\\n(n = 313)']
  E [label = 'Full-text articles\\nassessed for eligibility\\n(n = 540)']
  F [label = < Full-text articles excluded<br/>(n = 287)<br/>
              <br align='left'/><B>Reasons:</B>
              <br align='left'/>1) Literature review or meta-analysis
              <br align='left'/>2) No phytoplankton in study
              <br align='left'/>3) No phytoplankton functional groups assessed>]
  G [label = 'Studies included\\nin review\\n(n = 253)', fillcolor = '#9DC5BB']

  A -> B -> C
  C -> D
  C -> E -> G
  E -> F
}
")
prisma |> export_svg() |> charToRaw() |> rsvg_png("figures/flow_chart.png", width = 1400)
