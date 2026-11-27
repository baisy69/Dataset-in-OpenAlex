# Short Paper
# What is a Dataset in OpenAlex? A Source-Level Audit of Data Repository Coverage and Quality

library("dplyr")
library("ggplot2")
library("tidyr")
library("ggforce")
library("scales")
library("ggh4x")
library(patchwork)

openalex <- read.csv("openalex_sources_fullversion.csv")
openalex_xpac <- read.csv("openalex_sources_100k.csv")
openalex_datasets <- read.csv("openalex_sources_dataset_count.csv")
options(scipen = 999)

# Methods
# Data
# Table 1 Total Number of Datasets in OpenAlex Broken Down by Subcollections
total_count <- openalex |> 
  summarise(
    xpac = sum(n_xpac_true, na.rm = TRUE),
    nonxpac = sum(n_xpac_false, na.rm = TRUE), 
    total = sum(n_datasets_primary, na.rm = TRUE)
  )
total_count

top200_count <- openalex_datasets |> 
  summarise(
    xpac = sum(n_xpac_true, na.rm = TRUE),
    xpac_percentage = xpac/116315517,
    nonxpac = sum(n_xpac_false, na.rm = TRUE), 
    nonxpac_percentage = nonxpac/30881474,
    total = sum(n_datasets_primary, na.rm = TRUE),
    total_percentage = total/144562156
  )
top200_count

# Classification of data repository - related to Table 2
# Appendix TableA1 Examples of Sources by Source Type
openalex_datasets$source_type <- factor(openalex_datasets$source_type, levels = c("Data repository", "General repository", "Book series", "Others"))
top5_SourceTitle_Type <- openalex_datasets %>%
  select(source_type, source_display_name, n_datasets_primary) %>%  
  group_by(source_type) %>%                      
  slice_max(order_by = n_datasets_primary, n = 5)
top5_SourceTitle_Type

# Results
# Descriptive results of datasets in OpenAlex
# Figure 1 Distribution of OpenAlex Sources by Record Counts
# Figure 1A
openalex_xpac$Type <- factor(openalex_xpac$Type, levels = c("All", "XPAC", "Non-XPAC"))

xpac_compare_facet <- ggplot(data = openalex_xpac) +
  geom_histogram(aes(x = Counts), binwidth = 10000, boundary = 0, fill= "#404080", color = "#404080", alpha=0.6) +
  scale_x_continuous(breaks = seq(0, 300000, by = 30000), labels = label_number(scale_cut = cut_short_scale())) +
  theme_linedraw() +
  facet_wrap(. ~ Type, nrow = 2, axes = "all") +   
  labs(
    x = "Record Counts of XPAC and Non-XPAC",
    y = "Source Counts") +  
  theme(
    plot.margin = margin(5.1, 15, 4.1, 2.1, "pt"),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15)),
    axis.text = element_text(size = 35), 
    axis.title = element_text(size = 30), 
    strip.text = element_text(size = 35)
  ) 

ggsave("Figure_1A.png", plot = xpac_compare_facet, , width = 15, height = 10, dpi = 500)

# Figure 1A - Loop 
xpac_compare_facet2 <- ggplot(data = openalex_xpac) +
  geom_histogram(aes(x = Counts), binwidth = 10000, boundary = 0, fill= "#404080", color = "#404080", alpha=0.6) +
  scale_x_continuous(breaks = seq(0, 100000, by = 10000), labels = label_number(scale_cut = cut_short_scale())) + 
  theme_linedraw() + 
  facet_wrap_paginate(. ~ Type, nrow = 1, ncol = 1, page = 1) +   
  labs(
    x = "Record Counts of XPAC and Non-XPAC",
    y = "Source Counts") +  
  theme(
    plot.margin = margin(5.1, 15, 4.1, 2.1, "pt"),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15)),
    axis.text = element_text(size = 35), 
    axis.title = element_text(size = 30), 
    strip.text = element_text(size = 35)
  )

xpac_compare_loop <- n_pages(xpac_compare_facet2)
xpac_compare_loop

for (i in seq_len(xpac_compare_loop)) { p_i <- ggplot(data = openalex_xpac) +
  geom_histogram(aes(x = Counts), binwidth = 10000, boundary = 0, fill= "#404080", color = "#404080", alpha=0.6) +
  scale_x_continuous(breaks = seq(0, 300000, by = 30000), labels = label_number(scale_cut = cut_short_scale())) +
  theme_linedraw() +
  facet_wrap_paginate(. ~ Type, nrow = 1, ncol = 1, page = i) +   
  labs(
    x = "Record Counts of XPAC and Non-XPAC",
    y = "Source Counts") +  
  theme(
    plot.margin = margin(5.1, 15, 4.1, 2.1, "pt"),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15)),
    axis.text = element_text(size = 35), 
    axis.title = element_text(size = 30), 
    strip.text = element_text(size = 35)
  )

ggsave(filename = paste0("Figure_1A_", i, ".png"), plot = p_i, width = 9, height = 6, dpi = 500) }

# Figure 1B 
top58_200 <- openalex_datasets[58:200, ]

point_compare_facet1 <- ggplot(data = top58_200) +
  geom_point(aes(x = n_xpac_true, y = n_xpac_false), color = "#404080", size = 4) +
  scale_x_continuous(breaks = seq(0, 100000, by = 50000), labels = label_number(scale_cut = cut_short_scale())) +
  scale_y_continuous(breaks = seq(0, 100000, by = 50000), labels = label_number(scale_cut = cut_short_scale())) +
  labs(
    x = "XPAC Record Counts",
    y = "Non-XPAC Record Counts") +  
  theme_linedraw() +
  theme(
    axis.title.x = element_text(margin = margin(t = 15)), 
    axis.title.y = element_text(margin = margin(r = 15)),
    axis.text = element_text(size = 35), 
    axis.title = element_text(size = 30), 
    panel.widths  = unit(28, "cm")
  )

ggsave("Figure_1B.png", plot = point_compare_facet1, width = 14, height = 10, dpi = 500)

# Correlation between XPAC and non-XPAC
corr_xpac <- cor.test(top58_200$n_xpac_true, top58_200$n_xpac_false, method = "pearson")
corr_xpac

# Final Figure 1
side_by <- xpac_compare_facet + 
  point_compare_facet1 +
  plot_annotation(tag_levels = "A") & 
  theme(plot.tag = element_text(size = 30, face = "bold"))

ggsave("Figure_1.png", plot = side_by, width = 25, height = 10, dpi = 500)

# Table 3 Top 10 Sources with the Most Record Counts
top10_sourcetitle <- openalex_datasets %>%
  select(source_display_name, n_xpac_true, n_xpac_false, n_datasets_primary) %>%  
  slice_max(order_by = n_datasets_primary, n = 10)
top10_sourcetitle 

# Distribution of Datasets and Sources by the Source Type
# Table 4 Total Records and Sources by Source Type Among the Top 200 Sources
top200_type_count <- openalex_datasets %>% 
  group_by(source_type) %>% 
  summarise(
    count = n(),
    source_percentage = count/200,
    nonXPAC = sum(n_xpac_false, na.rm = TRUE),
    nonXPAC_percentage = nonXPAC/30296552,
    total = sum(n_datasets_primary, na.rm = TRUE),
    total_percentage = total/142995631 
  )
top200_type_count

# Table 5 Sources Matched to the Two Registries by Source Types
registry_matching <- openalex_datasets %>% 
  group_by(source_type) %>% 
  summarise(
    count = n(),
    re3data = sum(re3data_included == "TRUE", na.rm = TRUE),
    re3data_percentage = re3data/count,
    DataCite = sum(datacite_in == "TRUE", na.rm = TRUE),
    DataCite_percentage = DataCite/count)      
registry_matching

# Table 6 Distribution of Sources Across the Share of Datasets Among All Items in the Non-XPAC Collection
dataset_share <- openalex_datasets %>% 
  group_by(source_type) %>% 
  summarise(
    count = n(),
    nonxpac_100 = sum(dataset_share_nonxpac == 1.000, na.rm = TRUE), 
    nonxpac_100_percentage = nonxpac_100/count,
    nonxpac_50 = sum(dataset_share_nonxpac < 0.500, na.rm = TRUE),
    nonxpac_50_percentage = nonxpac_50/count,
    mean = round(mean(dataset_share_all), digits =4)
    )
dataset_share
