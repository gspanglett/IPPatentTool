library(ggplot2)
library(RColorBrewer)

data <-
  pv_res$data$patents %>%
  unnest(cpcs) %>%
  mutate(
    title = case_when(
      grepl("filtering", .$cpc_group_title, ignore.case = T) ~
        "Filtering policies",
      grepl("Firewall traversal", .$cpc_group_title, ignore.case = T) ~
        "Firewall traversal",
      TRUE ~
        .$cpc_group_title
    )
  ) %>%
  mutate(title = gsub(".*(?=-)-", "", title, perl = TRUE)) %>%
  group_by(title, patent_year) %>%
  count() %>%
  ungroup() %>%
  mutate(patent_year = as.numeric(patent_year))

ggplot(data = data) +
  geom_smooth(aes(x = patent_year, y = n, colour = cpc_group_title), se = FALSE) +
  scale_x_continuous("\nPublication year", limits = c(2007, 2016),
                     breaks = 2007:2016) +
  scale_y_continuous("Patents\n", limits = c(0, 700)) +
  scale_colour_manual("", values = brewer.pal(5, "Set2")) +
  theme_bw() + # theme inspired by https://hrbrmstr.github.io/hrbrthemes/
  theme(panel.border = element_blank(), axis.ticks = element_blank())