library(leaflet)
library(htmltools)
library(dplyr)
library(tidyr)

data <-
  pv_res$data$patents %>%
  unnest(assignees) %>%
  select(assignee_id, assignee_organization, patent_number,
         assignee_longitude, assignee_latitude) %>%
  group_by_at(vars(-matches("pat"))) %>%
  mutate(num_pats = n()) %>%
  ungroup() %>%
  select(-patent_number) %>%
  distinct() %>%
  mutate(popup = paste0("<font color='Black'>",
                        htmlEscape(assignee_organization), "<br><br>Patents:",
                        num_pats, "</font>")) %>%
  mutate_at(vars(matches("_l")), as.numeric) %>%
  filter(!is.na(assignee_id))

leaflet(data) %>%
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
  addCircleMarkers(lng = ~assignee_longitude, lat = ~assignee_latitude,
                   popup = ~popup, ~sqrt(num_pats), color = "yellow")