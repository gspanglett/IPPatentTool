# Copyright 2020-2021, Sarah Crothers <scrot021@uottawa.ca> and Gareth Spanglett <gspan042@uottawa.ca>
# Authors:  Gareth Spanglett, Sarah Crothers
# GNU General Public License v3.0
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


if (!require(devtools)) install.packages("devtools")
devtools::install_github("ropensci/patentsview")

library(patentsview)


query <- with_qfuns( # with_qfuns is basically just: with(qry_funs, ...)
  and(
    gte(patent_date = "2014-02-01"),
    lt(patent_date = "2014-03-01")
  )
)

fields <- c(
  c("patent_abstract","patent_average_processing_time","patent_date","patent_firstnamed_assignee_country","patent_firstnamed_assignee_id","patent_firstnamed_assignee_location_id","patent_firstnamed_inventor_country","patent_firstnamed_inventor_id","patent_firstnamed_inventor_location_id","patent_kind","patent_num_cited_by_us_patents","patent_num_claims","patent_num_combined_citations","patent_num_foreign_citations","patent_num_us_application_citations","patent_num_us_patent_citations","patent_number","patent_processing_time","patent_title","patent_type","patent_year"),
  get_fields(endpoint = "patents", groups = c("assignees", "inventors", "cpcs", "ipcs", "pct_data"))
)

# Send HTTP request to API's server:
pv_res <- search_pv(query = query, fields = fields, all_pages = TRUE)
