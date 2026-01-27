
# MWTL data: download and write to csv files

dir_MWTL <- "/Users/robvb/Documents/projects/25-0618 KEC6 kaarten/data"

source("/Users/robvb/Documents/github/MWTL_surveys/R/pg_connection.R")
source("/Users/robvb/Documents/github/MWTL_surveys/MWTL_db_creds.r")
  
d_MWTL_obs <- RPostgres::dbGetQuery(con,  paste0("SELECT * FROM noordzeetellingen.v_esas_export_observations"))
d_MWTL_pos <- RPostgres::dbGetQuery(con,  paste0("SELECT * FROM noordzeetellingen.v_esas_export_position"))
d_MWTL_tri <- RPostgres::dbGetQuery(con,  paste0("SELECT * FROM noordzeetellingen.v_esas_export_trip"))

write.table(
  d_MWTL_obs,
  file = file.path(
    dir_MWTL,
    "observations.csv"
  ),
  sep = ";",
  row.names = FALSE
)
write.table(
  d_MWTL_pos,
  file = file.path(
    dir_MWTL,
    "position.csv"
  ),
  sep = ";",
  row.names = FALSE
)
write.table(
  d_MWTL_tri,
  file = file.path(
    dir_MWTL,
    "trip.csv"
  ),
  sep = ";",
  row.names = FALSE
)



