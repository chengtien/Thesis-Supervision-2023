"https://codis.cwb.gov.tw/api/station_list" |>
  jsonlite::fromJSON()
"/Users/martin/Downloads/C-B0024-002.json" |>
  jsonlite::fromJSON() -> ww
"/Users/martin/Downloads/氣象局測站列表.json" |>
  jsonlite::fromJSON() -> wstations
"/Users/martin/Downloads/O-A0001-001.json" |>
  jsonlite::fromJSON() -> auto_wstations
"https://opendata.cwb.gov.tw/fileapi/v1/opendataapi/O-A0003-001?Authorization=rdec-key-123-45678-011121314&format=JSON" |>
  jsonlite::fromJSON() -> wdata
