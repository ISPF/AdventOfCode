session_cookie = rstudioapi::askForSecret("Advent of Code Session Cookie")
cookie <- set_cookies(session = session_cookie)
response <- GET("https://adventofcode.com/2022/leaderboard/private/view/742479.json", cookie)
jsonFile <- fromJSON(rawToChar(response$content))
rm(session_cookie)
rm(cookie)
rm(response)

dt1 <- rbindlist(lapply(jsonFile$members, as.data.table), use.names = T)
dt1 <- unique(dt1[,.(name, id, local_score, stars, last_star_ts)])
dt1[, last_star_ts:=as.POSIXct(last_star_ts, origin="1970-01-01")]
dt1 <- dt1[order(local_score, decreasing = T)]

list1 <- lapply(jsonFile$members, FUN=function(x) {
  l1 <- x$completion_day_level
  if(length(l1)>0) {
    dtx <- data.table(id=x$id, as.data.table(t(unlist(x$completion_day_level))))
    dtx <- melt(dtx, id.vars = "id")
  dtxm1 <- dtx[variable %like% 'star_index']
  dtxm1 <- dtxm1[, .(id, start_index=gsub(".star_index", "", variable), value)]
  dtxm1 <- dcast(dtxm1, id~start_index)
  dtxm1 <- merge(dtxm1, dt1[,.(id, name)])
  setcolorder(dtxm1, c("name", "id"))
  }
})

list2 <- lapply(jsonFile$members, FUN=function(x) {
  l1 <- x$completion_day_level
  if(length(l1)>0) {
    dtx <- data.table(id=x$id, as.data.table(t(unlist(x$completion_day_level))))
    dtx <- melt(dtx, id.vars = "id")
    dtxm2 <- dtx[variable %like% 'star_ts']
    dtxm2[, value:=as.POSIXct(value, origin="1970-01-01")]
    dtxm2 <- dtxm2[, .(id, get_star_ts=gsub(".get_star_ts", "", variable), value)]
    dtxm2[, c("jour", "etape"):=tstrsplit(get_star_ts, "\\.")]
    dtxm2[, shiftJour:=as.integer(jour)]
    dtxm2[,origin:=as.POSIXct("2022-12-01 19:00:00", tz="HST")]
    dtxm2[,origin:=origin + lubridate::days(shiftJour-2)]
    dtxm2[, time_to_solve:=difftime(value, origin, units = "mins")]
    #dtxm2 <-dcast(dtxm2[,.(id, get_star_ts, time_to_solve)], id~get_star_ts, value.var = "time_to_solve")
    dtxm2 <- merge(dtxm2, dt1[,.(id, name)])
    setcolorder(dtxm2, c("name", "id"))
  }
})

dt2 <- rbindlist(list1, fill=T, use.names = T)
dt3 <- rbindlist(list2, fill=T, use.names = T)
setorderv(dt3, cols=c("jour", "etape", "time_to_solve"))

dt3[,.(mean(time_to_solve))]

