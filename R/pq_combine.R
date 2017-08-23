examples.pq.combine.answers = function() {
  id = "Kap4_1_Gutschein"
  pq.combine.answers(id=id)
  dat = pq.compute.points(id=id)
  udat = pq.compute.user.points(userid="Guest1")
}

load.pq.answers = function(id = pq$id, task.dir = pq.task.dir(id=id), pq=NULL) {
  restore.point("load.pq.answers")
  new.answer = pq.has.new.answer(id=id, task.dir=task.dir)

  if (new.answer) {
    return(pq.combine.answers(id=id, task.dir=task.dir))
  }

  file = file.path(task.dir,"answers.rds")
  if (file.exists(file)) {
    df = readRDS(file)
    return(df)
  }
  return(NULL)
}

pq.has.new.answer = function(id = pq$id, task.dir = pq.task.dir(id=id)) {

  dir = file.path(task.dir,"answers")
  files = list.files(dir, pattern = glob2rx("*.pqa"),full.names = TRUE)
  if (length(files)==0)
    return(FALSE)

  ans.file = file.path(task.dir,"answers.rds")
  if (!file.exists(ans.file))
    return(TRUE)

  any(file.mtime(files) > file.mtime(ans.file))
}


pq.combine.answers = function(id = pq$id, task.dir = pq.task.dir(id=id), save=!TRUE) {
  restore.point("pq.combine.answers")

  dir = file.path(task.dir,"answers")
  files = list.files(dir, pattern = glob2rx("*.pqa"),full.names = TRUE)
  if (length(files)==0) return(NULL)

  li = lapply(files, function(file) {
    restore.point("innner")
    pqa = readRDS(file)
    do.call(data_frame,pqa)
  })
  df = bind_rows(li)
  df = df %>%
    arrange(time) %>%
    mutate(answer.ind = seq_len(n())) %>%
    select(answer.ind, everything())

  if (save) {
    out.file = file.path(task.dir,"answers.rds")
    saveRDS(df,out.file)
  }
  df
}

pq.compute.points = function(id=pq$id, pq=NULL, pq.dir = get.pq.dir(), points.write = c(6,3,1,0), points.guess = c(3,2,1,0), db = get.pqdb(pq.dir=pq.dir)) {
  restore.point("pq.points")
  df = dbGet(db,"pqguess",nlist(id=id))


  # compute points average for writing
  wdf = df %>%
    mutate(userid=writerid, wpoints = points.write[rank]) %>%
    group_by(userid) %>%
    summarize(write.points=round(mean(wpoints,na.rm=TRUE),1),
      first = sum(rank==1),
      second = sum(rank==2),
      third = sum(rank==3),
      fourth = sum(rank==4)
    )

  # average points for guessing
  gdf = df %>%
    filter(writerid=="SOLUTION") %>%
    mutate(userid = responderid,  gpoints = points.guess[rank]) %>%
    group_by(userid) %>%
    summarize(guess.points=round(mean(gpoints,na.rm=TRUE),1))


  rdf = full_join(wdf,gdf, by="userid")
  rdf[is.na(rdf)] = 0

  rdf = rdf %>%
    mutate(points = write.points+guess.points, has.written = userid %in% wdf$userid, has.guessed = userid %in% gdf$userid) %>%
    select(userid, points, write.points, guess.points, everything()) %>%
    arrange(desc(points))
  rdf
}


pq.compute.user.points = function(userid, pq.dir = get.pq.dir(), points.write = c(6,3,1,0), points.guess = c(3,2,1,0), db = get.pqdb(pq.dir=pq.dir), id=NULL) {
  restore.point("pq.compute.user.points")

  if (is.null(id)) {
    param = NULL
  } else {
    param = list(id=id)
  }

  # compute points average for writing
  dfw = dbGet(db,"pqguess",c(param,nlist(writerid=userid)),empty.as.null = FALSE)

  wdf = dfw %>%
    mutate(userid=writerid, wpoints = points.write[rank]) %>%
    group_by(id, userid) %>%
    summarize(write.points=round(mean(wpoints,na.rm=TRUE),1),
      first = sum(rank==1),
      second = sum(rank==2),
      third = sum(rank==3),
      fourth = sum(rank==4)
    ) %>% ungroup()

  dfg = dbGet(db,"pqguess",c(param,nlist(responderid=userid, writerid="SOLUTION")), empty.as.null = FALSE)

  # average points for guessing
  gdf = dfg %>%
    mutate(userid = responderid,  gpoints = points.guess[rank]) %>%
    group_by(id,userid) %>%
    summarize(guess.points=round(mean(gpoints,na.rm=TRUE),1)) %>%
    ungroup()


  rdf = full_join(wdf,gdf, by=c("id","userid"))
  rdf[is.na(rdf)] = 0

  rdf = rdf %>%
    mutate(points = round(write.points+guess.points,1), has.written = userid %in% wdf$userid, has.guessed = userid %in% gdf$userid) %>%
    select(userid,id, points, write.points, guess.points, everything())
  rdf
}
