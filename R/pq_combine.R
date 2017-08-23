examples.pq.combine.answers = function() {
  id = "Kap4_1_Gutschein"
  pq.combine.answers(id=id)
  dat = pq.compute.points(id=id)
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
    summarize(write.points=mean(wpoints,na.rm=TRUE),
      num.first = sum(rank==1),
      num.second = sum(rank==2),
      num.third = sum(rank==3),
      num.fourth = sum(rank==4)
    )

  # average points for guessing
  gdf = df %>%
    filter(writerid=="SOLUTION") %>%
    mutate(userid = responderid,  gpoints = points.guess[rank]) %>%
    group_by(userid) %>%
    summarize(guess.points=mean(gpoints,na.rm=TRUE))


  rdf = full_join(wdf,gdf, by="userid")
  rdf[is.na(rdf)] = 0

  rdf = rdf %>%
    mutate(points = write.points+guess.points, has.written = userid %in% wdf$userid, has.guessed = userid %in% gdf$userid) %>%
    select(userid, points, write.points, guess.points, everything()) %>%
    arrange(desc(points))
  rdf
}
