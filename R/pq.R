# State of a peerquiz
PQ_PRE = 0
PQ_WRITE = 1
PQ_GUESS = 2
PQ_CLOSED = 3

get.pq.dir = function(...) {
  pq.opts()$pq.dir
}

pq.task.dir = function(pq, id=pq$id, pq.dir = get.pq.dir()) {
  file.path(pq.dir,"tasks", id)
}

get.pq.userid = function(app=getApp()) {
  app$userid
}

pq.opts = function(app=getApp()) {
  restore.point("pq.opts")
  if (!is.null(app[["pq.opts"]]))
    return(app$pq.opts)

  pq.opts = getOption("peerquiz.options")
  if (is.null(pq.opts)) {
    pq.opts = init.pq.opts()
    set.pq.opts(pq.opts)
  }
  pq.opts
}

set.pq.opts = function(pq.opts = init.pq.opts(), app=getApp()) {
  if (!is.null(app)) {
    app$pq.opts = pq.opts
  }
  options(pq.opts=pq.opts)
  invisible(pq.opts)
}

init.pq.opts = function(pq.dir = getwd()) {
  nlist(pq.dir)
}

load.pq = function(id, pq.file = file.path(dir,paste0(id,".pq")),  dir = pq.task.dir(id=id)) {
  restore.point("load.pq")
  return(readRDS(pq.file))
}

compile.and.save.pq = function(id=NULL, pq=NULL, pq.dir = get.pq.dir(), source.dir = file.path(pq.dir,"sources"), save=TRUE, db=NULL, state= PQ_PRE) {
  restore.point("create.pq")

  if (is.null(pq)) {
    source.files = list.files(source.dir,pattern = glob2rx(paste0(id,".*")))
    if (length(source.files)==0) {
      stop(paste0("No .Rmd or .yaml source file starting with id '", id,"' found in peerquiz sources directory '",source.dir,"'."))
    }
    file = source.files[1]
    txt = readUtf8(file.path(source.dir,file))
    if (tolower(tools::file_ext(file))=="rmd") {
      pq = parse.hashdot.yaml(txt=txt)
    } else {
      yaml = merge.lines(txt)
      pq = read.yaml(text=yaml)
    }
  }

  pq$question_html = md2html(pq$question)
  if (!is.null(title)) {
    pq$question_html = paste0("<h3>", pq$title,"</h3>\n", pq$question_html)
  }
  pq$ns = NS(paste0("pq_",pq$id))
  pq$sol_div_id = paste0("sol_div_",1:8,"_",pq$id)
  pq$num.sol.click = 2
  pq$lang = first.none.null(pq$lang, "en")
  pq$str = pq_strings(pq$lang)

  if (is.null(pq$ace_lines)) {
    pq$ace_lines = 10
  }

  pq = init.pq.form(pq)
  if (!is.null(pq$funs)) {
    txt = sep.lines(pq$funs)

    code = unlist(find.rmd.chunks(txt, add.code=TRUE)$code)
    if (length(code)>0) {
      call = parse(text=code)
      env = new.env()
      eval(call, env)
      pq = c(pq,as.list(env))
    }
  }
  if (is.null(pq$render.answer.fun) & !is.null(pq[["form"]])) {
    pq$render.answer.fun = pq.default.render.answer.fun
  }

  # save pq file
  if (save) {
    task.dir = pq.task.dir(pq)
    if (!dir.exists(task.dir)) {
      dir.create(task.dir, recursive=TRUE)
      dir.create(file.path(task.dir,"answers"))
    }
    saveRDS(pq, file.path(task.dir, paste0(pq$id,".pq")))
    save.pq.sample.sol(pq=pq)

    restore.point("before.db.entry")
    close.db = is.null(db)
    if (is.null(db)) db = get.pqdb(pq.dir=pq.dir)
    dbInsert(db, "pqstate", list(id=pq$id, state = state, writestart=NA, writeend=NA, guessstart=NA, guessend=NA),mode = "replace")
    if (close.db) dbDisconnect(db)

  }


  pq
}
