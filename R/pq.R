# State of a peerquiz
PQ_PRE = 0
PQ_WRITE = 1
PQ_GUESS = 2
PQ_CLOSED = 3

get.pqs.dir = function(...) {
  pq.opts()$pqs.dir
}

get.pq.dir = function(pq, id=pq$id, pqs.dir = get.pqs.dir()) {
  file.path(pqs.dir, id)
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

init.pq.opts = function(pqs.dir = file.path(getwd(),"pq")) {
  nlist(pqs.dir)
}

example.peerquiz = function() {
  setwd("D:/libraries/peerquiz")
  file = "budget.yaml"
  app = eventsApp()
  pq = import.yaml()
  pq = create.pq(yaml.file=file)

  sols = c(list(pq$solution), pq$wrongsols)
  sols = lapply(sols,sol.source.to.secure.html)
  ui = peerquiz.guess.sol.ui(sols, pq=pq)

  ui = peerquiz.write.ui(pq)


  app$ui = fluidPage(
    # Add a CSS class for red text colour
    inlineCSS(list(
      .bgempty = "background: white",
      .bg1 = "background:  #FFD700",
      .bg2 = "background:  #D0D0D0",
      .bg3 = "background: ##CD7F32"
    )),
    withMathJax(ui)
  )
  viewApp()

  #view.html(ui=ui)
}

load.pq = function(id, pq.file = file.path(dir,paste0(id,".pq")),  dir = get.pq.dir(id=id)) {
  restore.point("load.pq")
  return(readRDS(pq.file))
}

create.pq = function(yaml=NULL,pq=NULL, file = NULL, pqs.dir = get.pqs.dir(), save=TRUE, db=NULL, state= PQ_PRE) {
  restore.point("create.pq")

  if (!is.null(file)) {
    txt = readUtf8(file)
    if (tolower(tools::file_ext(file))=="rmd") {
      pq = parse.hashdot.yaml(txt=txt)
    } else {
      yaml = merge.lines(txt)
    }
  }
  if (is.null(pq)) {
    pq = read.yaml(text=yaml)
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
    pq.dir = get.pq.dir(pq)
    if (!dir.exists(pq.dir)) {
      dir.create(pq.dir, recursive=TRUE)
      dir.create(file.path(pq.dir,"answers"))
    }
    saveRDS(pq, file.path(pq.dir, paste0(pq$id,".pq")))

    if (!is.null(yaml)) {
      writeLines(yaml,file.path(pq.dir, paste0(pq$id,".yaml")),useBytes = TRUE)
    }

    save.pq.sample.sol(pq=pq)

    restore.point("before.db.entry")
    close.db = is.null(db)
    if (is.null(db)) db = get.pqdb(pqs.dir=pqs.dir)
    dbInsert(db, "pqstate", list(id=pq$id, state = state, writestart=NA, writeend=NA, guessstart=NA, guessend=NA),mode = "replace")
    if (close.db) dbDisconnect(db)

  }


  pq
}
