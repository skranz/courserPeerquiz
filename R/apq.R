examples.pq.stud = function() {
  setwd("D:/libraries/courserPeerquiz/peerquiz")
  app = eventsApp()
  apq = init.apq()
  app$ui = shiny::fluidPage(
    uiOutput("mainUI")
  )
  appInitHandler(function(...) {
    setUI("mainUI",active.pqs.ui(apq))
  })
  viewApp(app)
}

init.apq = function(pq.dir=get.pq.dir(), tt = pq.load.time.table(pq.dir=pq.dir, convert.date.times=TRUE), lang=NULL) {
  restore.point("init.apq")
  apq = list(
    pq.dir = pq.dir,
    tt = tt
  )
  apq$pqs = pqs = get.pq.states(tt=tt)
  apq$pq.li =  lapply(pqs$id, load.or.compile.pq, pq.dir=pq.dir)
  names(apq$pq.li) = pqs$id
  apq$pqs$title = sapply(apq$pq.li, function(pq) pq$title)
  apq$lang = first.non.null(lang, apq$pq.li[[1]]$lang,"en")
  apq$strings = apq_strings(lang=apq$lang)
  apq
}

active.pqs.ui = function(apq, upq=NULL) {
  restore.point("make.apq.ui")
  pqs = apq$pqs %>% filter(state %in% c("write","guess"))

  library(shinyBS)
  require(MathjaxLocal)

  li = lapply(seq_len(NROW(pqs)), function(row) {
    restore.point("apq.ui.inner.panel")
    id = pqs$id[row]
    ns = NS(paste0("apq-",id))
    pq = apq$pq.li[[id]]

    state = apq$pqs$state[[row]]
    state_desc = apq$strings$state_desc[[state]]
    title = paste0(pq$title, " (",state_desc,")")
    if (state == "write") {
      ui = peerquiz.write.ui(pq)
    } else if (state == "guess") {
      ui = uiOutput(ns("guessUI"))
      set.pgu.ui(container.id = ns("guessUI"),pq=pq)
    } else {
      ui = HTML("No ui implemented")
    }

    #tabPanel(title=title, ui)
    slimCollapsePanel(title=title, ui,heading.style=paste0("padding-top:  5px; padding-bottom: 5px;"))
  })

  #ui = do.call(tabsetPanel,li)
  ui = tagList(li)
  withMathJax(ui)
}

pq.state.stop.time = function(pqs) {
  #stop.time =
}

apq.ui = function(apq) {
  restore.point("make.apq.ui")
  pqs = apq$pqs
  library(shinyBS)
  require(MathjaxLocal)

  li = lapply(seq_len(NROW(pqs)), function(row) {
    restore.point("apq.ui.inner.panel")
    id = pqs$id[row]
    ns = NS(paste0("apq-",id))
    pq = apq$pq.li[[id]]

    state = apq$pqs$state[[row]]
    state_desc = apq$strings$state_desc[[state]]
    stop.time = pqs$start_guess[[row]]
    title = paste0(pq$title, " (",state_desc," ", format(stop.time), ")")
    if (state == "write") {
      ui = peerquiz.write.ui(pq)
    } else if (state == "guess") {
      ui = uiOutput(ns("guessUI"))
      set.pgu.ui(container.id = ns("guessUI"),pq=pq)
    } else {
      ui = HTML("No ui implemented")
    }

    slimCollapsePanel(title=title, ui,heading.style=paste0("padding-top:  5px; padding-bottom: 5px; background-color: #ccccff;"))
  })

  ui = tagList(li)
  withMathJax(ui)
}

get.pq.states = function(tt = pq.load.time.table(pq.dir=pq.dir, convert.date.times=TRUE),pq.dir=get.pq.dir()) {
  restore.point("get.pq.states")
  time = Sys.time()

  pqs = tt %>%
    filter(active) %>%
    mutate(
      state =
        ifelse(is.true(time >= end_guess),"after",
        ifelse(is.true(time >= start_guess),"guess",
        ifelse(is.true(time >= start_write),"write",
        "before"))),
      state.prio = match(state, rev(c("write", "guess","after","before")))
    ) %>%
    mutate(
      state_change_sec =
        ifelse(state=="after",NA_real_,
        ifelse(state=="guess",as.integer(end_guess)-as.integer(time),
        ifelse(state=="write",as.integer(start_guess)-as.integer(time),
        ifelse(state=="before",as.integer(start_write)-as.integer(time),
        NA
        ))))
    ) %>%
    select(id, state, state_change_sec, start_write, start_guess, end_guess)

  pqs

}
