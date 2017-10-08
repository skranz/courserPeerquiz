examples.pq.stud = function() {
  setwd("D:/libraries/courserPeerquiz/peerquiz")
  setwd("D:/libraries/courser/courses/vwl/course/peerquiz")

  app = eventsApp()
  apq = init.apq()

  app$ui = shiny::fluidPage(
    pq.guess.headers(),
    uiOutput("mainUI")
  )
  appInitHandler(function(...) {
    userid = app$userid = paste0("Guest_", sample.int(4,1))
    setUI("mainUI",tagList(
      h4("Active Peer-Quizes"),
      active.pqs.ui(apq, userid=userid),
      h4("Closed Peer-Quizes"),
      closed.pqs.ui(apq = apq, userid=userid)
    ))
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
  if (!is.null(apq$pqs))
    apq$pqs$title = sapply(apq$pq.li, function(pq) pq$title)
  apq$lang = first.non.null(lang, if (length(apq$pq.li)>0) apq$pq.li[[1]]$lang,"en")

  # list of answer df for quizzes in guess mode
  rows = which(pqs$state == "guess")
  adf.li = lapply(rows, function(row) {
    pq = apq$pq.li[[row]]
    pq.get.answers.df(pq)
  })
  names(adf.li) = pqs$id[rows]
  apq$adf.li = adf.li

  apq
}


active.pqs.ui = function(apq, userid) {
  restore.point("make.apq.ui")

  if (NROW(apq$pqs)==0)
    return(p("---"))

  pqs = apq$pqs %>%
    filter(state %in% c("write","guess")) %>%
    arrange(state_change_date)

  userhash = digest(userid)
  library(shinyBS)

  li = lapply(seq_len(NROW(pqs)), function(row) {
    restore.point("apq.ui.inner.panel")
    id = pqs$id[row]
    ns = NS(paste0("apq-",id))
    pq = apq$pq.li[[id]]

    state = pqs$state[[row]]
    state_desc = pq_string(apq$lang)$state_desc[[state]]
    stop.time = pqs$state_change_date[[row]]-60L
    title = paste0(pq$title, " (",state_desc," ", format(stop.time, pq_string(apq$lang)$datetime_format), ")")
    if (state == "write") {
      ui = peerquiz.write.ui(pq,userid = userid)
    } else if (state == "guess") {
      ui = uiOutput(ns("guessUI"))
      pgu = get.apq.pgu(apq=apq, pq=pq, userid=userid)
      set.pgu.ui(container.id = ns("guessUI"),pq=pq,pgu=pgu)
    } else {
      ui = HTML("No ui implemented")
    }

    #tabPanel(title=title, ui)
    slimCollapsePanel(title=title, ui,heading.style=paste0("padding-top:  5px; padding-bottom: 5px;"))
  })

  #ui = do.call(tabsetPanel,li)
  ui = tagList(li)
  withMathJaxNoHeader(ui)
}


closed.pqs.ui = function(apq, userid) {
  restore.point("make.apq.ui")
  pqs = apq$pqs %>%
    filter(state %in% c("after")) %>%
    arrange(end_guess)

  userhash = digest(userid)
  library(shinyBS)

  li = lapply(seq_len(NROW(pqs)), function(row) {
    restore.point("apq.ui.inner.panel")
    id = pqs$id[row]
    ns = NS(paste0("apq-",id))
    pq = apq$pq.li[[id]]

    state = apq$pqs$state[[row]]
    title = paste0(pq$title)
    ui = pq.after.ui(pq=pq,userid=userid)

    #tabPanel(title=title, ui)
    slimCollapsePanel(title=title, ui,heading.style=paste0("padding-top:  5px; padding-bottom: 5px;"))
  })

  #ui = do.call(tabsetPanel,li)
  ui = tagList(li)
  withMathJaxNoHeader(ui)
}


get.apq.pgu = function(apq, pq, userid) {
  restore.point("get.apq.pgu")
  task.dir=pq.task.dir(pq=pq,pq.dir = apq$pq.dir)
  state = get.user.pgu.state(pq=pq, userid=userid)

  if (state == "no") {
    return(new.apq.pgu(apq=apq,pq=pq, userid))
  }

  dir = file.path(task.dir,paste0("pgu_", state))
  readRDS(file.path(dir, digest(userid)))
}

# A guess object based on apq and pq data
new.apq.pgu = function(apq,pq, userid) {
  restore.point("new.apq.pgu")
  adf=apq$adf.li[[pq$id]]
  ans = select.guess.choices(adf = adf, responderid=userid,n = first.non.null(pq[["num.ans"]],4))
  pgu = new.pgu(pq=pq,responderid=userid,ans=ans)
  set.pgu(pgu)

  # save
  dir = file.path(pq.task.dir(pq=pq, pq.dir=apq$pq.dir), "pgu_assigned")
  if (!dir.exists(dir))
    dir.create(dir, recursive=TRUE)
  file = file.path(dir,  digest(userid))
  saveRDS(pgu, file)

  pgu
}

pq.state.stop.time = function(pqs) {
  #stop.time =
}

change.time = function(x, sec=min*60, min=hour*60, hour=day*24, day=0) {
  restore.point("fhduhf")
  x-as.integer(sec)
  #as.POSIXct(as.integer(as.POSIXct(x))-sec)
}

get.pq.states = function(tt = pq.load.time.table(pq.dir=pq.dir, convert.date.times=TRUE),pq.dir=get.pq.dir(), only.active = TRUE) {
  restore.point("get.pq.states")
  time = Sys.time()

  if (only.active) {
    tt = filter(tt, active==TRUE)
  }
  if (NROW(tt)==0)
    return(NULL)

  pqs = tt %>%
    mutate(
      state =
        ifelse(is.true(time >= end_guess),"after",
        ifelse(is.true(time >= start_guess),"guess",
        ifelse(is.true(time >= start_write),"write",
        "before"))),
      state.prio = match(state, rev(c("write", "guess","after","before")))
    ) %>%
    mutate(
      state_change_date = case_when(
        state=="guess"~end_guess,
        state=="write"~start_guess,
        state=="before"~start_write,
        TRUE ~ rep(as.POSIXct(NA), n())
      ),
      state_change_sec = as.integer(state_change_date)-as.integer(time)
    ) %>%
    select(id, state, state_change_date, state_change_sec, start_write, start_guess, end_guess,active)

  pqs

}


pq.after.ui = function(userid,id=pq$id, pq=load.or.compile.pq(id=id), pgu=NULL) {
  restore.point("pgu.after.ui")

  pqa = load.pq.answer(pq=pq, userid=userid)
  sol = load.pq.sample.sol(pq=pq)
  pdf = pq.compute.user.points(userid=userid, id=id)


  ui = withMathJaxNoHeader(tagList(
    HTML(pq$question_html),
    h3(pq_string(pq$lang)$sample_sol),
    sol$answer.ui,
    h3(pq_string(pq$lang)$your_sol),
    pqa$answer.ui,
    h3(pq_string(pq$lang)$num_guesses),
    HTML(html.table(select(pdf,first, second, third, fourth), header=paste0(pq_string(pq$lang)$Rank, " ",1:4))),
    h3(pq_string(pq$lang)$points),
   HTML(html.table(select(pdf,write.points, guess.points, points), header=c("From people guessing your answer","From your guess","Total"),round.digits = 2))
  ))
  ui
}
