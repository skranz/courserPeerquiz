example.peerquiz = function() {
  setwd("D:/libraries/courserPeerquiz/peerquiz")
  set.pq.opts()
  pq = load.pq("p-value")
  pq = load.pq("Kap1_Software_1")

  responderid = "guest"
  adf = pq.get.answers.df(pq=pq)

  ans = select.guess.choices(adf, responderid = responderid)
  pgu = set.pgu(new.pgu(pq=pq, ans=ans,responderid = responderid))

  app = eventsApp()
  app$ui = fluidPage(
    pq.guess.headers(),
    uiOutput("mainUI")
  )
  app$userid = paste0("Guest_", sample.int(1e6,1))
  appInitHandler(function(...) {
    set.pgu.ui("mainUI",pq=pq, pgu=pgu)
  })
  viewApp()

  #view.html(ui=ui)
}


pq.get.answers.df = function(pq) {
  restore.point("pq.get.answers.df")
  adf = load.pq.answers(pq=pq)
  db = get.pqdb()
  gdf = dbGet(db,"pqguess",nlist(id=pq$id))
  if (NROW(gdf)>0) {
    sgdf = gdf %>%
      mutate(userid=writerid) %>%
      group_by(userid) %>%
      summarize(num_guess = n())

    adf = left_join(adf, sgdf, by="userid") %>%
      mutate(num_guess = ifelse(is.na(num_guess),0,num_guess))
  } else {
    adf$num_guess = 0
  }
  adf
}

# select 4 choices for the responder
select.guess.choices = function(adf, responderid, n=4) {
  restore.point("select.guess.choices")
  adf$row = seq_len(NROW(adf))

  sol = filter(adf, is.sol, userid != responderid)
  ord = order(-sol$num_guess + runif(NROW(sol),0,0.0001))

  sol.row = sol$row[ord[1]]
  ans = filter(adf, !is.sol,  userid != responderid)
  ord = order(-ans$num_guess + runif(NROW(ans),0,0.0001))
  ans.rows = ans$row[ord[1:(n-1)]]

  rows = sample(c(sol.row,ans.rows),replace = FALSE)
  adf[rows,]
}


new.pgu = function(pq,responderid, ans= if(!is.null(adf)) select.guess.choices(adf = adf, responderid=responderid), num.ans = NROW(ans), adf = NULL, ...) {
  pgu = as.environment(list(id=pq$id,responderid=responderid, ans=ans, num.ans=num.ans, ans.div.id = paste0("ans-div-id-",seq_len(NROW(ans)),"-",pq$id)))
}

set.pgu = function(pgu, app=getApp()) {
  if (is.null(app[["pgu.li"]]))
    app$pgu.li = list()

  app$pgu.li[[pgu$id]] = pgu
  pgu
}

get.pgu = function(pq=NULL,id = pq$id, app=getApp()){
  if (is.null(app[["pgu.li"]]))
    app$pgu.li = list()
  if (is.null(app$pgu.li[[id]]))
    app$pgu.li[[id]] = new.pgu(pq=pq)
  app$pgu.li[[id]]
}

set.pgu.ui = function(container.id,pq, pgu = NULL) {
  restore.point("set.pgu.ui")

  ns = pq$ns
  ans = pgu$ans
  ui = pgu.ui(pq=pq,pgu = pgu)

  eventHandler("clickRankChange",id=pq$id,function(ranked,max_ranked, num_ranked, ...) {
    restore.point("cr.clickRankChange")
    ns = pq$ns
    ranked = unlist(ranked)
    if (length(ranked)>0) {
      ranked = ranked+1
      if (num_ranked == pgu$num.ans-1) {
        ranked = unique(c(ranked,1:pgu$num.ans))
      }
    }
    pgu$ranked = ranked
    cat("\nRanking:",paste0(ranked, collapse=", "))
    if (length(ranked)==0) {
      str = pq$str$not_yet_ranked
    } else {
      str = paste0(seq_along(ranked), ": ",pq$str$Answer," ", ranked,collapse="<br>")
    }
    ranking.ui = tagList(
      h4(pq$str$your_ranking,":"),
      p(HTML(str))
    )
    setUI(ns("ranking"), ranking.ui)
  })

  callJS("newClickRank",id=pq$id,div_ids=pgu$ans.div.id,max_ranked=3)

  buttonHandler(ns("submitGuessBtn"), function(...) {
    restore.point("submitGuessBtn")
    db = get.pqdb()

    idf = data_frame(id=pq$id,writerid = ans$userid[pgu$ranked],responderid=pgu$responderid, rank=1:NROW(ans), numchoices=NROW(ans),guesstime=Sys.time())

    dbInsert(db,"pqguess",idf)

  })

  setUI(container.id,ui)
  dsetUI(container.id,ui)
  pgu
}

pq.guess.headers = function() {
  www.path = system.file("www",package="peerquiz")
  tagList(
    singleton(tags$head(includeScript(file.path(www.path,"clickrank.js")))),
    singleton(tags$head(includeCSS(file.path(www.path,"clickrank.css"))))
  )

}

pgu.ui = function(ans=pgu$ans,pq, pgu=get.pgu(pq), num.cols=2, add.header = TRUE) {
  restore.point("pgu.ui")
  ns = pq$ns
  pgu$ans = ans

  divs = lapply(seq_len(NROW(ans)), quiz.ans.div, pq=pq,pgu=pgu)
  is.left = seq_along(divs)%%2 == 1
  left = divs[is.left]
  right = divs[!is.left]
  if (length(right)<length(left)) right[[length(left)]] = ""

  str = paste0('<tr><td valign="top" style="border: 0px solid #000000">',left,'</td><td valign="top" style="border: 0px solid #000000">',right,"</td></tr>")
  tab = paste0('<table  style="width: 100%; border-collapse:collapse;"><col width="50%"><col width="50%">', paste0(str, collapse="\n"),"</table>")


  ui = withMathJax(tagList(
    if (add.header) pq.guess.headers(),
    HTML(pq$question_html),
    h4(pq$str$proposed_answers),
    HTML(tab),
    uiOutput(ns("ranking")),
    actionButton(ns("submitGuessBtn"),pq$str$submitBtn)
  ))
  ui
}

quiz.ans.div = function(ans.num=1, pq, pgu=get.pgu(pq)) {
  restore.point("quiz.ans.div")

  ans = pgu$ans[ans.num,]
  id = pgu$ans.div.id[[ans.num]]

  ui = div(id = id,style="margin:5px; border: 1px solid #000000; padding:10px;", class="clickable",
    tags$h4(pq$str$Answer, ans.num),
    ans$answer.ui[[1]]
  )

  as.character(ui)
}
