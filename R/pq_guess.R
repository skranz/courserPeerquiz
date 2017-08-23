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


# state of pgu for user: "no", "assigned", "submitted"
get.user.pgu.state = function(pq, userid, task.dir=pq.task.dir(pq)) {
  file.name = digest(userid)
  if (file.exists(file.path(task.dir,"pgu_submitted", file.name))) return("submitted")
  if (file.exists(file.path(task.dir,"pgu_assigned", file.name))) return("assigned")
  return("no")
}

new.pgu = function(pq,responderid, ans= if(!is.null(adf)) select.guess.choices(adf = adf, responderid=responderid), num.ans = NROW(ans), adf = NULL, state="assigned", ...) {
  pgu = as.environment(list(id=pq$id,state=state,responderid=responderid, ans=ans, num.ans=num.ans, ans.div.id = paste0("ans-div-id-",seq_len(NROW(ans)),"-",pq$id)))
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

set.pgu.ui = function(container.id,pq, pgu = NULL, edit = !isTRUE(pgu$state=="submitted"),  show.sol=!edit) {
  restore.point("set.pgu.ui")

  ns = pq$ns
  ans = pgu$ans
  ui = pgu.ui(pq=pq,pgu = pgu, edit=edit)

  if (edit) {
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
      pgu.show.ans.ranking(pgu, pq)
    })

    callJS("newClickRank",id=pq$id,div_ids=pgu$ans.div.id,max_ranked=3)

    buttonHandler(ns("submitGuessBtn"), function(...) {
      pgu.submit(pq=pq, pgu=pgu)
    })

  } else {
    # disable click event handler
    eventHandler("clickRankChange",id=pq$id,function(...) {})
    pgu.show.ans.ranking(pgu, pq)
    ui = tagList(ui,
      tags$script(HTML(pgu.show.sol(pgu,pq, return.js=TRUE)))
    )
  }

  setUI(container.id,ui)
  dsetUI(container.id,ui)

  pgu
}

get.pgu.points = function(pgu, pq) {
  if (length(pgu$ranked)==0) return(NULL)
  sol.rank = which(pgu$ans$is.sol[pgu$ranked])
  c(4,2,1,0)[sol.rank]
}

pgu.show.sol = function(pgu, pq, return.js = FALSE) {
  restore.point("pgu.show.sol")
  sol.ind = which(pgu$ans$is.sol)

  if (length(sol.ind)==0) return()
  id = pgu$ans.div.id[sol.ind]
  if (return.js)
    return(paste0('$("#',id,'").css({border:"4px solid #0000aa"});'))

  setHtmlCSS(id=id, list(border="4px solid blue;"))
}

pgu.show.ans.ranking = function(pgu, pq, show.sol=isTRUE(pgu$state=="submitted")) {
  restore.point("pgu.show.ans.ranking")
  ranked = pgu$ranked
  ns = pq$ns

  cat("\nRanking:",paste0(ranked, collapse=", "))
  if (length(ranked)==0) {
    str = pq_string(pq$lang)$not_yet_ranked
  } else {
      str = paste0(seq_along(ranked), ": ",pq_string(pq$lang)$Answer," ", ranked)
    if (show.sol) {
      rows = which(pgu$ans$is.sol[pgu$ranked])
      points = get.pgu.points(pgu=pgu,pq=pq)
      str[rows] = paste0('<font color="#0000aa">', str[rows],' (',pq_string(pq$lang)$sample_sol,', ', points, ' ',pq_string(pq$lang)$points,')</font>')
    }
    str = paste0(str, collapse="<br>")
  }

  ranking.ui = tagList(
    h4(pq_string(pq$lang)$your_ranking,":"),
    p(HTML(str))
  )
  setUI(ns("ranking"), ranking.ui)

}

pgu.submit = function(pgu, pq,show.sol=TRUE,...) {
  restore.point("pgu.submit")
  ans = pgu$ans; ns = pq$ns;

  if (length(pgu$ranked) < pgu$num.ans) {
    timedMessage(pq$ns("pguAlert"), html=colored.html(pq_string(pq$lang)$not_all_ranked, color="#880000"))
    return()
  }
  pgu$state = "submitted"
  pgu$ranked

  db = get.pqdb()

  idf = data_frame(id=pq$id,writerid = ans$userid[pgu$ranked],responderid=pgu$responderid, rank=1:NROW(ans), numchoices=NROW(ans),guesstime=Sys.time())

  dbInsert(db,"pqguess",idf)

  dir = file.path(pq.task.dir(pq=pq),"pgu_submitted")
  if (!dir.exists(dir))
    dir.create(dir, recursive = TRUE)

  file.name = digest(pgu$responderid)
  saveRDS(pgu, file.path(dir , file.name))

  timedMessage(pq$ns("pguAlert"), html=colored.html(pq_string(pq$lang)$guess_save_msg, color="#880000"),millis = Inf)

  if (show.sol) {
    shinyEvents::setHtmlHide(pq$ns("submitGuessBtn"))
    pgu.show.ans.ranking(pgu, pq)
    pgu.show.sol(pgu,pq)
  }


}

pq.guess.headers = function() {
  www.path = system.file("www",package="peerquiz")
  tagList(
    singleton(tags$head(includeScript(file.path(www.path,"clickrank.js")))),
    singleton(tags$head(includeCSS(file.path(www.path,"clickrank.css"))))
  )

}



pgu.ui = function(ans=pgu$ans,pq, pgu=get.pgu(pq), num.cols=2, add.header = TRUE, edit=TRUE) {
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
    h4(pq_string(pq$lang)$proposed_answers),
    HTML(tab),
    uiOutput(ns("ranking")),
    uiOutput(ns("pguAlert")),
    if (edit)
      actionButton(ns("submitGuessBtn"),pq_string(pq$lang)$submitBtn)
  ))
  ui
}

quiz.ans.div = function(ans.num=1, pq, pgu=get.pgu(pq)) {
  restore.point("quiz.ans.div")

  ans = pgu$ans[ans.num,]
  id = pgu$ans.div.id[[ans.num]]

  ui = div(id = id,style="margin:5px; border: 1px solid #000000; padding:10px;", class="clickable",
    tags$h4(pq_string(pq$lang)$Answer, ans.num),
    ans$answer.ui[[1]]
  )

  as.character(ui)
}

