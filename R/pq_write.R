example.peerquiz = function() {
  setwd("D:/libraries/courserPeerquiz")
  file = "p-value.yaml"
  file = "gutschein.yaml"
  file = "Kap1_Software_3.Rmd"
  file = "Kap2_3_Happiness.Rmd"
  app = eventsApp()
  pq = create.pq(file=file)
  app$userid = paste0("Guest_", sample.int(100000,1))
  save.pq.sample.sol(pq=pq)
  ui = peerquiz.write.ui(pq)

  app$ui = fluidPage(
    withMathJax(ui)
  )
  viewApp(app)

  #view.html(ui=ui)
}



peerquiz.write.ui = function(pq) {
  restore.point("peerquiz.write.ui")

  ns = pq$ns

  ui = tagList(
    p(HTML(pq$question_html)),
    tabsetPanel(type="pills",id = ns("tabset"),
      tabPanel(title = "Edit", value = "edit",
        pq[["form.ui"]],
        pqTextAreaInput(ns("ace"),value = pq$template,label="",width="100%",rows=pq$ace_lines, resize="both")
        #aceEditor(outputId = ns("ace"),value = pq$template,mode = "text",showLineNumbers = FALSE,wordWrap = TRUE,height = 12*pq$ace_lines+20)
      ),
      tabPanel(title = "Preview", value = "preview",
        uiOutput(ns("previewUI"))
      )
    ),
    uiOutput(ns("msg")),
    actionButton(ns("saveBtn"),"Save")
  )

  buttonHandler(ns("saveBtn"), function(...) {
    ns = pq$ns
    res = get.preview.values(pq=pq)
    if (!res$ok) {
      updateTabsetPanel(app$session,ns("tabset"),"edit")
      return()
    }
    save.pq.answer(pq, answer=res$answer, values=res$values, answer.ui = res$answer.ui)
    timedMessage(ns("msg"),"Your answer has been saved.")
  })

  changeHandler(ns("tabset"),pq=pq, function(value, app,pq, ...) {
    ns = pq$ns
    if (value=="preview") {
      res = get.preview.values(pq=pq)
      if (!res$ok) {
        updateTabsetPanel(app$session,ns("tabset"),"edit")
        return()
      }
      setUI(ns("previewUI"), wellPanel(res$answer.ui))
    }
  })
  ui
}

get.preview.values = function(pq) {
  restore.point("get.preview.values")
  ns = pq$ns
  answer = getInputValue(ns("ace"))
  if (pq$has.form) {
    res = get.form.values(pq$form, show.alerts=TRUE)
    if (!res$ok) {
      return(list(ok=FALSE))
    }
    values = res$values
  } else {
    values = NULL
  }

  restore.point("shhduihsiudhf")
  answer.ui = withMathJax(HTML(answer.source.to.secure.html(answer)))
  if (!is.null(pq$render.answer.fun)) {
    answer.ui = pq$render.answer.fun(pq=pq, values=values, answer=answer, answer.ui=answer.ui)
  }

  return(list(ok=TRUE, values=values, answer=answer, answer.ui=answer.ui))
}

pqTextAreaInput = function (inputId, label, value = "", width = NULL, height = NULL,
    cols = NULL, rows = NULL, placeholder = NULL, resize = NULL, style=NULL,spellcheck="true",...)
{
    value <- restoreInput(id = inputId, default = value)
    if (!is.null(resize)) {
        resize <- match.arg(resize, c("both", "none", "vertical",
            "horizontal"))
    }
    restore.point("pqTextAreaInput")
    nstyle <- paste(if (!is.null(width))
        paste0("width: ", validateCssUnit(width), ";"), if (!is.null(height))
        paste0("height: ", validateCssUnit(height), ";"), if (!is.null(resize))
        paste0("resize: ", resize, ";"))
    if (length(style)>0) {
      style = paste0(style,"; ")
    }
    style = paste0(style,nstyle)
    if (length(style) == 0)
        style <- NULL

    div(class = "form-group shiny-input-container", tags$textarea(id = inputId,
        class = "form-control", placeholder = placeholder, style = style,
        rows = rows, cols = cols,spellcheck=spellcheck, value,...))
}

save.pq.sample.sol = function(pq, userid = "SOLUTION",  is.sol=TRUE, file="sample_solution.pqa") {
  restore.point("save.pq.sample.sol")

  values = lapply(pq$fields, function(field) {
    field$sol
  })
  answer = pq$solution
  answer.ui = withMathJax(HTML(answer.source.to.secure.html(answer)))
  if (!is.null(pq$render.answer.fun)) {
    answer.ui = pq$render.answer.fun(pq=pq, values=values, answer=answer, answer.ui=answer.ui)
  }
  save.pq.answer(pq=pq, answer=answer, values=values, answer.ui = answer.ui,userid=userid, is.sol=TRUE, file=file)

}

save.pq.answer = function(pq, userid=get.pq.userid(), values, answer, answer.ui, pq.dir=get.pq.dir(pq), is.sol=FALSE, file=paste0(digest(userid),".pqa")) {
  restore.point("save.pq.answer")

  pqa = list(id=pq$id, userid=userid, values=list(values), answer=answer, answer.ui=list(answer.ui), is.sol=is.sol, time=Sys.time())

  answers.dir = file.path(pq.dir,"answers")
  if (!dir.exists(answers.dir))
    dir.create(answers.dir, recursive = TRUE)

  saveRDS(pqa, file.path(answers.dir, file))
}


answer.source.to.secure.html = function(answer) {
  restore.point("answer.source.to.secure.html")
  # Properly escape to prevent
  # an XSS attack
  html = htmltools::htmlEscape(answer)
  # Still we want to preserve line breaks
  html = gsub("\n","<br>\n", html, fixed=TRUE)
  html
}
