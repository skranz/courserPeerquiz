
init.pq.form = function(pq, lang=first.none.null(pq$lang,"en"),explain_label = first.none.null(pq$explain_label, pq$str$explain_label,"")) {
  restore.point("init.pq.form")

  # We cannot combine sc with other fields
  if (!is.null(pq[["sc"]])) {
    answer.ind = which(str.ends.with(pq$sc,"*"))

    choices = pq$sc
    choices[answer.ind] = str.remove.ends(choices[answer.ind],right=1)
    pq$fields = list(sc=list(
      choices = choices, type="radio", value=NA, sol=if (length(answer.ind)>0) choices[[answer.ind[1]]]
    ))

    if (is.null(pq$render.answer.fun)) {
      pq$render.answer.fun = function(pqi,values=NULL, answer=NULL, answer.ui=NULL,...) {
        restore.point("pq.render.answer.fun.sc")
        tagList(
          HTML(values[[1]]),
          h4(explain_label),
          answer.ui
        )
      }
    }
  }


  pq$has.form = !is.null(pq[["fields"]])
  if (!pq$has.form) {
    pq$form = pq$form.ui = NULL
    return(pq)
  }

  pq$form.prefix = paste0(pq$id,"-")


  if (is.null(pq[["inputform"]])) {
    pq$inputform = pq.default.input.form(pq=pq,lang=lang)
  }

  form = pq["fields"]
  form$widget.as.character=form$form.control.class=FALSE
  form$prefix =pq$form.prefix
  set.form(form)
  rmd = pq[["inputform"]]
  cr = compile.rmd(text = rmd,out.type = "shiny")
  ui = render.compiled.rmd(cr,out.type = "shiny")
  form.ui = ui

  pq$form = form
  pq$form.ui = form.ui
  pq
}

pq.default.input.form = function(pq, lang=first.none.null(pq$lang,"en"), explain_label = first.none.null(pq$explain_label, pq$str$explain_label,"")) {
  restore.point("pq.default.input.form")

  txt = paste0('{{ fieldInput("',names(pq$fields),'")}}<br>', collapse="\n")
  txt = paste0(txt,"\n", explain_label)
  txt
}

pq.default.render.answer.fun = function(pqi, values, answer=NULL, answer.ui=NULL,...) {
  restore.point("pq.default.render.answer.fun")
  if (is.null(pq$form)) return(answer.ui)

  labels = lapply(pq$form$fields[names(values)], function(field) {
    if (!is.null(field[["label"]])) {
      paste0(field$label,": ")
    } else {
      ""
    }
  })

  values = lapply(values, function(val) {
    if (is.numeric(val)) return(format(val, scientific=FALSE))
    val
  })

  field.text = paste0(labels, values,"\n",collapse="")
  explain_label = first.none.null(pqi$explain_label, pqi$str$explain_label,"")

  tagList(
    HTML(field.text),
    h4(explain_label),
    answer.ui
  )

}
