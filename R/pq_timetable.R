examples.pq.timetable.ui = function() {
  setwd("D:/libraries/courserPeerquiz/peerquiz")
  set.pq.opts()

  tt = pq.load.time.table()

  app = eventsApp()
  app$ui = fluidPage(
    pq.timetable.ui()
  )
  viewApp(app)
}

pq.load.time.table = function(pq.dir = get.pq.dir(), adapt.to.sources=TRUE) {
  restore.point("pq.load.time.table")
  file = file.path(pq.dir,"timetable","timetable.json")
  if (!file.exists(file)) {
    tt = NULL
  } else {
    tt = jsonlite::read_json(file)
    tt = bind_rows(tt)
    tt$active = as.logical(tt$active)
  }
  if (adapt.to.sources)
    tt = pq.adapt.time.table.to.sources(tt=tt, pq.dir=pq.dir)
  tt
}

pq.adapt.time.table.to.sources = function(tt=NULL, pq.dir = get.pq.dir()) {
  restore.point("pq.adapt.time.table.to.sources")
  sources.dir = file.path(pq.dir,"sources")
  sources = tools::file_path_sans_ext(list.files(sources.dir))

  new.sources = setdiff(sources, tt$id)

  if (length(new.sources)==0) return(tt)

  tt.new = data_frame(id=new.sources, start_write = "", start_guess = "", end_guess = "")
  if (is.null(tt)) {
    return(tt.new)
  }
  tt = rbind(tt, new.tt)
  tt = tt[match(tt$id, sources),]
  tt
}

pq.timetable.ui = function(tt=pq.load.time.table(pq.dir=pq.dir), pq.dir=get.pq.dir()) {
  restore.point("pq.timetable.ui")

  toDateChar = function(x) {
    restore.point("hdhfuihfuihf")
    if (nchar(x)==0) return("")
    as.Date(x)
  }
  toTimeChar = function(x) {
    if (nchar(x)==0) return("")
    str.right.of(x," ")
  }

  ui.li = lapply(seq_len(NROW(tt)), function(row) {
    ttr = as.list(tt[row,])
    id = ttr$id

    ns = function(x) paste0(id,"-",x,"-pqtt")

    tagList(
      tags$h4(id),
      checkboxInput(ns("active"),"Active", value=ttr$active),
      tags$table(tags$tr(
        tags$td(dateInput(width="10em",ns("start_write_date"), "Start Date",value = toDateChar(ttr$start_write))),
        tags$td(textInput(width = "5em", ns("start_write_time"), "Time",value = toTimeChar(ttr$start_write))))
      ),
      tags$table(tags$tr(
        tags$td(dateInput(width="10em",ns("start_guess_date"), "Guess Date",value = toDateChar(ttr$start_guess))),
        tags$td(textInput(width = "5em", ns("start_guess_time"), "Time",value = toTimeChar(ttr$start_guess))))
      ),
      tags$table(tags$tr(
        tags$td(dateInput(width="10em",ns("end_guess_date"), "Stop Date",value = toDateChar(ttr$end_guess))),
        tags$td(textInput(width = "5em", ns("end_guess_time"), "Time",value = toTimeChar(ttr$end_guess))))
      ),
      hr()
    )
  })
  idm = expand.grid(tt$id,c("active","start_write_date","start_write_time","start_guess_date","start_guess_time","end_guess_date","end_guess_time"))
  form.ids = paste0(idm[,1],"-",idm[,2],"-pqtt")

  ui = tagList(
    ui.li,
    uiOutput("pqttAlert"),
    defaultButton("pqttSubmitBtn","Save", form.ids=form.ids)
  )
  setUI("pqttAlert",HTML(""))

  buttonHandler("pqttSubmitBtn",pq.time.table.save.click, pq.dir=pq.dir)
  ui
}

pq.time.table.save.click = function(formValues,..., pq.dir=get.pq.dir()) {
  restore.point("pq.time.table.save.click")
  fields = names(formValues)
  df = fast_df(
    id = str.left.of(fields,"-"),
    field = str.between(fields,"-","-"),
    value = unlist(formValues)
  ) %>% spread(field, value)

  res = try({
    df$start_write = combine.time.and.date.string(df$start_write_date,df$start_write_time)
    df$start_guess = combine.time.and.date.string(df$start_guess_date,df$start_guess_time)
    df$end_guess = combine.time.and.date.string(df$end_guess_date,df$end_guess_time, default.time = "23:59")
  })

  if (is(res,"try-error")) {
    timedMessage("pqttAlert",html=colored.html("Error cannot correctly parse data times for all your fields.",color="#880000"),millis = 10000)

  }

  json = toJSON(select(df,id,active,start_write, start_guess, end_guess))
  dir = file.path(pq.dir,"timetable")
  if (!dir.exists(dir))
    dir.create(dir, recursive = TRUE)
  writeLines(json, file.path(dir,"timetable.json"))

  timedMessage("pqttAlert","Your changes have been saved.",millis = 3000)


}

combine.time.and.date.string = function(date, time, default.time = "00:00", check=TRUE) {
  restore.point("combine.time.and.date.string")
  date = str.trim(date); time = str.trim(time)
  str =
    ifelse(nchar(date)==0,"",
    ifelse(nchar(time)==0,paste0(date," ", default.time),
    paste0(date," ",time)
  ))
  if (check) {
    rows = nchar(str)>0
    as.POSIXct(str[rows])
  }
  str
}

defaultButton = function(id, label,class.add="",class="btn btn-default action-button btn-default",style="",form.ids=NULL,form.sel=NULL,...) {
  args = list(...)
  if ("data-form-selector" %in% names(args) | (is.null(form.ids) & is.null(form.sel))) {
    tags$button(id=id, style=style, type="button", class=paste(class,class.add),...,label)
  } else {
    if (is.null(form.sel)) {
      form.sel = paste0("#", form.ids,collapse=", ")
    }
    tags$button(id=id, style=style, type="button", class=paste(class,class.add),`data-form-selector`=form.sel,...,label)

  }

}
