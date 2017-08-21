pq.schema = function(app=getApp()) {
  if (!is.null(app$glob[["pqschema"]]))
    return(app$glob[["pqschema"]])

  schema.file = system.file("schema/pqdb.yaml", package="peerquiz")
  app$glob[["pqschema"]] = rmdtools::read.yaml(schema.file)
}

get.pqdb = function(pq.dir = get.pq.dir(), db=app$glob[["pqdb"]], app = getApp(), db.dir = file.path(pq.dir,"db")) {
  if (!is.null(db)) {
    if (dbIsValid(dh))
      return(db)
  }


  db.file = file.path(db.dir,"pq.sqlite")
  if (!file.exists(db.file)) {
    db = create.pqdb(pq.dir=pq.dir)
  } else {
    db = dbConnect(SQLite(),dbname = file.path(db.dir,"pq.sqlite"))
  }
  if (!is.null(app$glob))
    app$glob$pqdb = db
  db
}

create.pqdb = function(pq.dir=get.pq.dir(), schema.file = NULL,db.dir = file.path(pq.dir,"db")) {
  restore.point("create.pqdb")


  if (!dir.exists(db.dir))
    dir.create(db.dir,recursive = TRUE)

  db = dbConnect(SQLite(),dbname = file.path(db.dir,"pq.sqlite"))
  schema = pq.schema()
  dbmisc::dbCreateSchemaTables(db, schemas = schema)
  db
}
