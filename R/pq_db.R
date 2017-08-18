


pq.schema = function(app=getApp()) {
  if (!is.null(app$glob[["pqschema"]]))
    return(app$glob[["pqschema"]])

  schema.file = system.file("schema/pqdb.yaml", package="peerquiz")
  app$glob[["pqschema"]] = rmdtools::read.yaml(schema.file)
}

get.pqdb = function(pqs.dir = get.pqs.dir(), db=app$glob[["pqdb"]], app = getApp()) {
  if (!is.null(db)) return(db)

  db.dir = pqs.dir
  db.file = file.path(db.dir,"pq.sqlite")
  if (!file.exists(db.file)) {
    db = create.pqdb(pqs.dir=pqs.dir)
  } else {
    db = dbConnect(SQLite(),dbname = file.path(db.dir,"pq.sqlite"))
  }
  if (!is.null(app$glob))
    app$glob$pqdb = db
  db
}

create.pqdb = function(pqs.dir=get.pqs.dir(), schema.file = NULL) {
  restore.point("create.pqdb")

  db.dir = pqs.dir
  if (!dir.exists(db.dir))
    dir.create(db.dir)

  db = dbConnect(SQLite(),dbname = file.path(db.dir,"pq.sqlite"))
  schema = pq.schema()
  dbmisc::dbCreateSchemaTables(db, schemas = schema)
  db
}
