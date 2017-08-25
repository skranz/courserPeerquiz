examples.pq.sim = function() {
  setwd("D:/libraries/courser/courses/vwl/course/peerquiz")
  set.pq.opts()
  pq = load.pq("Kap1_Software_1")
  pq = load.pq("Kap1_Software_2")
  pq = load.or.compile.pq("Kap1_Software_3")
  pq_create_random_answers(pq=pq,n=20)
  pq_create_random_guesses(pq=pq,n=20)
}

pq_create_random_answers = function(pq=load.pq(id), n=100, id=NULL) {
  values = lapply(pq$fields, function(field) {
    field$sol
  })

  cat("\n")
  for (i in 1:n) {
    cat(".")
    answer = paste0("Random answer ", i)
    answer.ui = withMathJax(HTML(answer.source.to.secure.html(answer)))
    if (!is.null(pq$render.answer.fun)) {
      answer.ui = pq$render.answer.fun(pq=pq, values=values, answer=answer, answer.ui=answer.ui)
    }
    save.pq.answer(pq=pq, answer=answer, values=values, answer.ui = answer.ui,userid=paste0("random_",i), is.sol=FALSE, file=paste0("000_random_",i,".pqa"))
  }

}

pq_create_random_guesses = function(pq=load.pq(id), n=100, id=NULL) {
  restore.point("pq_create_random_guesses")
  userids=paste0("random_",1:n)

  adf = pq.get.answers.df(pq)

  pq.remove.random.users.from.db(id=pq$id,del.writer=FALSE, del.guesser = TRUE)
  lapply(userids, function(userid) {
    restore.point("pq_create_random_guess")
    pgu = new.pgu(pq=pq, responderid = userid,adf = adf)
    pgu$ranked = sample(1:4,4)
    pgu.submit(pgu=pgu,pq = pq,file.name = paste0("000_",userid),  show.msg=FALSE, show.sol=FALSE)
  })

}

pq.remove.random.users.from.db = function(id=NULL,del.writer=TRUE, del.guesser=TRUE, db=get.pqdb()) {
  restore.point("pq.remove.random.users.from.db")
  sql = "DELETE FROM pqguess WHERE "
  where = ""
  if (!is.null(id)) {
    where = paste0(' id = "',id,'" AND ')
  }
  if (del.writer) {
    asql = paste0(sql,where,' writerid LIKE "random%"')
    dbDelete(db,sql=asql,params=NULL, table="pqguess")
  }
  if (del.guesser) {
    asql = paste0(sql,where,' responderid LIKE "random%"')
    dbDelete(db,sql=asql,params=NULL, table="pqguess")
  }



}
