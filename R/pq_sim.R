examples.pq.sim = function() {
  setwd("D:/libraries/peerquiz")
  pq = load.pq("p-value")
  pq_create_random_answers(pq=pq,n=20)
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
