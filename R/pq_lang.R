pq_strings = function(lang = "en") {
  if (lang == "de"){
    mark_utf8(list(Answer = "Antwort",answers="Antworten", your_ranking="Ihr Ranking",proposed_answers="Vorgeschlagene Antworten:",not_yet_ranked="Noch keine Antwort auf Platz 1 gerankt.", task="Aufgabe:", submitBtn="Ranking absenden", explain_label="Erklärung:" ))
  } else {
    list(Answer = "Answer",answers="answers", your_ranking="Your Ranking", proposed_answers="Proposed Answers",not_yet_ranked="No answer yet ranked on position 1.", task="Task:",submitBtn="Submit your ranking", explain_label="Explanation:")

  }
}
