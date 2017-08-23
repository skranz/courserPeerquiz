pq_string = function(lang = first.non.null(pq[["lang"]], apq[["lang"]], "en"), pq=NULL, apq=NULL) {
  if (lang == "de"){
    mark_utf8(list(points = "Punkte", sample_sol = "Musterlösung", Answer = "Antwort",answers="Antworten", your_ranking="Ihr Ranking der Antworten (Ziel: Musterlösung möglichst weit oben)",proposed_answers="Vorgeschlagene Antworten:",not_yet_ranked="Noch keine Antwort auf Platz 1 gerankt.",not_all_ranked="Sie haben noch nicht alle Antworten gerankt.", guess_save_msg="Ihr Ranking wurde abgesendet. Sie sehen jetzt die Musterlösung.", submitBtn="Ranking absenden", explain_label="Erklärung:",write_save_msg="Ihre Antwort wurde gespeichert.",datetime_format = "%a %d.%m. %H:%M",state_desc=c(write="Schreibe Antwort bis ", guess="Rate Antwort bis ", before="Noch nicht offen",after="Beendet") ))
  } else {
    list(points="points", sample_sol = "Sample Solution", Answer = "Answer",answers="answers", your_ranking="Your ranking of the answers (Goal: Rank sample solution high)", proposed_answers="Proposed Answers",not_yet_ranked="No answer yet ranked on position 1.",not_all_ranked="You have not yet ranked all answers.", submitBtn="Submit your ranking", explain_label="Explanation:",guess_save_msg="Your ranking has been saved.",write_save_msg="Your answer has been saved.",datetime_format = "%a %b %d %H:%M",state_desc=c(write="Write Answer", guess="Guess Answer", before="Not yet open",after="Finished"))

  }
}
