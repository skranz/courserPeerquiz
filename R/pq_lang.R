pq_strings = function(lang = "en") {
  if (lang == "de"){
    mark_utf8(list(Answer = "Antwort",answers="Antworten", your_ranking="Ihr Ranking",proposed_answers="Vorgeschlagene Antworten:",not_yet_ranked="Noch keine Antwort auf Platz 1 gerankt.", submitBtn="Ranking absenden", explain_label="Erklärung:",write_save_msg="Ihre Antwort wurde gespeichert." ))
  } else {
    list(Answer = "Answer",answers="answers", your_ranking="Your Ranking", proposed_answers="Proposed Answers",not_yet_ranked="No answer yet ranked on position 1.",submitBtn="Submit your ranking", explain_label="Explanation:",write_save_msg="Your answer has been saved.")

  }
}

apq_strings = function(lang="en") {
  if (lang == "de"){
    mark_utf8(list(datetime_format = "%a %d.%m. %H:%M",state_desc=c(write="Schreibe Antwort bis ", guess="Rate Antwort bis ", before="Noch nicht offen",after="Beendet")))
  } else {
    mark_utf8(list(datetime_format = "%a %b %d %H:%M",state_desc=c(write="Write Answer", guess="Guess Answer", before="Not yet open",after="Finished")))
  }

}
