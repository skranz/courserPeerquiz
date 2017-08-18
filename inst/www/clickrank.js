newClickRank = function(id, div_ids, max_ranked) {
  cr = {
    id: id,
    n_divs: div_ids.length,
    div_ids: div_ids,
    max_ranked: max_ranked,
    ranked: []
  };
  sel = "#" + div_ids.join(", #");
  $(document).on("click",sel, function(e) {
    var div_id = e.currentTarget.id;
    var ind = cr.div_ids.indexOf(div_id);
    var rank_pos = cr.ranked.indexOf(ind);


    rem_class = "click-rank-1 click-rank-2 click-rank-3 click-rank-4 click-rank-5";

    // reset ranking equal or above rank_pos
    if (rank_pos >-1) {
      for (var rp=rank_pos; rp < cr.ranked.length; rp++) {
        var di = cr.ranked[rp];
        $("#"+cr.div_ids[di]).removeClass(rem_class);
      }
      if (rank_pos==0) {
        cr.ranked = [];
      } else {
        cr.ranked = cr.ranked.slice(0,rank_pos);
      }
    } else if (cr.ranked.length < cr.max_ranked) {
      $("#"+div_id).removeClass(rem_class);
      cr.ranked.push(ind);
      $("#"+div_id).addClass("click-rank-"+cr.ranked.length);
    } else {
      return;
    }


    // send shiny event
    Shiny.onInputChange("clickRankChange", {eventId: "clickRankChange", id: cr.id, div_id: div_id, div_ind: ind,ranked: cr.ranked, num_ranked: cr.ranked.length, max_ranked: cr.max_ranked,  nonce: Math.random()});


  });

  return cr;
};
