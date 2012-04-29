/*global
 jQuery:true,
 $:true,
 us: true,
 _: true,
 Backbone: true,
 document: true
*/

$(document).ready(function () {

  window.app = new AppView({el: $('#playing-field'), players: 1 })

  app.dealCard(new CardView({
    model: (new Card({kind: "Push"})) 
  }), 0, 0)
  app.dealCard(new CardView({
    model: (new Card({kind: "Hit"})) 
  }), 0, 0)

  $("body").append($('<a />', {
    "href": '#',
    "class": '',
    "text": 'Clear Riders'
  }).click(function (event) {
    for (var numPlayers = app.playerCount(), curr = 0; curr < numPlayers; curr++) {
      app.clearRider(curr)
    }
  }))
})












