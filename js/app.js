/*global
 jQuery:true,
 $:true,
 us: true,
 _: true,
 Backbone: true,
 document: true
*/

$(document).ready(function () {
  var app = new AppView()

  app.addCard(new Card({ kind: "Hit" }))
  app.addCard(new Card({ kind: "Push"}))

})












