/*global
 jQuery:true,
 $:true,
 us: true,
 _: true,
 Backbone: true,
 document: true
*/

$(document).ready(function () {

  window.app = new AppView({el: $('#playing-field') })

  app.addRiders(new Rider())
  app.addCard(new Card({ kind: "Hit"  }))
  app.addCard(new Card({ kind: "Push" }))

})












