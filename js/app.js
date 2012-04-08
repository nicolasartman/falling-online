/*global
 jQuery:true,
 $:true,
 us: true,
 _: true,
 Backbone: true,
 document: true
*/

$(document).ready(function () {
  var Card = Backbone.Model.extend({
    defaults: {
      kind: "None",
      playable: false
    }
  })

  var CardList = Backbone.Collection.extend({
    model: Card
  })

  var CardView = Backbone.View.extend({

    tagName: 'div',

    className: "card",

    events: {
      // 'click button#add': 'callback'
    },

    initialize: function () {
      _.bindAll(this, 'render');
      // this.$el.draggable()
      this.render(); // not all views are self-rendering. This one is.
    },

    render: function () {
      this.$el.append("CARD");
    }
  })

  var cards = new CardList()

  var AppView = Backbone.View.extend({
    // attaches 'this.el' to an existing element.
    el: $('#playing-field'),

    cards: cards,

    events: {
      // 'click button#add': 'callback'
    },

    initialize: function () {
      _.bindAll(this, 'render');

      this.cards.bind('add', this.addOne, this)
      this.render() // not all views are self-rendering. This one is.
    },

    addOne: function (card) {
      var view = new CardView({ model: card })
      this.$('#cards').append(view.render().el)
    },

    render: function () {
      this.$el.append("")
    }
  })

  cards.add(new Card({ kind: "Hit" }))

  var app = new AppView()

})












