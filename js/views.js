/*global 
 jquery:true,
 $:true,
 us: true,
 Backbone: true,
 _: true,
*/

window.CardView = Backbone.View.extend({
  tagName: 'div',
  className: "card",
  events: {
    'click': 'toggleKind'
  },

  initialize: function () {
    _.bindAll(this)
    this.$el.draggable()
    
    this.model.on('destroy', this.destroyView)
    this.model.on('change', this.render)
    
    this.render() // not all views are self-rendering. This one is.
  },
  
  destroyView: function () {
    this.off()
    this.model.off(null, null, this) // unbind
    this.remove() // remove from the dom
  },

  render: function () {
    return this.$el.html(this.model.get('kind') + this.model.get('uid'))
  },
  
  // Controller Logic
  toggleKind: function() {
    this.model.set({
      kind: 'test'
    })
  }
})

window.CardListView = Backbone.View.extend({
  el: $('#cards'),
  
  events: {
    // 'click button#add': 'callback'
  },

  initialize: function() {
    _.bindAll(this, 'render')
    
    this.cards = new CardList()
    
    this.render(); // not all views are self-rendering. This one is.
  },

  addOne: function (card) {
    this.cards.add(card) // add to the collection
    var view = new CardView({ model: card }) // make a new cardview and render it
    this.$el.append(view.el)
  },

  render: function() {
    $(this.el).append("<ul> <li>hello world</li> </ul>");
  }
})


window.AppView = Backbone.View.extend({
  el: $('#playing-field'),

  cardListView: new CardListView(),

  events: {
    // 'click button#add': 'callback'
  },

  initialize: function () {
    _.bindAll(this, 'render');

    this.render() // not all views are self-rendering. This one is.
  },
  
  addCard: function (card) {
    this.cardListView.addOne(card)
  },

  render: function () {
    return this.$el
  }
})