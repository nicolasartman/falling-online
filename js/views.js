/*global 
 jquery:true,
 $:true,
 us: true,
 window: true,
 Backbone: true,
 _: true,
 CardList: true,
 CardView: true,
 RiderView: true,
 RiderList: true
*/

// ====================
// = Basic Data Views =
// ====================

window.CardView = Backbone.View.extend({
  tagName: 'div',
  className: "card",
  events: {
    'click': 'toggleKind',
    'dragstart': 'startDrag',
  },

  initialize: function () {
    _.bindAll(this)
    this.$el.draggable({ zIndex: 9001 })

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
  toggleKind: function () {
    this.model.set({
      kind: 'test'
    })
  },
  
  startDrag: function(event) {
    this.inHand = true
    console.log("success")
  },
  
})


window.RiderView = Backbone.View.extend({
  // attaches 'this.el' to an existing element.
  tagName: 'div',
  className: 'rider',
  
  events: {
    // 'click button#add': 'callback'
  },
  
  initialize: function(){
    _.bindAll(this, 'render')
    this.render()
  },
  
  render: function(){
    return this.$el
  }
})

// ====================
// = Collection Views =
// ====================

window.CardListView = Backbone.View.extend({

  tagName:'div',
  id: 'cards',

  events: {
    // 'click button#add': 'callback'
  },

  initialize: function() {
    _.bindAll(this)

    this.cards = new CardList()
    this.cards.on('add', this.addOne)

    this.render(); // not all views are self-rendering. This one is.
  },

  addOne: function (card) {
    var view = new CardView({ model: card })
    this.$el.append(view.el)
  },
  
  addCard: function(card) {
    this.cards.add(card)
  },
  

  render: function() {
    return this
  }
})

window.RiderListView = Backbone.View.extend({
  
  tagName: 'div',
  id: 'riders',
  
  events: {
    // 'click button#add': 'callback'
  },
  
  initialize: function(){
    _.bindAll(this)
    
    this.riders = new RiderList()
    this.riders.on('add', this.render)

    this.render()
  },

  addRiders: function(riders) {
    this.riders.add(riders)
  },
  
  render: function() {
    var self = this
    console.log(this.riders)
    this.$el.html("")
    _.each(this.riders.models, function (rider) {
      console.log(rider)
      self.$el.append(rider.el)
    })
    return this
  }
})

window.AppView = Backbone.View.extend({

  events: {
    // 'click button#add': 'callback'
  },

  initialize: function () {
    _.bindAll(this, 'render');

    this.cardListView = new CardListView()
    this.riderListView = new RiderListView()

    this.render()
  },

  addCard: function (card) {
    this.cardListView.addCard(card)
  },

  addRiders: function(riders) {
    this.riderListView.addRiders(riders)
  },

  render: function () {
    return this.$el.append(this.cardListView.el).append(this.riderListView.el)
  }
})
