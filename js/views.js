window.CardView = Backbone.View.extend({
  tagName: 'div',
  className: "card",
  events: {
    'click': 'toggleKind',
    'dragstart': 'startDrag',
    'dragstop': 'stopDrag'
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
  
  stopDrag: function (event) {
    return false
  }
  
})


var RiderView = Backbone.View.extend({
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

/*global
 jquery:true,
 $:true,
 us: true,
 Backbone: true,
 _: true,
*/
window.CardListView = Backbone.View.extend({

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
    return this.$el
  }
})

window.RiderListView = Backbone.View.extend({
  
  events: {
    // 'click button#add': 'callback'
  },
  
  initialize: function(){
    _.bindAll(this)
    
    this.riders = new RiderList()
    this.riders.on('add', this.render)
    
    this.render(); // not all views are self-rendering. This one is.
  },
  
  addRiders: function(riders) {
    this.riders.add(riders)
  },
  
  render: function(){
    _.each(this.riders, function (rider) {
      $(this.el).append(rider.el);        
    })
  }
})


window.AppView = Backbone.View.extend({

  events: {
    // 'click button#add': 'callback'
  },

  initialize: function () {
    _.bindAll(this, 'render');

    this.cardListView = new CardListView({ tagName:'div', id: 'cards' })
    this.riderListView = new RiderListView({ tagName:'div', id: 'riders' })

    this.render() // not all views are self-rendering. This one is.
  },

  addCard: function (card) {
    this.cardListView.addOne(card)
  },
  
  addRiders: function(riders) {
    this.riderListView.addRiders(riders)
  },
  
  render: function () {
    return this.$el
  }
})
