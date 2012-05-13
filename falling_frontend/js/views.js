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
 RiderList: true,
 CardListView: true,
 RiderListView: true
*/

// ====================
// = Basic Data Views =
// ====================

"use strict";

window.CardView = Backbone.View.extend({
  tagName: 'div',
  className: 'card',
  events: {
    'click': 'toggleKind',
    'dragstart': 'startDrag',
    'droppedOn': 'droppedOn'
  },

  initialize: function () {
    _.bindAll(this)
    // this.$el.draggable({
    //   stack: ".card",
    //   containment: 'document'
    // })

    this.model.on('destroy', this.destroyView)
    this.model.on('change', this.render)

    this.render()
  },

  destroyView: function () {
    this.off()
    this.model.off(null, null, this) // unbind everything
    this.remove() // remove from the dom
  },

  render: function () {
    return this.$el.html(this.model.get('kind') + this.model.get('uid'))
  },

  // Controller Logic
  droppedOn: function(event, riderView) {
    // Returns true if the card was accepted onto the rider spot
    if (riderView.model.addCard(this.model)) {
      this.$el.draggable('disable') // card should no longer be draggable
      this.$el.offset(riderView.$el.offset())
    }
  },

  toggleKind: function () {
    this.model.set({
      kind: 'test'
    })
  },

  startDrag: function(event) {
    this.inHand = true
  }
})


window.RiderView = Backbone.View.extend({
  tagName: 'div',
  className: 'rider',

  events: {
    // 'click button#add': 'callback'
  },

  initialize: function() {
    var self = this

    _.bindAll(this)

    if (!this.model) {
      this.model = new Rider()
    }

    this.model.on('destroy', this.destroyView)
    this.model.on('change', this.render)
    this.on('droppedOn', this.droppedOn)

    this.$el.droppable({
      accept: ".card",
      activate: function (event, ui) {
        if (!self.model.hasCard()) {
          self.$el.addClass('available-rider')
        }
      },
      deactivate: function(event, ui) {
        self.$el.removeClass('available-rider')
      },
      over: function (event, ui) {
        $(ui.draggable).addClass('card-hovering-rider')
      },
      out: function(event, ui) {
        $(ui.draggable).removeClass('card-hovering-rider')
      },
      drop: function (event, ui) {
        // TODO: put this somewhere it will be triggered any time a card is added to a rider
        self.$el.removeClass('available-rider')
        ui.draggable.trigger('droppedOn', self)
      }
    })

    this.render()
  },

  render: function() {
    console.log("rendering")
    return this.$el
  }
})

// ====================
// = Collection Views =
// ====================

window.CardListView = Backbone.View.extend({

  tagName:'div',
  className: 'stack',

  events: {
    // 'click button#add': 'callback'
  },

  initialize: function() {
    _.bindAll(this)

    this.cards = new CardList()

    this.render(); // not all views are self-rendering. This one is.
  },

  addCardView: function (cardView) {
    this.cards.add(cardView.model)
    this.$el.append(cardView.$el)
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

  initialize: function() {
    _.bindAll(this)

    this.riders = new RiderList()
    this.riders.on('add', this.addRiderView)

    this.render()
  },

  addRiders: function(riders) {
    this.riders.add(riders)
  },

  addRiderView: function (rider) {
    var view = new RiderView({ model: rider })
    this.$el.append(view.$el)
  },

  render: function() {
    return this
  }
})

window.PlayerView = Backbone.View.extend({

  tagName: 'div',
  className: 'player',

  events: {
    // 'click button#add': 'callback'
  },

  initialize: function () {
    _.bindAll(this)
    
    this.stacks = []
    this.addStack()
    
    this.riderView = new RiderView()
    this.$el.append(this.riderView.$el)
  },

  addStack: function () {
    var view = new CardListView()
    this.stacks.push(view)
    this.$el.append(view.$el)
  },
  
  dealCard: function(cardView, stackNumber) {
    if (stackNumber === undefined || stackNumber >= this.stacks.length) {
      console.log("Invalid stack number for player: " + stackNumber)
    }
    else {    
      this.stacks[stackNumber].addCardView(cardView)
    }
  },
  
  clearRider: function() {
    this.riderView.model.clearCard()
  },  
  
  render: function () {
    return this
  }
})

window.AppView = Backbone.View.extend({

  events: {
    // 'click button#add': 'callback'
  },

  initialize: function () {
    _.bindAll(this);

    this.playerViews = []
    
    if (this.options.players) {
      for (var i = this.options.players - 1; i >= 0; i--) {
        this.addPlayer()
      }
    }
    
    this.riderListView = new RiderListView()

    this.render()
  },

  addRiders: function(riders) {
    this.riderListView.addRiders(riders)
  },

  addPlayer: function(options) {
    var view = new PlayerView()
    this.playerViews.push(view)
    this.$el.append(view.$el)
  },

  dealCard: function(cardView, playerNumber, stackNumber) {
    if (playerNumber === undefined || playerNumber >= this.playerViews.length) {
      console.log("Invalid player number: " + playerNumber)
    }
    else {
      this.playerViews[playerNumber].dealCard(cardView, stackNumber)
    }
  },
  
  addStack: function(playerNumber) {
    if (playerNumber === undefined || playerNumber >= this.playerViews.length) {
      console.log("Invalid player number: " + playerNumber)
    }
    else {
      this.playerViews[playerNumber].addStack()
    }    
  },  
  
  clearRider: function(playerNumber) {
    if (playerNumber === undefined || playerNumber >= this.playerViews.length) {
      console.log("Invalid player number: " + playerNumber)
    }
    else {
      this.playerViews[playerNumber].clearRider()
    }    
  },
  
  playerCount: function() {
    return this.playerViews.length
  },
  
  
  render: function () {
    return this
  }
})
