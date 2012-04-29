/*global 
 jquery:true,
 $:true,
 us: true,
 window: true,
 _: true,
 Backbone: true,
 Card: true
*/

window.Card = Backbone.Model.extend({
  defaults: {
    kind: "None",
    playable: false
  }
})

window.CardList = Backbone.Collection.extend({
  model: Card,
  
  initialize: function () {
    _.bindAll(this, 'add')
    this.nextCardUID = 0
    this.on('add', this.addUID)
  },
  
  addUID: function (models, options) {
    var self = this
    if (!models.length) {
      models.set({ uid: self.nextCardUID++ })
    }
    else {  
      _.each(models, function (curr) {
        curr.set({ uid: self.nextCardUID++ }) // Add UID and increment
      })
    }
  }
})

window.Rider = Backbone.Model.extend({
  defaults: {
    card: null
  },
  
  initialize: function() {
    
  },  
  
  hasCard: function () {
    return this.get('card') !== null
  },
  
  addCard: function(card) {
    if (!(card instanceof Card)) {
      console.log("Not a card:") 
      console.log(card)
    } else if (this.get('card') === null) {
      this.set({ "card": card })
      console.log("Rider now has a " + this.get('card').get('kind') + " card on it")
      // TODO: clean up
      return true
    } else {
      console.log("Rider Already Has Card")
    }
    return false
  },

  clearCard: function() {
    if (this.hasCard()) {
      this.get('card').destroy()
      // TODO: make it automatically react to its card being destroyed by setting it to null
      this.set('card', null)
    }
  }
  
})

window.RiderList = Backbone.Collection.extend({
  model: Rider
})

