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
  }
})

window.RiderList = Backbone.Collection.extend({
  model: Rider
})

