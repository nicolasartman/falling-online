/*global 
  jquery:true,
  $:true,
  us: true,
  ig: true,
  EntityCard: true,
  EntityStack: true
*/

ig.module('game.entities.card')
.requires(
  'impact.entity',
  'game.entities.stack',
  'game.entities.rider'
)
.defines(function () {

  EntityCard = ig.Entity.extend({
    isBeingDragged: false,
    
    size: { x: 150, y: 200 },

    type: ig.Entity.TYPE.CARD,
    
    playable: false,
    // checkAgainst: ig.Entity.TYPE.STACK,
    
    zIndex: 10,

    animSheet: new ig.AnimationSheet('media/card.png', 150, 200),

    init: function (x, y, settings) {
      this.parent(x, y, settings)

      this.addAnim('none', 1, [0])
      
      if (settings.playable) {
        this.playable = settings.playable
      }
    },

    update: function () {
      if (this.isBeingDragged) {
        this.pos.x = ig.input.mouse.x - (this.size.x/2)
        this.pos.y = ig.input.mouse.y - (this.size.y/2)
      }
      
      this.parent()
    },
    
    collideWith: function (other, axis) {
        console.log("Dropped")
      if (other instanceof EntityStack && !this.isBeingDragged) {
      }
    },
    
    underCursor: function() {
      return (
        (this.pos.x <= (ig.input.mouse.x + ig.game.screen.x)) &&
        ((ig.input.mouse.x + ig.game.screen.x) <= this.pos.x + this.size.x) &&
        (this.pos.y <= (ig.input.mouse.y + ig.game.screen.y)) &&
        ((ig.input.mouse.y + ig.game.screen.y) <= this.pos.y + this.size.y)
      )
    },

    pickUp: function () {
      this.isBeingDragged = true
    },

    attemptPlay: function () {
      // Check for a valid play on an empty stack
      var stacks = ig.game.getEntitiesByType(EntityRider),
          stack

      for (var i = stacks.length - 1; i >= 0; i--){
        stack = stacks[i]
        if (this.touches(stack) && stack.getCards().length === 0) {
          stack.push(this)
          this.isBeingDragged = false
          this.playable = false
          return true
        }
      }
      
      return false
    }
  })
})