/*global 
  jquery:true,
  $:true,
  us: true,
  ig: true,
  EntityCard: true
*/

ig.module('game.entities.card')
.requires(
  'impact.entity',
  'game.entities.stack'
)
.defines(function () {

  EntityCard = ig.Entity.extend({
    isBeingDragged: false,
    
    size: { x: 150, y: 200 },

    type: ig.Entity.TYPE.A,
    
    collides: ig.Entity.COLLIDES.LITE,

    zIndex: 10,

    animSheet: new ig.AnimationSheet('media/card.png', 150, 200),

    init: function (x, y, settings) {
      this.parent(x, y, settings)

      this.addAnim('none', 1, [0])
      
      if (settings.moveToStack) {
        
      }
    },

    update: function () {
      if (this.isBeingDragged) {
        this.pos.x = ig.input.mouse.x - (this.size.x/2)
        this.pos.y = ig.input.mouse.y - (this.size.y/2)
      }
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

    startDrag: function () {
      this.isBeingDragged = true
    },

    stopDrag: function () {
      this.isBeingDragged = false
    }

  })
})