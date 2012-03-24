/*global 
  jquery:true,
  $:true,
  us: true,
  ig: true,
  EntityCard: true
*/

ig.module('game.entities.card')
.requires(
  'impact.entity'
)
.defines(function () {

  EntityCard = ig.Entity.extend({
    isBeingDragged: false,
    
    size: { x: 150, y: 200 },

    checkAgainst: ig.Entity.TYPE.A,

    zIndex: 10,

    animSheet: new ig.AnimationSheet('media/card.png', 150, 200),

    init: function (x, y, settings) {
      this.parent(x, y, settings)

      this.addAnim('none', 1, [0])
    },

    update: function () {
      if (this.isBeingDragged) {
        this.pos.x = ig.input.mouse.x - (this.size.x/2)
        this.pos.y = ig.input.mouse.y - (this.size.y/2)
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
    
    touchTimer: null,
    
    check: function (stack) {
      if (this.touchTimer === null) {
        this.touchTimer = new ig.Timer(1)
      }
      if (this.touchTimer.delta() > 0) {
        stack.currentAnim = stack.anims.hover.rewind()
        this.touchTimer.reset()
      }
    },

    startDrag: function () {
      this.isBeingDragged = true
    },

    stopDrag: function () {
      this.isBeingDragged = false
    }

  })
})