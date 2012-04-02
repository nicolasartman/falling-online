/*global
 jquery:true,
 EntityStack:true,
 EntityRider: true,
 ig: true
*/

ig.module('game.entities.rider')
.requires(
  'impact.entity',
  'game.entities.stack'
)
.defines(function () {

  EntityRider = EntityStack.extend({

    checkAgainst: ig.Entity.TYPE.CARD,

    init: function (x, y, settings) {
      this.parent(x, y, settings)
    },

    touchTimer: new ig.Timer(1),

    check: function (card) {
      if (this.cards.length === 0 && card.isBeingDragged && this.touchTimer.delta() > 0) {
        this.currentAnim = this.anims.hover.rewind()
        this.touchTimer.reset()
      }
    }


  })

})