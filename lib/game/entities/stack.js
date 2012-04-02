/*global
jquery:true,
$:true,
us: true,
ig: true,
EntityCard: true,
EntityStack: true
*/

ig.module('game.entities.stack')
.requires(
  'impact.entity'
)
.defines(function () {

  EntityStack = ig.Entity.extend({
    size: { x: 150, y: 200 },

    type: ig.Entity.TYPE.STACK,

    zIndex: 1,

    animSheet: new ig.AnimationSheet('media/stack.png', 150, 200),
    
    cards: [],

    init: function (x, y, settings) {
      this.parent(x, y, settings)

      this.addAnim('none', 1, [0])
      // Note: this animation length is tied to the timer in the card class
      this.addAnim('hover', 0.1, [1,2,3,2,1,0], true)
    },

    getCards: function () {
      return this.cards
    },
    
    push: function (card) {
      card.pos.x = this.pos.x
      card.pos.y = this.pos.y
      return this.cards.push(card)
    },
    
    pop: function () {
      return this.cards.pop()
    }

  })

})



















