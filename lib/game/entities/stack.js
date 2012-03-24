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
  'impact.entity',
  'game.entities.card'
)
.defines(function () {

  EntityStack = ig.Entity.extend({
    size: { x: 150, y: 200 },
    
    type: ig.Entity.TYPE.A,

    zIndex: 1,
    
    animSheet: new ig.AnimationSheet('media/stack.png', 150, 200),
    
    init: function (x, y, settings) {
      this.parent(x, y, settings)
      
      this.addAnim('none', 1, [0])
      // Note: this animation length is tied to the timer in the card class
      this.addAnim('hover', 0.1, [0,1,2,3,2,1,0,0,0,0], true)
    }
  })

})