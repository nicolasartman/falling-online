/*global 
us: true,
EntityPlayer: true,
EntityCard: true,
ig: true
*/


ig.module('game.entities.player')
.requires(
  'impact.entity',
  'game.entities.card'
)
.defines(function () {
  
  EntityPlayer = ig.Entity.extend({
    cardInHand: null,
  
    size: { x: 1, y: 1 },
    // animSheet: new ig.AnimationSheet('', 1, 1),

    init: function (x, y, settings) {
      this.parent(x, y, settings)
    },

    update: function () {

      // If the player has an empty hand and is attempting to grab a card
      if (!this.cardInHand && ig.input.pressed('mouse1')) {
        var cards = ig.game.getEntitiesByType(EntityCard)
        for (var currCard = cards.length - 1; currCard >= 0; currCard--) {
          if (cards[currCard].underCursor() && cards[currCard].playable) {
            cards[currCard].pickUp()
            this.cardInHand = cards[currCard]
            break
          }
        }
      } else if (this.cardInHand && ig.input.released('mouse1')) {
        // If play is successful
        if (this.cardInHand.attemptPlay()) {
          console.log("Card played")
          this.cardInHand = null          
        }
      }
    }
  })
})