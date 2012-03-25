/*global
 MyGame: true,
 ig: true,
 EntityCard: true,
 EntityStack: true
*/

ig.module( 
  'game.main' 
)
.requires(
  'impact.game',
  'impact.font',
  'game.entities.card',
  'game.entities.stack'
)
.defines(function(){

MyGame = ig.Game.extend({
  cardInHand: null,
  
  // Load a font
  font: new ig.Font( 'media/04b03.font.png' ),
  
  
  init: function() {
    // Initialize your game here; bind keys etc.
    ig.input.bind(ig.KEY.MOUSE1, 'mouse1')
    
    for (var i = 5; i >= 0; i--){ 
      ig.game.spawnEntity(EntityCard, 300, 300)
    }
    ig.game.spawnEntity(EntityStack, 10, 10)
    ig.game.sortEntities()
  },
  
  update: function() {
    // Update all entities and backgroundMaps
    this.parent()
    
    // Add your own, additional update code here
    
    // If the player has an empty hand and is attempting to grab a card
    if (!this.cardInHand && ig.input.pressed('mouse1')) {
      var cards = ig.game.getEntitiesByType(EntityCard)
      for (var currCard = cards.length - 1; currCard >= 0; currCard--){
        if (cards[currCard].underCursor()) {
          cards[currCard].startDrag()
          this.cardInHand = cards[currCard]
          break
        }
      }
    } else if (this.cardInHand && ig.input.released('mouse1')) {
      this.cardInHand.stopDrag()
      this.cardInHand = null
    }
  },
  
  draw: function() {
    // Draw all entities and backgroundMaps
    this.parent()
    
    // Add your own drawing code here
    // this.font.draw( ig.system.fps.toString(), 0, 0, ig.Font.ALIGN.LEFT )
  }
})


// Start the Game with 60fps, a resolution of 320x240, scaled
// up by a factor of 2
ig.main( '#canvas', MyGame, 60, 1200, 800, 1 );

  // ig.game.spawnEntity(EntityCard, 100, 100)
  
})





















