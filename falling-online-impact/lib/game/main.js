/*global
 MyGame: true,
 ig: true,
 EntityCard: true,
 EntityStack: true,
 EntityPlayer: true,
 EntityRider: true
*/

ig.module(
  'game.main'
)
.requires(
  'impact.game',
  'impact.font',
  'game.entities.player',
  'game.entities.card',
  'game.entities.stack'
)
.defines(function (){

  MyGame = ig.Game.extend({

    // Load a font
    font: new ig.Font('media/04b03.font.png'),

    init: function() {
      // Initialize your game here; bind keys etc.
      ig.input.bind(ig.KEY.MOUSE1, 'mouse1')

      ig.game.spawnEntity(EntityPlayer, 0, 0)

      // Random testing code
      for (var i = 5; i >= 0; i--){
        ig.game.spawnEntity(EntityCard, 100 + i * 150, 300, { playable: true })
      }
      ig.game.spawnEntity(EntityRider, 10, 10)
      ig.game.sortEntities()
    },

    update: function () {
      // Update all entities and backgroundMaps
      this.parent()
    },

    draw: function () {
      // Draw all entities and backgroundMaps
      this.parent()

      // Add your own drawing code here
      // this.font.draw( ig.system.fps.toString(), 0, 0, ig.Font.ALIGN.LEFT )
    }
  })

  // Start the Game with 60fps, a resolution of 1200x800
  ig.main('#canvas', MyGame, 40, 1200, 800, 1);

})