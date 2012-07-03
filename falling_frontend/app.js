/*global
 jquery:true,
 $:true,
 us: true,
 _: true,
 document: true,
 angular: true
*/


var us = _.noConflict();

var myPlayerNumber = 0;

var fallingModule = new angular.module("falling", []);

function GameController($scope) {
  $scope.gameState = {
    players: []
  };

  // Sets up a new game with the specified number of players
  $scope.newGame = function (playerCount) {
    for (var playerNum = 0; playerNum < playerCount; playerNum++) {
      $scope.gameState.players.push({
        playerNumber: playerNum,
        hand: null,
        rider: {
          card: null,
          extras: 0
        },
        stacks: [[]]
      });
    }
  };

  $scope.removeCard = function (index) {
    $scope.cards.splice(index, 1);
  };

  // Gets all stacks for a player
  $scope.getStacks = function (playerNumber) {
    return $scope.gameState.players[playerNumber].stacks;
  };

  $scope.getMyHand = function () {
    return $scope.gameState.players[myPlayerNumber].hand;
  };

  var setMyHand = function (card) {
    $scope.gameState.players[myPlayerNumber].hand = card;
  };

  $scope.drawCard = function (card) {
    if (!$scope.getMyHand()) {
      setMyHand(card);
    }
  };

  $scope.getRiderCard = function (playerNumber) {
    return $scope.gameState.players[playerNumber].rider.card;
  };
  
  // TODO: remove
  $scope.newGame(4);

  $scope.pickUpCardFromStack = function (stackNumber) {
    if (!$scope.getMyHand()) {
      setMyHand($scope.gameState.players[myPlayerNumber].stacks[stackNumber].pop());
    }
  };

  $scope.dealCard = function (playerNumber, stackNumber, card) {
    $scope.gameState.players[playerNumber].stacks[stackNumber].push(card);
  };
  
  $scope.playCard = function (playerNumber) {
    var rider = $scope.gameState.players[playerNumber].rider;
    var card = $scope.getMyHand();
    
    if (!rider.card && card.kind !== "extra") {
      rider.card = card;
      setMyHand(null)
    } else if (rider.card && card.kind === "extra") {
      rider.extras += 1;
      setMyHand(null)
    }
    $scope.$apply();
  };
  
  $scope.clearRider = function (playerNumber) {
    // TODO: add condition for skip
    $scope.gameState.players[playerNumber].rider.card = null;
    $scope.gameState.players[playerNumber].rider.extras = 0;
  };

  // TODO: remove in prod
  $scope.dealCard(myPlayerNumber, 0, { name: "cheese", number: 22 });
  $scope.dealCard(myPlayerNumber, 0, { name: "cheesier", number: 222 });
}


fallingModule.directive("hand", function () {
  return {
    restrict: "A",
    link: function ($scope, element, attrs) {
      $(document).bind('mousemove', function (event) {
        if ($scope.getMyHand()) {
          $('#hand').offset({
            left: (event.pageX - ($('#hand').width() / 2.0)),
            top: (event.pageY - ($('#hand').height() / 2.0))
          });
        }
      })
      .bind('mouseup', function (cursor) {
        $('.rider').each(function (index) {
          var playerNumber = $(this).attr("playerNumber");
          var rider = $(this);
          var $rider = angular.element(this).scope();
          
          // hit test - if the player is attempting to play their hand onto this
          if ((cursor.pageX > rider.offset().left && // left edge
              cursor.pageX < rider.offset().left + rider.width()) && // right edge
              (cursor.pageY > rider.offset().top && // top edge
              cursor.pageY < rider.offset().top + rider.height())) { // bottom edge
            

            // TODO: account for invalidated plays
            $rider.playCard(playerNumber);
          }
        });
      });
    }
  };
});