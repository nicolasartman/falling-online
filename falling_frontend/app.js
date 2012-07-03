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
          card: null
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
  
  // TODO: remove
  $scope.newGame(4);

  // $scope.pickUpCard = function (card) {
  //   if (!$scope.myHand) {
  //     $scope.myHand = card
  //     return true
  //   } else {
  //     return false
  //   }
  //   // $scope.$apply()
  // }
}

function RiderController($scope) {

  $scope.rider = $scope.$parent.player.rider;

  $scope.playCard = function (card) {
    // console.log("playCard"); console.log($scope)
    if (!$scope.rider.card) {
      $scope.rider.card = card;
      return true;
    } else {
      return false;
    }
  };
}

var fallingModule = new angular.module("falling", []);

fallingModule.directive("card", function () {
  return {
    restrict:"E",
    link: function ($scope, element, attrs) {
      $(element).on('mousedown', function (event, $index) {
        // attempt to pick up the card
        if ($scope.pickUpCard($scope.card)) {
          $scope.removeCard($scope.$index);
        }
        $scope.$apply();
      });
    }
  };
});

fallingModule.directive("rider", function () {
  return {
    restrict:"E", // it's an html element
    link: function ($scope, element, attrs) {
      $(element).on('mouseup', function (event) {
        console.log("card dropped on rider test");
      }).draggable();
    }
  };
});

fallingModule.directive("hand", function () {
  return {
    restrict: "A",
    link: function ($scope, element, attrs) {
      $(document).bind('mousemove', function (event) {
        if ($scope.hasCard()) {
          $('#hand').offset({
            left: (event.pageX - ($('#hand').width() / 2.0)),
            top: (event.pageY - ($('#hand').height() / 2.0))
          });
        }
      })
      .bind('mouseup', function (cursor) {
        $('.rider').each(function (index) {
          var rider = $(this);
          // hit test - if the player is attempting to play their hand onto this
          if ((cursor.pageX > rider.offset().left && // left edge
              cursor.pageX < rider.offset().left + rider.width()) && // right edge
              (cursor.pageY > rider.offset().top && // top edge
              cursor.pageY < rider.offset().top + rider.height())) { // bottom edge
            var $rider = angular.element(this).scope();
            // If card is successfully played on thie rider, clear the hand
            // TODO: account for invalidated plays
            if ($rider.playCard($scope.card)) {
              $scope.clearHand();
              console.log("drop callback");
              console.log($scope);
              // Don't let player draw another card until the move is validated
              $scope.pendingValidation = true;
            }
          }
        });
      });
    }
  };
});



























