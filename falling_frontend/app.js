/*global
 jquery:true,
 $:true,
 us: true,
 _: true,
 jQuery: true,
 setTimeout: true,
 document: true,
 angular: true,
 WebSocket: true
*/


console.log([3,4,5].join([4,6]))

var us = _.noConflict();

var myPlayerNumber = 0;


// Creates a placeholder card that animates to the stack

function animateDeal(card, playerNumber, stackNumber, animationDuration, continuation) {
  // Initial card placeholder creation and insertion in the dom
  var animCard = $("<div />", {
    "class": "animated card",
    "text": card.name
  })
  .offset({
    top: 500,
    left: "50%"
  })
  .css("-webkit-transition-duration", animationDuration + "s");
  $("body").append(animCard);
  // Trigger the animation - send the card to the requested stack
  animCard.offset($("#player-" + playerNumber + "-stack-" + stackNumber).offset());
  setTimeout(function () {
    animCard.remove();
  
    if (continuation && us.isFunction(continuation)) {
      continuation(card, playerNumber, stackNumber);
    }
  }, animationDuration * 1000);

}

// TODO: remove -- test stuff
jQuery(document).ready(function($) {
  // deal a card
  animateDeal({ name: "poof", kind: "HIT" }, 0, 0, 1, function (card, playerNumber, stackNumber) {
    var $gameCtrl = angular.element($("#app")).scope();
    $gameCtrl.dealCard(card, playerNumber, stackNumber);
    $gameCtrl.$apply();
    
    // then deal another
    animateDeal({ name: "poof", kind: "HIT" }, 0, 0, 1, function (card, playerNumber, stackNumber) {
      $gameCtrl.dealCard(card, playerNumber, stackNumber);
      $gameCtrl.$apply();
    });
  });
});

var fallingModule = new angular.module("falling", []);

fallingModule.factory('Server', function () {
  
  var socket = new WebSocket("ws://localhost:8080")
  
  socket.onopen = function () {
    console.log("Connected to server");
    socket.send("data");
  };
  
  socket.onmessage = function (message) {
    console.log(message);
  };
  
  socket.onerror = function (err) {
    console.log("ERROR")
    console.log(err)
  }
});

fallingModule.controller('GameController', function ($scope, Server) {
  $scope.gameState = {
    players: []
  };

  // Test method. TODO: delete
  $scope.testDeal = function () {
    console.log($scope.gameState)
    if (!$scope.gameState.players[myPlayerNumber].stacks.length) {
      $scope.gameState.players[myPlayerNumber].stacks.push([])
      $scope.$apply()
    }
    animateDeal({ name: "hit", kind: "HIT" }, 0, 0, 0.25, function (card, playerNumber, stackNumber) {
      var $gameCtrl = angular.element($("#app")).scope();
      $gameCtrl.dealCard(card, playerNumber, stackNumber);
      $gameCtrl.$apply();
    });
    console.log($scope.gameState.players[myPlayerNumber].stacks);
  }

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
    var stack = $scope.gameState.players[myPlayerNumber].stacks[stackNumber];
    if (!$scope.getMyHand()) {
      setMyHand(stack.pop());
      // IF the stack is now empty, remove it from the player's stacks
      if (stack.length === 0) {
        $scope.gameState.players[myPlayerNumber].stacks.splice(stackNumber, 1);
        console.log("Emptied stack");
        console.log($scope.gameState.players[myPlayerNumber].stacks);
      }
    }
  };

  $scope.dealCard = function (card, playerNumber, stackNumber) {
    $scope.gameState.players[playerNumber].stacks[stackNumber].push(card);
  };
  
  $scope.playCard = function (playerNumber) {
    var rider = $scope.gameState.players[playerNumber].rider;
    var card = $scope.getMyHand();
    
    if (!rider.card) {
      rider.card = card;
      setMyHand(null);
    } else if (rider.card && card.kind === "extra") {
      rider.extras += 1;
      setMyHand(null);
    }
    $scope.$apply();
  };
  
  $scope.clearRider = function (playerNumber) {
    // TODO: add condition for skip
    $scope.gameState.players[playerNumber].rider.card = null;
    $scope.gameState.players[playerNumber].rider.extras = 0;
  };
});


fallingModule.directive("hand", function () {
  return {
    restrict: "A",
    link: function ($scope, element, attrs) {
      $(document).bind('mousemove', function (event) {
        // if ($scope.getMyHand()) {
        $('#hand').offset({
          left: (event.pageX - ($('#hand').width() / 2.0)),
          top: (event.pageY - ($('#hand').height() / 2.0))
        });
        // }
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