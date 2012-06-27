/*global 
 jquery:true,
 $:true,
 us: true,
 _: true,
 document: true,
 angular: true
*/


var us = _.noConflict()

/* The global game state */
var state = {}
var myPlayerNumber = 0

// function dealCard(playerNumber, stackNumber, card) {
//   state.players[playerNumber].stacks[stackNumber].push(card)
//   // angular.$apply()
// }

function GameController($scope) {
  
}

function PlayerListController($scope) {
  var playerCount = 5
  state.players = []
  $scope.players = state.players
  
  for (var playerNum = 0; playerNum < playerCount; playerNum++) {
    state.players.push({
      playerNumber: playerNum,
      hand: null,
      rider: {
        card: null
      },
      stacks: [[]]
    })
  }
  
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

function HandController($scope) {
  $scope.card = null
  
  $scope.drawCard = function (card) {
    if (!$scope.card) {
      $scope.card = card      
    }
    return $scope.hasCard()
  }
  
  $scope.hasCard = function () {
    return $scope.card !== null
  }
  
  $scope.clearHand = function () {
    console.log($scope)
    $scope.card = null
  }
}

function CardListController($scope) {
  $scope.cards = [
    { number: 1 },
    { number: 2 },
    { number: 3 }
  ]
  
  $scope.removeCard = function (index) {
    $scope.cards.splice(index, 1)
  }
}

function RiderController($scope) {
  
  $scope.rider = $scope.$parent.player.rider
  
  $scope.playCard = function (card) {
    // console.log("playCard"); console.log($scope)
    if (!$scope.rider.card) {
      $scope.rider.card = card
      return true
    } else {
      return false
    }
  }
}

var fallingModule = new angular.module("falling", [])

fallingModule.directive("card", function () {
  return {
    restrict:"E",
    link: function ($scope, element, attrs) {
      $(element).on('mousedown', function (event, $index) {
        // attempt to pick up the card
        if ($scope.pickUpCard($scope.card)) {
          $scope.removeCard($scope.$index)          
        }
        $scope.$apply()
      })
    }
  }
})

fallingModule.directive("rider", function () {
  return {
    restrict:"E", // it's an html element
    link: function ($scope, element, attrs) {
      $(element).on('mouseup', function (event) {
        console.log("card dropped on rider test")
      }).draggable()      
    }
  }
})

fallingModule.directive("hand", function () {
  return {
    restrict: "A",
    link: function ($scope, element, attrs) {
      $(document).bind('mousemove', function (event) {
        if ($scope.hasCard()) {
          $('#hand').offset({
            left: (event.pageX - ($('#hand').width() / 2.0)),
            top: (event.pageY - ($('#hand').height() / 2.0))  
          })      
        }
      })
      .bind('mouseup', function (cursor) {
        $('.rider').each(function (index) {
          var rider = $(this)
          // hit test - if the player is attempting to play their hand onto this
          if ((cursor.pageX > rider.offset().left && // left edge
              cursor.pageX < rider.offset().left + rider.width()) && // right edge
              (cursor.pageY > rider.offset().top && // top edge
              cursor.pageY < rider.offset().top + rider.height())) { // bottom edge
            var $rider = angular.element(this).scope()
            // If card is successfully played on thie rider, clear the hand
            // TODO: account for invalidated plays
            if ($rider.playCard($hand.myHand)) {
              $hand.clearHand()
              console.log("drop callback")
              console.log($hand)
              // Don't let player draw another card until the move is validated
              $hand.pendingValidation = true
            }
          }
        })
      })
    }
  }
})



























