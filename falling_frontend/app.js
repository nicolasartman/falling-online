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

$(document).ready(function ($) {
  $(document).bind('mousemove', function (event) {
    var x = (event.pageX - ($('#hand').width() / 2.0)),
    y = (event.pageY - ($('#hand').height() / 2.0))
    $('#hand').offset({left: x, top: y })
  })
  .bind('mouseup', function (cursor) {
    $('.rider').each(function (index) {
      var rider = $(this)
      // hit test
      if ((cursor.pageX > rider.offset().left && // left edge
          cursor.pageX < rider.offset().left + rider.width()) && // right edge
          (cursor.pageY > rider.offset().top && // top edge
          cursor.pageY < rider.offset().top + rider.height())) { // bottom edge
        var $rider = angular.element(this).scope()
        var $hand = angular.element($('#hand')).scope()
        // If card is successfully played, clear the hand
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
})


// function dealCard(playerNumber, stackNumber, card) {
//   state.players[playerNumber].stacks[stackNumber].push(card)
//   // angular.$apply()
// }

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
  
  $scope.pickUpCard = function (card) {
    if (!$scope.myHand) {
      $scope.myHand = card
      return true
    } else {
      return false
    }
    // $scope.$apply()
  }
}

function HandController($scope) {
  $scope.clearHand = function () {
    console.log($scope)
    // $scope.myHand = null
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
      // $(element).droppable({
      //         accept: ".card",
      //         activate: function (event, ui) {
      //           // if (!self.model.hasCard()) {
      //             element.addClass('available-rider')
      //           // }
      //         },
      //         deactivate: function(event, ui) {
      //           element.removeClass('available-rider')
      //         },
      //         over: function (event, ui) {
      //           $(ui.draggable).addClass('card-hovering-rider')
      //         },
      //         out: function(event, ui) {
      //           $(ui.draggable).removeClass('card-hovering-rider')
      //         },
      //         drop: function (event, ui) {
      //           // TODO: put this somewhere it will be triggered any time a card is added to a rider
      //           element.removeClass('available-rider')
      //           // If play was successful, center card on the rider
      //           if ($scope.playCard($scope, {card:"snargle"})) {
      //             // console.log(angular.element(ui.draggable).scope())
      //             ui.draggable.offset($(element).offset())
      //             ui.draggable.draggable('disable')
      //           }
      //         }
      //       })
      $(element).on('mouseup', function (event) {
        console.log("card dropped on rider test")
      }).draggable()
      
    }
  }
})