"use strict"

var us = _.noConflict()

/* The global game state */
var state = {}
var myPlayerNumber = 0

$(document).ready(function () {
  $(document).bind('mousemove', function (event) {
    var x, y
    x = event.pageX - ($('#hand').width() / 2.0)
    y = event.pageY - ($('#hand').height() / 2.0)
    $('#hand').offset({left: x, top: y })
  })
})


function dealCard(playerNumber, stackNumber, card) {
  state.players[playerNumber].stacks[stackNumber].push(card)
  // angular.$apply()
}

function PlayerListController($scope) {
  var playerCount = 5
  state.players = []
  $scope.players = state.players
  
  for (var playerNum = 0; playerNum < playerCount; playerNum++){
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
  
  $scope.playCard = function ($riderScope, $cardScope) {
    console.log($riderScope)
    console.log($cardScope)
    if (!$riderScope.rider.card) {
      $riderScope.rider.card = $cardScope.card
      return true
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