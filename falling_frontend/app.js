"use strict"

var us = _.noConflict()

// The global game state
var state = {}

function dealCard

function PlayerListController($scope) {
  var playerCount = 5
  
  $scope.players = []
  
  for (var playerNum = 0; playerNum < playerCount; playerNum++){
    $scope.players.push({
      playerNumber: playerNum,
      rider: {
        card: null
      },
      stacks: [[]]
    })
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
    console.log($scope.cards)
  }
}

function RiderController($scope) {
  
  console.log($scope)
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
      $(element).draggable()
      $(element).on('click', function (event, $index) {
        $scope.removeCard($scope.$index)
        $scope.$apply()
      })
      $(element).on('cardPlayed', function (event) {
        // Act on the event
      })
    }
  }
})

fallingModule.directive("rider", function () {
  return {
    restrict:"E", // it's an html element
    link: function ($scope, element, attrs) {
      $(element).droppable({
        accept: ".card",
        activate: function (event, ui) {
          // if (!self.model.hasCard()) {
            element.addClass('available-rider')
          // }
        },
        deactivate: function(event, ui) {
          element.removeClass('available-rider')
        },
        over: function (event, ui) {
          $(ui.draggable).addClass('card-hovering-rider')
        },
        out: function(event, ui) {
          $(ui.draggable).removeClass('card-hovering-rider')
        },
        drop: function (event, ui) {
          // TODO: put this somewhere it will be triggered any time a card is added to a rider
          element.removeClass('available-rider')
          // If play was successful, center card on the rider
          if ($scope.playCard($scope, {card:"snargle"})) {
            console.log($(element).offset())
            ui.draggable.offset($(element).offset())
            ui.draggable.draggable('disable')
            
          }
        }
      }).draggable()
    }
  }
})