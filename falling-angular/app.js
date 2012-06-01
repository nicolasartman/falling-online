function CardListController($scope) {
  $scope.cards = [
    { number: 1 },
    { number: 2 },
    { number: 5 }
  ]
  
  $scope.riders = [
    {}, 
    {}
  ]
  
  $scope.removeCard = function (index) {
    $scope.cards.splice(index, 1)
    console.log($scope.cards)
  }
}

var myModule = new angular.module("falling", [])

myModule.directive("card", function () {
  return {
    restrict:"E",
    link: function ($scope, element, attrs) {
      $(element).draggable()
      $(element).on('click', function (event, $index) {
        $scope.removeCard($scope.$index)
        $scope.$apply()
      })
    }
  }
})

myModule.directive("rider", function () {
  return {
    restrict:"E",
    link: function (scope, element, attrs) {
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
          // ui.draggable.trigger('droppedOn', self)
        }
      })
    }
  }
})