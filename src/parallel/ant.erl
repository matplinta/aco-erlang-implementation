-module (ant).
% -export ([ant/4]).

start(FoodList) ->
spawn(?MODULE, fridge2, [FoodList]).