-record(fgame, { players, deck, deal_stage = rider, deal_player=1, deal_stack=1, hit_count=0, hit_max=1}).
% Ids have to be from 1 to max number of players
-record(fplayer, { id, rider=none, extras = 0, stacks=[], hand=none, dead=false, stack_id=1 }).

% Ids have to be from 1 to max number of stacks
-record(fstack, {id, cards=[]}).
