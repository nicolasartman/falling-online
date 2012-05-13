-record(fgame, { players, deck, deal_stage = rider, deal_player=1, deal_stack=1, hit_count=0, hit_max=1}).

-record(fplayer, { id, rider=none, extras = 0, stacks=[], hand=none, dead=false, stack_id=1 }).

-record(fstack, {id, cards=[]}).
