-module(server_users).

-export([]).

initialize() ->
  	initialize_with(dict:new()). 

% Start server with an initial state.
% Useful for benchmarking.
initialize_with(Users) ->
	ServerPid = spawn_link(?MODULE, server_actor, [Users]),
    catch unregister(server_actor),
    register(server_actor, ServerPid),
    ServerPid.

server_actor(Users) ->
    receive
        {Sender, register_user, UserName} ->
            NewUsers = dict:store(UserName, create_user(UserName), Users),
            Sender ! {self(), user_registered},
            server_actor(NewUsers);

        {Sender, log_in, UserName} ->
            Sender ! {self(), logged_in, get_user(Users, UserName)},
            server_actor(Users);

        {Sender, update_user, UserName, NewlyFollowed, NewlyUnfollowed} ->
            NewUsers = update_followers(Users, UserName, NewlyFollowed, NewlyUnfollowed),
            Sender ! {self(), user_updated},
            server_actor(NewUsers)
    end.

	create_user(UserName) ->
		{user, sets:new()}.

	get_user(Users, UserName) ->
		case dict:find(UserName, Users) of
			{ok, User} -> User;
			error -> throw({user_not_found, UserName})
		end.

	update_followers(Users, UserName, NewlyFollowed, NewlyUnfollowed) ->
		dict:update(UserName, fun({user, Following}) -> 
			{user,sets:union(NewlyFollowed, sets:subtract(Following,NewlyUnfollowed))} 
		end, Users).