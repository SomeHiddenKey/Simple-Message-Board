-module(server_messages).

-export([initialize/0, initialize_with/1, server_actor/1]).

initialize() ->
    initialize_with(dict:new()).

initialize_with(MessagesDatabase) ->
	ServerPid = spawn_link(?MODULE, server_actor, [MessagesDatabase]),
	catch unregister(server_messages),
	register(server_messages, ServerPid),
	ServerPid.

server_actor(MessagesDatabase) ->
	receive
		{Sender, send_message, UserName, MessageText, Timestamp} ->
			UpdatedMessagesDatabase = store_message(MessagesDatabase, {message, UserName, MessageText, Timestamp}),
			Sender ! {self(), message_sent},
			server_actor(UpdatedMessagesDatabase);

		{Sender, get_timeline, [UserNames]} ->
			Sender ! {self(), timeline, get_messageBunch(MessagesDatabase, UserNames)},
			server_actor(MessagesDatabase);

		{Sender, get_messages, UserName} ->
			case get_messages(MessagesDatabase, UserName) of
				{ok, Messages} -> Sender ! {self(), profile, Messages};
				error -> Sender ! {self(), profile, []}
			end,
			server_actor(MessagesDatabase)
	end.

store_message(MessagesDatabase, {message, UserName, MessageText, Timestamp}) ->
	dict:update(
		UserName, 
		fun(Messages) -> [{UserName, MessageText, Timestamp} | Messages] end, 
		[{UserName, MessageText, Timestamp}], 
		MessagesDatabase).

get_messageBunch(MessagesDatabase, UserNames) ->
	lists:foldl(fun(FollowedUserName, Acc) -> 
		case get_messages(MessagesDatabase, FollowedUserName) of
			{ok, Messages} -> [Messages | Acc];
			error -> Acc
		end
		end, [], UserNames).

get_messages(MessagesDatabase, UserName) -> dict:find(MessagesDatabase, UserName).