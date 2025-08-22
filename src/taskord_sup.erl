%%%-------------------------------------------------------------------
%% @doc taskord top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(taskord_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    
    % Define Cowboy HTTP listener as a child spec
    CowboySpec = #{
        id => cowboy_http,
        start => {cowboy, start_clear, [
            http_listener,
            [{port, 2025}],
            #{env => #{dispatch => get_dispatch()}}
        ]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [cowboy]
    },
    
    ChildSpecs = [CowboySpec],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
get_dispatch() ->
    cowboy_router:compile([
        {'_', [
            {"/api/orders", taskord_rest_orderer, []}
        ]}
    ]).