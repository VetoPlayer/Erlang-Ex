-module(trees).
-export([empty/0, insert/3, lookup/2]).

%% To represent the nodes of a tree we can use a tagged tuple.
%% {node, {Key, Value, Smaller, Larger}},
%% whereas Smaller and Larger can be any other similar node or an empty one {node, nil}.


%% empty returns an empty node
empty() -> {node, 'nil'}.

insert(Key, Value, {node, 'nil'}) ->
        {node, {Key, Value, {node,'nil'}, {node, 'nil'}}};
insert(NewKey, NeWValue,{node})