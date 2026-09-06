-module(lsp_codeaction).

-export([code_actions/3, resolve/1]).

-include("lsp_log.hrl").

%% @doc Task 2.1: infrastructure only, deliberately empty.
%%
%% No quick fix is registered yet - task 2.2+ will pattern-match on each
%% diagnostic's `correlation_data` (lsp_syntax:correlation_data/1) to decide
%% what to offer here. Until then this always returns no actions, so the
%% client shows no lightbulb - that is the correct, honest behaviour for
%% "codeActionProvider advertised, nothing implemented yet".
-spec code_actions(File :: file:filename(), Range :: term(), Context :: map()) -> [map()].
code_actions(_File, _Range, _Context) ->
    [].

%% @doc Resolve a lazily-computed CodeAction (codeActionProvider's
%% resolveProvider => true means the initial code_actions/3 result can omit
%% `edit`, and the client asks for it here only for the one action the user
%% is about to apply). Since no fix populates a `data` field yet, this is
%% the identity function - task 2.2+ will pattern-match on `data` to build
%% the real edit at this point, via lsp_rename:build_workspace_edit/1.
-spec resolve(CodeAction :: map()) -> map().
resolve(CodeAction) ->
    CodeAction.
