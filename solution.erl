-module(solution).
-export([subs/1, perms/1, choice/1, split/1, exprs/1, solutions/2, solutions2/2]).
-compile(export_all).

-type op() :: add | sub | mul | divi.
-type expr() :: {val, integer()} | {app, op(), expr(), expr()}.

-spec valid(op(), integer(), integer()) -> integer().
%valid(add, _I1, _I2) -> true;
valid(add, I1, I2) -> I1 =< I2;
valid(sub, I1, I2) -> I1 > I2;
%valid(mul, _I1, _I2) -> true;
valid(mul, I1, I2) ->  (I1 =< I2) and (I1 =/= 1) and (I2 =/= 1);
%valid(divi, I1, I2) -> I1 rem I2 =:= 0.
valid(divi, I1, I2) -> (I1 rem I2 =:= 0) and (I2 =/= I1).

-spec  apply_op(op(), integer(), integer()) -> integer().
apply_op(add, I1, I2) -> I1 + I2;
apply_op(sub, I1, I2) -> I1  - I2;
apply_op(mul, I1, I2) -> I1 * I2;
apply_op(divi, I1, I2) -> I1 div I2.


%% solution:is_elment({val, 3}, {1,2,3},3). 
%% solution:is_elment({app, add, {val, 1}, {val, 2}}, [1,2,3], 3).

-spec subs([A]) ->[[A]].
subs([]) -> [[]];
subs([H | T]) ->
	YSS = subs(T),
	YSS ++ [[H] ++ YS || YS <- YSS].

%-spec perms([A]) -> [[A]].
%perms([]) -> [[]];
%perms(L) ->
%	[[I | XS] || I <- L, XS <- perms(L -- [I])].

-spec interleave(integer(), [A]) -> [[A]].
interleave(X, []) -> [[X]];
interleave(X, [H | T]) ->
	[ [X, H | T]|[[H | I] || I <-interleave(X, T) ]].

perms([]) -> [[]];
perms([H|T]) ->
	lists:append([interleave(H, XS) || XS <-perms(T)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec choice([A]) -> [[A]].
choice(L) -> 
	[E || S<-subs(L), E <- perms(S) ].

%%-spec is_elment(expr(), [integer()], integer()) -> boolean().
%%is_elment({val,  I1}, List, Target) -> hd([X || X <- List, X =:= I1]) =:= Target;
%%is_elment({app,  add, {val,  I1}, {val,  I2}}, List, Target) -> apply_op(add, I1, I2) =:= Target;
%%is_elment({app,  sub, {val,  I1}, {val,  I2}}, List, Target) -> apply_op(sub, I1, I2) =:= Target;
%%is_elment({app,  mul, {val,  I1}, {val,  I2}}, List, Target) -> apply_op(mul, I1, I2) =:= Target;
%%is_elment({app,  divi, {val,  I1}, {val,  I2}}, List, Target) -> apply_op(divi, I1, I2) =:= Target.

-spec values (expr()) -> [integer()].
values({val, N}) -> [N];
values({app, _Op, L, R}) -> values(L) ++ values(R).

-spec solution(expr(), [integer()], integer()) -> boolean().
solution(E, NS, N) ->
	(eval(E) =:= [N]) and lists:member(values(E), choice(NS)).

-spec eval(expr()) -> [integer()].
%eval({app,  add, {val,  I1}, {val,  I2}}) -> apply_op(add, I1, I2) ;
%eval({app,  sub, {val,  I1}, {val,  I2}}) -> apply_op(sub, I1, I2) ;
%eval({app,  mul, {val,  I1}, {val,  I2}}) -> apply_op(mul, I1, I2);
%eval({app,  divi, {val,  I1}, {val,  I2}}) -> apply_op(divi, I1, I2) .
eval({val, I}) -> [I || I > 0];
eval({app, Op, Expr1, Expr2}) ->
	[apply_op(Op, V1, V2) || V1 <- eval(Expr1), V2 <- eval(Expr2), valid(Op, V1, V2)].

%% solutions([1,2], 3) ->
%%	[{app, add, {val, 1}, {val, 2}},
%%	{app, add, {val, 2}, {val, 1}}]

-spec solutions([integer()], integer()) -> [expr()].
solutions(NS, N) -> [E || NS2 <-choice(NS),
		       E <- exprs(NS2),
		       eval(E) =:= [N]].

-spec split([A]) -> [{[A], [A]}].
split([]) -> [];
split([_]) -> [];
split([H|T]) ->
	%R1 = [{[H], T}],
	%[H1|T1] = T, 
	%R2 = [{[H,H1], T1}],
	[{[H], T}] ++ [{[H|H1], T1} || {H1, T1} <-split(T)].

%% solutions([1,2], 3) ->
%%	[{app, add, {val, 1}, {val, 2}},
%%	{app, add, {val, 2}, {val, 1}}]
-spec exprs([integer()]) -> [expr()].
exprs([]) -> [];
exprs([I]) -> [{val, I}];
exprs(NS) -> 
	[E || {LS, RS} <- split(NS),
	        L <- exprs(LS),
	        R <- exprs(RS),
	        E <- combine(L, R)].
	
-spec ops() -> [op()].
ops()  -> [add, sub, mul, divi].

-spec combine(expr(), expr()) -> [expr()].
combine(L, R) -> [{app, O, L, R} || O <-ops()].
%split([1,2,3,4]) ->
%	[{[1],[2,3,4]}, {[1,2], [3,4]}, {[1,2,3],[4]}]

-type result() :: {expr(), integer()}.
-spec results([integer()]) -> [result()].
results([]) -> [];
results([I]) -> [{{val, I}, I} || I > 0];
results(NS) -> [Res || {LS, RS} <- split(NS),
		LX <- results(LS),
		RY <- results(RS),
		Res <- combine2(LX, RY)].
-spec combine2(result(), result()) -> [result()].
combine2({EL, X}, {ER, Y}) ->
	[{{app, O, EL, ER}, apply_op(O, X, Y)}
	|| O <- ops(),
	valid(O, X, Y)].

-spec solutions2([integer()], integer()) -> [expr()].
solutions2(NS, N) -> [E || NS2 <- choice(NS),
		{E, M} <- results(NS2),
		M =:= N].
		