-module(mywf).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

delay(Time,Fun) ->
	wf:wire(me,
		#event{
			type=timer,
			delay=Time,
			delegate=?MODULE,
			postback={afterdelay,Fun}
		}
	).

event({afterdelay,Fun}) ->
	Fun(),
	ok.

flash() ->
	#panel{id=flash_div,body=""}.
flash(Msg) ->
	flash(warning,Msg).
flash(Type,Msg) ->
	flash(Type,Msg,infinity).
flash(Type,Msg,CloseTimeout) ->
	Class = case Type of
		error -> 	flash_div_error;
		warning -> 	flash_div_warning;
		good -> 	flash_div_good
	end,
	HideEvent = #event{target=flash_div,actions=#fade{}},
	wf:update(flash_div,#panel{actions=#show{target=flash_div,effect=bounce,speed=300},class=Class,body=[
		case CloseTimeout of
			infinity ->
				#link{id=flash_div_closer,class=flash_div_closer,text="[Close]",actions=HideEvent#event{type=click}};
			_ -> ""
		end,
		Msg
	]}),
	case CloseTimeout of
		infinity ->
			"";
		MS when is_integer(MS) ->
			wf:wire(flash_div,#event{delay=MS,type=timer,actions=#fade{}})
	end.

dollars("") -> [];
dollars(N) ->
	[$$ | ppb:dollars(N)].

%% L is expected to be a list in the format [ {ID,Val}, {ID,Val}...] or [ [ID,Val],[ID,Val]...]
%% That is, either a list of key,value tuples or key,value lists
format_options(L) ->
	[format_option(R) || R<-L].


format_option([ID,Val]) ->
	format_option({ID,Val});
format_option({ID,Val}) ->
	#option{text=wf:to_list(Val),value=wf:to_list(ID)};
format_option(V) ->
	NewV = wf:to_list(V),
	format_option({NewV,NewV}).

open_lightbox(Body) ->
	close_lightbox(),
	wf:insert_top("body",#lightbox{id=lightbox,body=[
		#panel{class=lightbox_form,body=Body}
	]}),
    wf:defer("if($('.lightbox').css('position')=='absolute') {
                var t = $(document).scrollTop() + 'px';
                $('.lightbox').css('top', t);
             }").


delete_link(Postback) ->
	#link{postback=Postback,body=[
		#image{image="/images/delete.png"}
	]}.

close_lightbox() ->
	wf:remove(lightbox).

close_lightbox_event() ->
	#event{type=click,actions=[
		#script{script="$(obj('lightbox')).remove()"}
	]}.

confirm_button_id(N) ->
    wf:to_atom("confirm_button_" ++ wf:to_list(N)).

confirm_button(N, {Text,Postback}) ->
	#button{id=confirm_button_id(N), text=Text,postback=Postback};
confirm_button(N, {Text,Delegate,Postback}) ->
	#button{id=confirm_button_id(N), text=Text,postback=Postback,delegate=Delegate}.

% buttons is a list of the form: 
% [{"Button Text",Postback},...] or [{"Button Text",Delegate,Postback},...]
confirm(Question,Buttons,CancelText,Style) ->
	Body = [
		#panel{class=confirm_question,style=Style,body=Question},
		#panel{class=confirm_buttonrow,body=[
			sigma:nmap(fun(N, Button) ->
                confirm_button(N, Button)
            end, Buttons),
			#button{class=cancel,text=CancelText,actions=close_lightbox_event()}
		]}
	],
	open_lightbox(Body).

prompt(Question,DefaultText,Buttons,CancelText) ->
	ConfirmQuestion = [
		#panel{class=confirm_question,body=Question},
		#br{},#br{},
		#textbox{class=confirm_question,id=prompt_text,text=DefaultText}
	],
	confirm(ConfirmQuestion,Buttons,CancelText).

prompt(Question,Buttons,CancelText) ->
	prompt(Question,"",Buttons,CancelText).


confirm(Question, Buttons, CancelText) ->
	confirm(Question, Buttons, CancelText, "").

confirm(Question,Buttons) ->
	confirm(Question,Buttons,"Cancel").

scroll_to_validation() ->
    JS = "	if(typeof scrolltimer == 'undefined')
            {
                scrolltimer=setTimeout(function() {
                    scrolltimer = undefined;
                    $.scrollTo('.LV_validation_message',400);
                },100);
            }",
    wf:defer(JS).

required(_Tag,Value) ->
	case sigma:trim(Value) of
		"" -> 
            scroll_to_validation(),
			false;
		_ -> true
	end.

is_valid_date(_Tag, "") ->
    % Allow blanks
    true;
is_valid_date(_Tag, Value) ->
    wf:info("Checking Date: ~p", [Value]),
    try qdate:to_unixtime(Value) of
        0 ->
            scroll_to_validation(),
            false;
        _ ->
            true
    catch _:_ ->
        scroll_to_validation(),
        false
    end.

is_email(Tag, Value) ->
    case validator_is_email:validate(Tag, Value) of
        true -> true;
        false ->
            scroll_to_validation(),
            false
    end.

is_email_blank_okay(_Tag, "") ->
    true;
is_email_blank_okay(Tag, Value) ->
    is_email(Tag, Value).


q_int(F) ->
	try wf:to_integer(wf:q(F)) of
		V -> V
	catch 
		_:_ -> undefined
	end.

text_coalesce(List) ->
	text_coalesce(List,[]).

text_coalesce([],_) ->
	[];
text_coalesce([<<>> | T],Options) ->
	text_coalesce(T,Options);
text_coalesce(["" | T],Options) ->
	text_coalesce(T,Options);
text_coalesce([undefined | T],Options) ->
	text_coalesce(T,Options);
text_coalesce([H | T],Options) when is_list(Options) ->
	case lists:member(allow_all_whitespace,Options) of
		true -> H;
		false ->
			case has_non_whitespace(H) of
				true -> H;
				false -> text_coalesce(T)
			end
	end.

has_non_whitespace([]) ->
	false;
has_non_whitespace([WS | T]) when WS==10;WS==13;WS==32;WS==9 ->
	has_non_whitespace(T);
has_non_whitespace(_) ->
	true.

strip_leading_and_trailing_whitespace(Text) ->
	Text1 = strip_leading_whitespace(Text),
	Text2 = strip_leading_whitespace(lists:reverse(Text1)),
	lists:reverse(Text2).


%% strips space, carriage return, linefeed, and tabs from the front of a string
strip_leading_whitespace([]) ->
	[];
strip_leading_whitespace([WS | T]) when WS==10; WS==13; WS==32; WS==9 ->
	strip_leading_whitespace(T);
strip_leading_whitespace(Text) ->
	Text.

q_maps(KeyList) ->
    PLs = q_pls(KeyList),
    [maps:from_list(PL) || PL <- PLs].

q_pls(KeyList) ->
	Initial = wf:mqs(KeyList),
	Pivoted = sigma:pivot(Initial),
	lists:map(fun(Row) ->
		lists:zip(KeyList, Row)
	end, Pivoted).
