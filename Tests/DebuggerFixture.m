(* ::Package:: *)

(* Functions exercised by DebuggerTests.wlt. The tests assert line numbers in this file: append, do not insert. *)

fixtureOuter[x_Integer]:=Module[
	{total},
	total = x + 1;
	total = fixtureInner[total];
	total
];

fixtureInner[y_Integer]:=Module[
	{total},
	total = y * 2;
	total
];

fixturePure[list_List]:=Map[Function[{element}, element + 1], list];

fixtureWith[a_]:=With[{doubled = 2 a}, doubled + 1];

fixtureConditionTail[a_Integer]:=With[{half = a / 2}, half /; IntegerQ[half]];
fixtureConditionTail[a_]:="odd";

Options[fixtureOptions] = {"Scale" -> 3};
fixtureOptions[a_, ops:OptionsPattern[]]:=a * OptionValue["Scale"];

SetAttributes[fixtureHeld, HoldAll];
fixtureHeld[expression_]:=Length[Unevaluated[expression]];

fixtureSequence[first_, rest___]:={first, Length[{rest}]};

fixtureOverload[samples_List]:=Module[{result}, result = Length[samples]; result];
fixtureOverload[sample_String]:=Module[{result}, result = StringLength[sample]; result];

fixtureMessage::oops = "Oops.";
fixtureMessage[x_]:=Module[{before}, before = x; Message[fixtureMessage::oops]; before];

fixtureReturn[x_]:=Module[{value}, value = If[x > 0, Return["early"], x]; "late"];

fixtureInitializer[x_]:=Module[{first = x + 1, second = x + 2}, first + second];

fixtureAbort[x_]:=Module[{reached}, reached = x; Abort[]; reached];

fixtureMutate[x_]:=Module[{list = {}, count = 0, assoc = <||>},
	AppendTo[list, x];
	count += 2;
	count++;
	list[[1]] = 10;
	assoc["k"] = 1;
	AssociateTo[assoc, "j" -> 2];
	{list, count, assoc}
];
