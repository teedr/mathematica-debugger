(* ::Package:: *)

(*
	Run with:
		TestReport[FileNameJoin[{$HomeDirectory, "mathematica-debugger", "Tests", "DebuggerTests.wlt"}]]
	Loads Debugger.m from this checkout and the fixture functions into the Fixture` context.
	Line numbers asserted below are lines of DebuggerFixture.m.
*)

VerificationTest[
	$debuggerTestDirectory = If[And[StringQ[$InputFileName], $InputFileName =!= ""],
		DirectoryName[$InputFileName],
		FileNameJoin[{$HomeDirectory, "mathematica-debugger", "Tests"}]
	];
	Get[FileNameJoin[{ParentDirectory[$debuggerTestDirectory], "Debugger", "Debugger.m"}]];
	(* the abort test protects a fixture; a rerun in the same kernel must be able to redefine it *)
	Quiet[Unprotect["Fixture`*"]];
	Block[{$Context = "Fixture`", $ContextPath = {"System`"}},
		Get[FileNameJoin[{$debuggerTestDirectory, "DebuggerFixture.m"}]]
	];
	Debugger`$DebuggerSourceFiles = {FileNameJoin[{$debuggerTestDirectory, "DebuggerFixture.m"}]};
	Fixture`fixtureOuter[3],
	8,
	TestID -> "Setup: package and fixture load"
]

VerificationTest[
	$fixtureOptions = Sequence[DebuggerContexts -> {"Fixture`"}, TrackInputs -> True, TrackLocations -> True];
	{
		Debugger[Fixture`fixtureOuter[3], DebuggerContexts -> {"Fixture`"}],
		Union[Map[Length, DebuggerInformation["AssignmentLog"]]],
		DebuggerInformation["AssignmentLog"][[1, 4]]
	},
	{8, {4}, Missing["NotTracked"]},
	TestID -> "Untracked runs log as before, with a Missing location"
]

VerificationTest[
	Debugger[Fixture`fixtureOuter[3], $fixtureOptions];
	Lookup[DebuggerInformation["CurrentAssignments"], {"Fixture`x", "Fixture`y"}],
	{Hold[3], Hold[4]},
	TestID -> "Pattern inputs are logged"
]

VerificationTest[
	Debugger[Fixture`fixtureOuter[3], $fixtureOptions];
	Lookup[DebuggerWhere["Fixture`total"], "Line"],
	{7, 14, 8},
	TestID -> "Same variable in two functions resolves to each assignment line"
]

VerificationTest[
	Lookup[DebuggerWhere["total"], "Function"],
	{"Fixture`fixtureOuter", "Fixture`fixtureInner", "Fixture`fixtureOuter"},
	TestID -> "Short names match and report the function"
]

VerificationTest[
	Lookup[DebuggerWhere["y"], "Line"],
	{12},
	TestID -> "Pattern input location is the definition line"
]

VerificationTest[
	DebuggerInputs["fixtureInner"],
	{<|"y" -> 4|>},
	TestID -> "DebuggerInputs groups inputs by call"
]

VerificationTest[
	Head[DebuggerHistory[5]],
	Grid,
	TestID -> "DebuggerHistory renders"
]

VerificationTest[
	With[{definitions = DownValues[Fixture`fixtureOuter]},
		Debugger[Fixture`fixtureOuter[3], $fixtureOptions];
		DownValues[Fixture`fixtureOuter] === definitions
	],
	True,
	TestID -> "Original definitions are restored after the run"
]

VerificationTest[
	Debugger[Fixture`fixtureOuter[3], $fixtureOptions, InstrumentSymbols -> {Fixture`fixtureInner}];
	KeyExistsQ[DebuggerInformation["CurrentAssignments"], #]& /@ {"Fixture`x", "Fixture`y"},
	{False, True},
	TestID -> "InstrumentSymbols restricts the rewritten functions"
]

VerificationTest[
	{
		Debugger[Fixture`fixturePure[{0, 1, 2}], $fixtureOptions],
		Cases[DebuggerInformation["AssignmentLog"], {_, "Fixture`element", Hold[value_], _} :> value],
		Union[Lookup[DebuggerWhere["element"], "Line"]]
	},
	{{1, 2, 3}, {0, 1, 2}, {18}},
	TestID -> "Function parameters are logged with their line"
]

VerificationTest[
	{
		Debugger[Fixture`fixtureWith[5], $fixtureOptions],
		DebuggerInformation["CurrentAssignments"]["Fixture`doubled"],
		Lookup[DebuggerWhere["doubled"], "Line"]
	},
	{11, Hold[10], {20}},
	TestID -> "With constants are logged with their line"
]

VerificationTest[
	{
		Debugger[Fixture`fixtureConditionTail[4], $fixtureOptions],
		Debugger[Fixture`fixtureConditionTail[3], $fixtureOptions]
	},
	{2, "odd"},
	TestID -> "With ... /; condition idiom still falls through"
]

VerificationTest[
	{
		Debugger[Fixture`fixtureOptions[2], $fixtureOptions],
		Debugger[Fixture`fixtureOptions[2, "Scale" -> 5], $fixtureOptions],
		DebuggerInformation["CurrentAssignments"]["Fixture`ops"]
	},
	{6, 10, Hold[{"Scale" -> 5}]},
	TestID -> "OptionValue still resolves; OptionsPattern inputs are logged as a list"
]

VerificationTest[
	{
		Debugger[Fixture`fixtureHeld[1 + 1], $fixtureOptions],
		DebuggerInformation["CurrentAssignments"]["Fixture`expression"]
	},
	{2, Hold[1 + 1]},
	TestID -> "HoldAll inputs are logged unevaluated"
]

VerificationTest[
	{
		Debugger[Fixture`fixtureSequence[1, 2, 3], $fixtureOptions],
		DebuggerInformation["CurrentAssignments"]["Fixture`rest"]
	},
	{{1, 2}, Hold[{2, 3}]},
	TestID -> "Sequence inputs are logged as a list"
]

VerificationTest[
	Debugger[Fixture`fixtureOverload["abc"], $fixtureOptions];
	Lookup[DebuggerWhere["result"], "Line"],
	{34},
	TestID -> "Overloads are told apart by pattern names (String overload)"
]

VerificationTest[
	Debugger[Fixture`fixtureOverload[{1, 2}], $fixtureOptions];
	Lookup[DebuggerWhere["result"], "Line"],
	{33},
	TestID -> "Overloads are told apart by pattern names (List overload)"
]

VerificationTest[
	{
		Debugger[Fixture`fixtureInitializer[1], $fixtureOptions],
		Lookup[DebuggerWhere["first"], "Line"],
		Lookup[DebuggerWhere["second"], "Line"]
	},
	{5, {41}, {41}},
	TestID -> "Module initializers are located"
]

VerificationTest[
	{
		Debugger[Fixture`fixtureReturn[1], $fixtureOptions],
		Debugger[Fixture`fixtureReturn[-1], $fixtureOptions]
	},
	{Fixture`fixtureReturn[1], Fixture`fixtureReturn[-1]},
	TestID -> "Return inside an assigned value behaves as without the Debugger"
]

VerificationTest[
	{
		Debugger[Fixture`fixtureMessage[7], $fixtureOptions],
		KeyTake[DebuggerWhere[], {"Variable", "Line"}]
	},
	{$Aborted, <|"Variable" -> "Fixture`before", "Line" -> 37|>},
	{Fixture`fixtureMessage::oops},
	TestID -> "After a message, DebuggerWhere[] points at the last tracked assignment"
]

VerificationTest[
	Debugger[Fixture`fixtureOuter[3], $fixtureOptions];
	{Head[DebuggerHistory["total"]], Head[DebuggerHistory[{1, 2}]]},
	{Grid, Grid},
	TestID -> "DebuggerHistory by variable name and by entries"
]

VerificationTest[
	Protect[Fixture`fixtureAbort];
	With[{definitions = DownValues[Fixture`fixtureAbort], attributes = Attributes[Fixture`fixtureAbort]},
		{
			CheckAbort[Debugger[Fixture`fixtureAbort[1], $fixtureOptions], "aborted"],
			DownValues[Fixture`fixtureAbort] === definitions,
			Attributes[Fixture`fixtureAbort] === attributes,
			DebuggerInformation["LastAssignment"]
		}
	],
	{"aborted", True, True, "Fixture`reached" -> Hold[1]},
	TestID -> "An abort mid-run (same unwinding as Alt+.) restores definitions and keeps the log"
]
