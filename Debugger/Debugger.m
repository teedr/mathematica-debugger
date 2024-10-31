(* ::Package:: *)

BeginPackage["Debugger`"];
Needs["GeneralUtilities`"];

Debugger;
DebuggerInformation;
DebuggerContexts;
AbortOnMessage;
BreakOnAssert;
ModuleNumbers;

$DebuggerContexts = {"Global`"};


Begin["`Private`"];

ClearAll[DebuggerInformation];

Options[Debugger]:={
	DebuggerContexts -> $DebuggerContexts,
	AbortOnMessage -> True,
	BreakOnAssert -> False,
	ModuleNumbers -> False
};

(* label we use for Goto/Label for early abort *)
debuggerEndLabel = "Debugger End "<>CreateUUID[];

Debugger[codeBlock_,OptionsPattern[]]:=Module[
	{runningReturn, reapReturn,return,sowedAssignments,sowedMessages},

	ClearAll[DebuggerInformation];

	(* Use Reap/Sow to optimize AssignmentLog appending *)
	reapReturn = Reap[
		With[
			{
				contextsRegex = Apply[
					Alternatives,
					StringRiffle[
						Map[
							StringJoin[#, ".*"]&,
							OptionValue[DebuggerContexts]
						],
						"|"
					]
				],
				abortOnMessage = OptionValue[AbortOnMessage],
				moduleNumbers = OptionValue[ModuleNumbers],
				returnSymbol = runningReturn
			},
			Block[
				{$AssertFunction},
				If[TrueQ[OptionValue[BreakOnAssert]],
					$AssertFunction = assertHandler
				];

				runningReturn = WithMessageHandler[
					(* If a message is Quieted, it wont be sent to the message handler
						However, if the message is called many times and triggers the
						General::stop message, General::stop is passed to the handler
						Quiet General::stop as a hacky way to handle this *)
					Quiet[
						WithSetHandler[
							codeBlock,
							setHandler[
								##,
								ContextsRegex -> contextsRegex,
								ModuleNumbers -> moduleNumbers
							]&
						],
						{General::stop, Unset::write}
					],
					messageHandler[
						##,
						AbortOnMessage -> abortOnMessage,
						ToReturn -> returnSymbol
					]&
				];
				(* we use label to force evaluation to stop earlier *)
				Label[debuggerEndLabel];
				runningReturn
			]
		],
		_,
		Rule
	];

	return = reapReturn[[1]];
	sowedAssignments = Lookup[
		reapReturn[[2]],
		"assignment",
		{}
	];
	sowedMessages = Lookup[
		reapReturn[[2]],
		"failure",
		{}
	];

	populateDebuggerInformation[sowedAssignments,sowedMessages];

	(* always print message so we know what is failing *)
	Message /@ sowedMessages;

	return
];
SetAttributes[Debugger,HoldFirst];

populateDebuggerInformation[assignments_List,failures_List]:=With[
	{
		currentAssignments = Association[
			Map[
				Function[
					variableName,
					variableName -> SelectFirst[
						Reverse[assignments],
						MatchQ[#[[2]],variableName]&,
						{Null,Null,Null}
					][[3]]
				],
				DeleteDuplicates[assignments[[All,2]]]
			]
		],
		lastAssignment = If[
			Length[assignments] === 0,
			Null,
			Apply[
				Rule,
				(* Last now accepts second argument as default return *)
				Last[assignments,{Null,Null,Null}][[{2,3}]]
			]
		]
	},
	DebuggerInformation = Association[
		"AssignmentLog" -> assignments,
		"CurrentAssignments" -> currentAssignments,
		"LastAssignment" -> lastAssignment,
		"Failures" -> failures
	]
];

Options[setHandler]:={
	ContextsRegex -> "*",
	ModuleNumbers -> False
};
setHandler[heldVars:HoldComplete[_List], values:HoldComplete[_],ops:OptionsPattern[]]:=With[
	{},
	Map[
		Apply[setHandler[##,ops]&],
		Transpose[
			{
				Thread[heldVars],
				Thread[values]
			}
		]
	]
];
setHandler[HoldComplete[$$variable_Symbol], HoldComplete[$$value_],ops:OptionsPattern[]]:=Module[
	{$$symbolString},

	$$symbolString = If[TrueQ[OptionValue[ModuleNumbers]],
		ToString[HoldForm[$$variable]],
		StringReplace[
			ToString[HoldForm[$$variable]],
			RegularExpression["\\$[0-9]+"] -> ""
		]
	];

	If[
		StringMatchQ[
			Context[$$variable],
			RegularExpression[OptionValue[ContextsRegex]]
		],
		Sow[{UnixTime[],$$symbolString,Hold[$$value]},"assignment"]
	]
];

Options[messageHandler]:={
	AbortOnMessage -> True,
	ToReturn -> mySymbol
};
messageHandler[failure_,OptionsPattern[]]:=With[
	{},

	Sow[failure,"failure"];

	If[TrueQ[OptionValue[AbortOnMessage]],
		(* set the returned value to be $Failed, then use Goto to abort early *)
		(* Set HoldFirst, so we have use with *)
		With[{return = OptionValue[ToReturn]},
			Set[return, $Aborted]
		];
		Goto[debuggerEndLabel]
	];
];

assertHandler[HoldComplete[Assert[_,assertionLocation_List]]]:=With[
	{},

	PrintTemporary[
		StringJoin[
			"Breakpoint at ",
			"line: ",
			ToString[First[assertionLocation]],
			" file: ",
			Last[assertionLocation]
		]
	];

	Interrupt[];
];
assertHandler[Assert[HoldComplete[___]]]:=With[
	{},
	PrintTemporary["Breakpoint at unknown location"];

	Interrupt[];
];

(*LastDebuggerAssignments*)

(*after releasing the debugger, can see the last set of variables that were assigned*)
LastDebuggerAssignments[]:=LastDebuggerAssignments[20];
LastDebuggerAssignments[x_Integer]:=Part["AssignmentLog" /. DebuggerInformation[], -x;;, 2];
LastDebuggerAssignments[x:All]:=Part["AssignmentLog" /. DebuggerInformation[], All, 2];


(* ::Section::Closed:: *)
(*End Package*)


End[];
EndPackage[];
