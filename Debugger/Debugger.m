BeginPackage["Debugger`"];
Needs["GeneralUtilities`"];

Debugger;
DebuggerInformation;
IgnoreContexts;
AbortOnMessage;

Begin["`Private`"];

ClearAll[DebuggerInformation];

Options[Debugger]:={
	IgnoreContexts -> {"System"},
	AbortOnMessage -> True
};
Debugger[codeBlock_,OptionsPattern[]]:=Module[
	{return,currentAssignments},
	
	ClearAll[DebuggerInformation];
	
	(* $$ prefix indicates that setting this variable should not trigger SetHandler *)
	$$assignments = Association[];
	$$messages = {};
	$$lastAssignment = Null;
	
	return = With[
		{
			ignoreContexts = OptionValue[IgnoreContexts],
			abortOnMessage = OptionValue[AbortOnMessage]
		},
		WithMessageHandler[
			WithSetHandler[
				codeBlock,
				setHandler[
					##,
					IgnoreContexts -> ignoreContexts
				]&
			],
			messageHandler[
				##,
				AbortOnMessage -> abortOnMessage
			]&
		]
	];
	
	currentAssignments = Map[
		Last,
		$$assignments
	];
	
	DebuggerInformation = Association[
		"AssignmentLog" -> $$assignments,
		"CurrentAssignments" -> currentAssignments,
		"LastAssignment" -> $$lastAssignment,
		"Failures" -> $$messages
	];
	
	If[MatchQ[return,$Aborted[]],
		Message/@$$messages
	];
	
	ClearAll[
		$$assignments,
		$$messages,
		$$lastAssignment
	];
	
	return
];
SetAttributes[Debugger,HoldFirst];

Options[setHandler]:={
	IgnoreContexts -> {}
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
setHandler[HoldComplete[$$variable_Symbol], HoldComplete[$$value_],OptionsPattern[]]:=With[
	{
		$$currentAssignments = Lookup[
			$$assignments,
			HoldForm[$$variable],
			{}
		]
	},
	If[
		MatchQ[
			First[StringSplit[Context[$$variable],"`"]],
			Except[
				Apply[
					Alternatives,
					OptionValue[IgnoreContexts]
				]
			]
		],
		AppendTo[
			$$assignments,
			HoldForm[$$variable] -> Append[
				$$currentAssignments,
				$$value
			]
		];
		$$lastAssignment = HoldForm[$$variable]
	]
];

Options[messageHandler]:={
	AbortOnMessage -> True
};
messageHandler[failure_,OptionsPattern[]]:=With[
	{},
	
	AppendTo[$$messages,failure];
	
	If[TrueQ[OptionValue[AbortOnMessage]],
		Abort[]
	];
];

(* ::Section::Closed:: *)
(*End Package*)


End[];
EndPackage[];