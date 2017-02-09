BeginPackage["Debugger`"];
Needs["GeneralUtilities`"];

Debugger;
DebuggerInformation;
DebuggerContexts;
AbortOnMessage;
BreakOnAssert;

$DebuggerContexts = {"Global`"};

Begin["`Private`"];

ClearAll[DebuggerInformation];

Options[Debugger]:={
	DebuggerContexts :> $DebuggerContexts,
	AbortOnMessage -> True,
	BreakOnAssert -> False
};
Debugger[codeBlock_,OptionsPattern[]]:=Module[
	{return},
	
	ClearAll[DebuggerInformation];
	
	(* $$ prefix indicates that setting this variable should not trigger SetHandler *)
	$$assignments = Association[];
	$$messages = {};
	$$lastAssignment = Null;
	
	return = With[
		{
			contexts = OptionValue[DebuggerContexts],
			abortOnMessage = OptionValue[AbortOnMessage]
		},
		Block[
			{$AssertFunction},
			If[TrueQ[OptionValue[BreakOnAssert]],
				$AssertFunction = assertHandler
			];
			WithMessageHandler[
				(* If a message is Quieted, it wont be sent to the message handler
					However, if the message is called many times and triggers the 
					General::stop message, General::stop is passed to the handler
					Quiet General::stop as a hacky way to handle this *)
				Quiet[
					WithSetHandler[
						codeBlock,
						setHandler[
							##,
							DebuggerContexts -> contexts
						]&
					],
					{General::stop}
				],
				messageHandler[
					##,
					AbortOnMessage -> abortOnMessage
				]&
			]
		]
	];
	
	populateDebuggerInformation[];
	
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

populateDebuggerInformation[]:=With[
	{
		currentAssignments = Map[
			SafeLast,
			$$assignments
		]
	},
	DebuggerInformation = Association[
		"AssignmentLog" -> $$assignments,
		"CurrentAssignments" -> currentAssignments,
		"LastAssignment" -> $$lastAssignment,
		"Failures" -> $$messages
	]
];

Options[setHandler]:={
	DebuggerContexts -> {}
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
			ToString[HoldForm[$$variable]],
			{}
		],
		$$symbolString = ToString[HoldForm[$$variable]]
	},
	If[
		StringMatchQ[
			Context[$$variable],
			Apply[
				Alternatives,
				OptionValue[DebuggerContexts]
			]
		],
		AppendTo[
			$$assignments,
			$$symbolString -> Append[
				$$currentAssignments,
				$$value
			]
		];
		$$lastAssignment = $$symbolString
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
	
	populateDebuggerInformation[];	
	Interrupt[];
];
assertHandler[Assert[HoldComplete[___]]]:=With[
	{},
	PrintTemporary["Breakpoint at unknown location"];
	populateDebuggerInformation[];
	Interrupt[];
];

(* ::Section::Closed:: *)
(*End Package*)

End[];
EndPackage[];