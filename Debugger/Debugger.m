(* ::Package:: *)

BeginPackage["Debugger`"];
Needs["GeneralUtilities`"];

Debugger;
DebuggerInformation;
DebuggerContexts;
AbortOnMessage;
BreakOnAssert;
ModuleNumbers;
TrackInputs;
TrackLocations;
InstrumentSymbols;
DebuggerWhere;
DebuggerHistory;
DebuggerInputs;
DebuggerLocation;
LastDebuggerAssignments;
ClearDebuggerCache;
$DebuggerSourceFiles;
$DebuggerLinkTemplate;

TrackInputs::usage = "TrackInputs is an option for Debugger. When True, values bound without Set are logged like assignments: function pattern inputs (f[x_] := ...), Function parameters and With constants.";
TrackLocations::usage = "TrackLocations is an option for Debugger. When True, every logged assignment remembers the function and source line that made it. Query it with DebuggerWhere or DebuggerHistory.";
InstrumentSymbols::usage = "InstrumentSymbols is an option for Debugger giving the functions rewritten for TrackInputs and TrackLocations. Automatic uses every function in DebuggerContexts plus the public functions of those packages.";
DebuggerWhere::usage = "DebuggerWhere[] gives the source location of the last tracked assignment of the last Debugger run. DebuggerWhere[n] gives it for entry n of the AssignmentLog (negative n counts from the end). DebuggerWhere[\"var\"] gives every assignment of var with its location.";
DebuggerHistory::usage = "DebuggerHistory[n] shows the last n assignments of the last Debugger run with the function, file and line that made each one. DebuggerHistory[\"var\"] shows every assignment of var; DebuggerHistory[{i, ...}] shows those AssignmentLog entries.";
DebuggerInputs::usage = "DebuggerInputs[f] gives the pattern inputs of every call to f during the last Debugger run, one association per call. Requires TrackInputs -> True.";
DebuggerLocation::usage = "DebuggerLocation[id] is the source tag stored as the fourth element of AssignmentLog entries. DebuggerWhere resolves it to a file and line.";
LastDebuggerAssignments::usage = "LastDebuggerAssignments[n] gives the names of the last n assigned variables (default 20; All for every one).";
ClearDebuggerCache::usage = "ClearDebuggerCache[] drops the cached instrumented definitions and source-file indexes.";
$DebuggerSourceFiles::usage = "$DebuggerSourceFiles gives the source files searched to resolve locations. Automatic asks Packager when it is loaded. It may also be a list of files, or a function taking the full name of a function and returning its files.";
$DebuggerLinkTemplate::usage = "$DebuggerLinkTemplate is a StringTemplate string with File and Line slots used to hyperlink locations, e.g. a vscode://file/ URL ending in the File slot, a colon and the Line slot. None disables links.";

$DebuggerContexts = {"Global`"};
$DebuggerSourceFiles = Automatic;
$DebuggerLinkTemplate = None;

Begin["`Private`"];

ClearAll[DebuggerInformation];

Options[Debugger] = {
	DebuggerContexts :> $DebuggerContexts,
	AbortOnMessage -> True,
	BreakOnAssert -> False,
	ModuleNumbers -> False,
	TrackInputs -> False,
	TrackLocations -> False,
	InstrumentSymbols -> Automatic
};
Debugger[codeBlock_,OptionsPattern[]]:=Module[
	{contextsRegex,settings,instrumentQ,reapReturn,failures,userAborted = False},

	ClearAll[DebuggerInformation];

	contextsRegex = StringRiffle[
		Map[
			StringJoin[#,".*"]&,
			OptionValue[DebuggerContexts]
		],
		"|"
	];
	settings = Association[
		"ContextsRegex" -> contextsRegex,
		"TrackInputs" -> TrueQ[OptionValue[TrackInputs]],
		"TrackLocations" -> TrueQ[OptionValue[TrackLocations]]
	];
	instrumentQ = Or[settings["TrackInputs"],settings["TrackLocations"]];

	$rewriteFailures = {};

	(* Use Reap/Sow to optimize AssignmentLog appending
		CheckAbort sits inside Reap so an abort (Alt+.) that reaches this far still returns the log *)
	reapReturn = Reap[
		CheckAbort[
		With[
			{
				abortOnMessage = OptionValue[AbortOnMessage],
				moduleNumbers = OptionValue[ModuleNumbers]
			},
			Block[
				{$AssertFunction},
				If[TrueQ[OptionValue[BreakOnAssert]],
					$AssertFunction = assertHandler
				];
				Catch[
				WithMessageHandler[
					(* If a message is Quieted, it wont be sent to the message handler
						However, if the message is called many times and triggers the
						General::stop message, General::stop is passed to the handler
						Quiet General::stop as a hacky way to handle this *)
					Quiet[
						withInstrumentation[
							WithSetHandler[
								codeBlock,
								setHandler[
									##,
									ContextsRegex -> contextsRegex,
									ModuleNumbers -> moduleNumbers
								]&
							],
							(* Functions are only rewritten when inputs or locations were asked for. Choosing them scans
								whole packages, so it runs here, inside the abort guard, rather than before the run *)
							If[instrumentQ,
								instrumentationCandidates[OptionValue[DebuggerContexts],OptionValue[InstrumentSymbols]],
								{}
							],
							settings
						],
						{General::stop,Unset::write}
					],
					messageHandler[
						##,
						AbortOnMessage -> abortOnMessage
					]&
				],
					"DebuggerAbort"
			]]
		],
			userAborted = True;
			$Aborted
		],
		_,
		Rule
	];

	(* Stash the raw log first: it is instant and abort-proof, so extra Alt+. presses cannot lose it.
		Building DebuggerInformation can take a while on a big log; if another abort cuts it short,
		DebuggerInformation stays a delayed value that finishes the build the next time it is read *)
	AbortProtect[
		$lastRun = Association["Sowed" -> reapReturn[[2]],"InstrumentQ" -> instrumentQ];
		DebuggerInformation := buildDebuggerInformation[];
	];
	DebuggerInformation;

	failures = Lookup[reapReturn[[2]],"failure",{}];

	(* Throw messages *)
	Message/@failures;

	If[$rewriteFailures =!= {},
		Print[
			Row[{
				Style["Debugger: ",Bold],
				"could not instrument ",Length[$rewriteFailures]," function(s); they ran untracked: ",
				Short[Map[shortName,DeleteDuplicates[$rewriteFailures]],1]
			}]
		]
	];

	Which[
		(* Say where the run was, then keep the abort going so the call still ends as aborted *)
		userAborted,
			printLastLocation["aborted"];
			Abort[],
		And[settings["TrackLocations"],Length[failures]>0],
			printLastLocation["message"]
	];

	reapReturn[[1]]
];
SetAttributes[Debugger,HoldFirst];

$lastRun = Association["Sowed" -> {},"InstrumentQ" -> False];

(* Every AssignmentLog entry is {time, variable, Hold[value], location} *)
buildDebuggerInformation[]:=With[
	{sowedAssignments = dropRepeatedReports[Lookup[$lastRun["Sowed"],"assignment",{}]]},
	populateDebuggerInformation[
		If[TrueQ[$lastRun["InstrumentQ"]],
			pairLocations[sowedAssignments],
			Map[Append[#,Missing["NotTracked"]]&,sowedAssignments]
		],
		Lookup[$lastRun["Sowed"],"failure",{}],
		Lookup[$lastRun["Sowed"],"messageStack",{}]
	]
];

(*
	The set handler can report one Set more than once (twice on 14.2). Duplicates are identical reports of the same
	runtime symbol (module number included) back to back, so collapse those and drop the raw name. Nested assignments
	of the same variable name are different Module symbols, and a tracked repeat has its marker in between, so both stay.
*)
dropRepeatedReports[items_List]:=Map[
	Replace[#,{time_,name_String,value_Hold,_String} :> {time,name,value}]&,
	Map[
		First,
		Split[items,And[MatchQ[#1,{_,_String,_Hold,_String}],#1 === #2]&]
	]
];

populateDebuggerInformation[assignments_List,failures_List,stacks_List]:=With[
	{
		(* Association keeps the last value of a repeated key at the position of its first assignment *)
		currentAssignments = Association[
			Map[
				#[[2]] -> #[[3]]&,
				assignments
			]
		],
		lastAssignment = If[
			Length[assignments] === 0,
			Null,
			Apply[
				Rule,
				Last[assignments][[{2,3}]]
			]
		]
	},
	DebuggerInformation = Association[
		"AssignmentLog" -> assignments,
		"CurrentAssignments" -> currentAssignments,
		"LastAssignment" -> lastAssignment,
		"Failures" -> failures,
		"MessageStacks" -> stacks
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
	{$$rawString,$$symbolString},

	$$rawString = ToString[HoldForm[$$variable]];
	$$symbolString = If[TrueQ[OptionValue[ModuleNumbers]],
		$$rawString,
		StringReplace[
			$$rawString,
			RegularExpression["\\$[0-9]+"] -> ""
		]
	];

	(* The raw name (module number included) is only used to drop duplicate reports, then removed *)
	If[
		StringMatchQ[
			Context[$$variable],
			RegularExpression[OptionValue[ContextsRegex]]
		],
		Sow[{UnixTime[],$$symbolString,Hold[$$value],$$rawString},"assignment"]
	]
];
(*
	Assignments into part of a variable: x[[i]] = v, and assoc["k"] = v when assoc holds an Association.
	Logged under the name x[[i]] / assoc["k"] with the assigned part's value. s[...] = v on a symbol without
	an Association value is a definition (e.g. memoization), not a variable, and is ignored.
*)
setHandler[HoldComplete[$$target:Verbatim[Part][$$base_Symbol,___]],HoldComplete[$$value_],ops:OptionsPattern[]]:=
	partSetHandler[HoldComplete[$$target],HoldComplete[$$base],HoldComplete[$$value],ops]/;OwnValues[$$base] =!= {};
setHandler[HoldComplete[$$target:($$base:Except[List|Part,_Symbol])[___]],HoldComplete[$$value_],ops:OptionsPattern[]]:=
	partSetHandler[HoldComplete[$$target],HoldComplete[$$base],HoldComplete[$$value],ops]/;MatchQ[OwnValues[$$base],{_ :> _Association}];

Options[partSetHandler]:=Options[setHandler];
partSetHandler[HoldComplete[$$target_],HoldComplete[$$base_Symbol],HoldComplete[$$value_],OptionsPattern[]]:=Module[
	{$$baseString,$$rawString,$$symbolString},

	If[StringMatchQ[Context[$$base],RegularExpression[OptionValue[ContextsRegex]]],
		$$baseString = ToString[Unevaluated[$$base],InputForm];
		(* the base keeps its context; contexts inside the part spec are dropped to keep the name readable *)
		$$rawString = StringJoin[
			$$baseString,
			StringReplace[
				StringDrop[ToString[Unevaluated[$$target],InputForm],StringLength[$$baseString]],
				RegularExpression["[A-Za-z$][A-Za-z0-9$]*`"] -> ""
			]
		];
		$$symbolString = If[TrueQ[OptionValue[ModuleNumbers]],
			$$rawString,
			StringReplace[$$rawString,RegularExpression["\\$[0-9]+"] -> ""]
		];
		Sow[{UnixTime[],$$symbolString,Hold[$$value],$$rawString},"assignment"]
	]
];

Options[messageHandler]:={
	AbortOnMessage -> True
};
messageHandler[failure_,OptionsPattern[]]:=With[
	{},

	Sow[failure,"failure"];
	Sow[Stack[],"messageStack"];

	If[TrueQ[OptionValue[AbortOnMessage]],
		(* We used to use Abort[] here but sometimes it would not actually abort, for reasons
		beyond our comprehension. Throw/Catch seems to be more robust I believe? *)
		Throw[$Aborted,"DebuggerAbort"]
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
(*Instrumentation*)


(*
	TrackInputs and TrackLocations rewrite the DownValues and SubValues of the functions under debug:
	- pattern inputs are logged from a Condition wrapped around each rule's left-hand side
	- Function parameters and With constants are logged at the top of their bodies
	- each Set's value is prefixed with a marker Sow that pairLocations matches to the assignment that follows it
	Rules are rewritten lazily, on a function's first call, and only live inside the
	Internal`InheritedBlock in withInstrumentation, so the original definitions come back when Debugger returns.
*)

$instrumentCache = Association[];
$rewriteFailures = {};
$locationTags = Association[];
$tagCounter = 0;
$fileIndexCache = Association[];
$fileLinesCache = Association[];
$sourceFilesCache = Association[];

ClearDebuggerCache[]:=(
	$instrumentCache = Association[];
	$fileIndexCache = Association[];
	$fileLinesCache = Association[];
	$sourceFilesCache = Association[];
);

(* Placeholder heads hold rebuilt Set/Function/With nodes unevaluated until the whole rule is rebuilt *)
SetAttributes[{setPH,functionPH,withPH,sequencePH,mutationPH},HoldAllComplete];
placeholderRules = {setPH -> Set, functionPH -> Function, withPH -> With, sequencePH -> CompoundExpression, mutationPH -> logMutation};

SetAttributes[{symbolString,symbolContext,fullName,variableInfo,sequencePatternQ},HoldAll];
symbolString[symbol_Symbol]:=ToString[HoldForm[symbol]];
symbolContext[symbol_Symbol]:=Context[symbol];
fullName[symbol_Symbol]:=StringJoin[Context[symbol],SymbolName[Unevaluated[symbol]]];

(* Module numbers and lexical-scoping renames ("x$123", "x$") are not part of a variable's identity *)
normalizeName[name_String]:=StringReplace[name,RegularExpression["\\$[0-9]*$"] -> ""];
shortName[name_String]:=StringReplace[normalizeName[name],RegularExpression["^.*`"] -> ""];
(* The variable a log name refers to: "x[[i]]" and "assoc[\"k\"]" belong to x and assoc *)
baseName[name_String]:=normalizeName[StringReplace[name,RegularExpression["\\[.*$"] -> ""]];


(* ::Subsection::Closed:: *)
(*Choosing and installing*)


(* Every function in the tracked contexts, plus the public functions of their packages *)
instrumentationCandidates[contexts_List,Automatic]:=Module[
	{trackedContexts,names},

	trackedContexts = Select[Contexts[],StringStartsQ[#,Apply[Alternatives,contexts]]&];
	names = DeleteDuplicates[
		Join[
			Flatten[Map[Names[#<>"*"]&,trackedContexts]],
			Flatten[Map[publicSymbolNames,contexts]]
		]
	];
	Select[heldSymbols[names],instrumentableQ]
];
instrumentationCandidates[_,symbols_List]:=Select[heldSymbols[symbols],instrumentableQ];
instrumentationCandidates[_,_]:={};

publicSymbolNames[context_String]:=With[
	{package = StringReplace[context,"Private`"~~EndOfString -> ""]},
	If[And[packagerQ[],MemberQ[$Packages,package]],
		Quiet[
			Module[{names,metadata},
				names = Symbol["Packager`PackageSymbols"][package];
				metadata = Symbol["Packager`PackageMetadata"][package];
				If[And[MatchQ[names,{___String}],AssociationQ[metadata],StringQ[Lookup[metadata,"Context"]]],
					Map[metadata["Context"]<>#&,names],
					{}
				]
			]
		],
		If[package === context,{},Names[package<>"*"]]
	]
];

packagerQ[]:=And[
	NameQ["Packager`PackageSymbols"],
	NameQ["Packager`PackageMetadata"],
	NameQ["Packager`FunctionPackage"],
	NameQ["Packager`PackageSources"]
];

(* Names that do not exist yet are skipped rather than created *)
heldSymbols[items_List]:=Cases[
	Map[
		Which[
			!StringQ[#],HoldComplete[#],
			NameQ[#],Quiet[ToExpression[#,InputForm,HoldComplete]],
			True,Nothing
		]&,
		items
	],
	HoldComplete[_Symbol]
];

instrumentableQ[HoldComplete[symbol_Symbol]]:=And[
	!StringStartsQ[Context[symbol],"Debugger`"|"GeneralUtilities`"|"System`"|"Internal`"],
	!MemberQ[Attributes[symbol],Locked],
	Or[DownValues[symbol] =!= {},SubValues[symbol] =!= {}]
];
instrumentableQ[_]:=False;

(* Every instrumented symbol is InheritedBlock-ed, so stubs and rewritten rules are reverted on exit *)
SetAttributes[withInstrumentation,HoldFirst];
withInstrumentation[body_,{},_]:=body;
withInstrumentation[body_,symbols:{__HoldComplete},settings_Association]:=Replace[
	Apply[Join,symbols],
	HoldComplete[syms___] :> Internal`InheritedBlock[
		{syms},
		Quiet[Scan[installInstrumentation[#,settings]&,symbols]];
		body
	]
];

cacheKey[name_String,settings_Association]:={
	name,
	settings["ContextsRegex"],
	settings["TrackInputs"],
	settings["TrackLocations"]
};

cacheValidQ[HoldComplete[symbol_Symbol],cached_Association]:=And[
	DownValues[symbol] === cached["OriginalDownValues"],
	SubValues[symbol] === cached["OriginalSubValues"]
];
cacheValidQ[_,_]:=False;

(* Reuse a cached rewrite when the definitions have not changed; otherwise rewrite on first call *)
installInstrumentation[held:HoldComplete[symbol_Symbol],settings_Association]:=Module[
	{cached},

	cached = Lookup[$instrumentCache,Key[cacheKey[fullName[symbol],settings]],None];
	Which[
		!cacheValidQ[held,cached],
			Unprotect[symbol];
			DownValues[symbol] = Prepend[DownValues[symbol],stubRule[held,settings]],
		TrueQ[cached["Unchanged"]],
			If[TrueQ[cached["Failed"]],
				$rewriteFailures = Append[$rewriteFailures,fullName[symbol]]
			],
		True,
			Unprotect[symbol];
			DownValues[symbol] = cached["DownValues"];
			If[cached["SubValues"] =!= cached["OriginalSubValues"],
				SubValues[symbol] = cached["SubValues"]
			]
	]
];

(* The stub rewrites the real rules, drops itself, and re-evaluates the call against the rewritten rules *)
stubRule[held:HoldComplete[symbol_Symbol],settings_Association]:=With[
	{heldSymbol = held,instrumentSettings = settings},
	HoldPattern[symbol[arguments___]] :> (instrumentNow[heldSymbol,instrumentSettings]; symbol[arguments])
];

instrumentNow[HoldComplete[symbol_Symbol],settings_Association]:=Quiet[
	Module[
		{name,originalDownValues,originalSubValues,newDownValues,newSubValues,failedQ},

		name = fullName[symbol];
		originalDownValues = DeleteCases[
			DownValues[symbol],
			Verbatim[RuleDelayed][_,Verbatim[CompoundExpression][_instrumentNow,_]]
		];
		originalSubValues = SubValues[symbol];

		(* Any message while rewriting means the original rules are used, except informational ones (a built-in creating a
			symbol on first use). The list is written out because Quiet holds its arguments *)
		newDownValues = Check[Quiet[instrumentRules[name,originalDownValues,settings],{General::newsym,General::shdw}],$Failed];
		newSubValues = Check[Quiet[instrumentRules[name,originalSubValues,settings],{General::newsym,General::shdw}],$Failed];
		failedQ = Or[newDownValues === $Failed,newSubValues === $Failed];
		If[failedQ,
			newDownValues = originalDownValues;
			newSubValues = originalSubValues;
			$rewriteFailures = Append[$rewriteFailures,name]
		];

		Unprotect[symbol];
		DownValues[symbol] = newDownValues;
		If[newSubValues =!= originalSubValues,
			SubValues[symbol] = newSubValues
		];

		$instrumentCache[cacheKey[name,settings]] = Association[
			"OriginalDownValues" -> originalDownValues,
			"OriginalSubValues" -> originalSubValues,
			"DownValues" -> newDownValues,
			"SubValues" -> newSubValues,
			"Unchanged" -> And[newDownValues === originalDownValues,newSubValues === originalSubValues],
			"Failed" -> failedQ
		];
	]
];


(* ::Subsection::Closed:: *)
(*Rewriting rules*)


instrumentRules[functionName_String,rules_List,settings_Association]:=MapIndexed[
	instrumentRule[#1,First[#2],functionName,settings]&,
	rules
];

registerTag[tag_Association]:=(
	$tagCounter = $tagCounter + 1;
	$locationTags[$tagCounter] = tag;
	$tagCounter
);

(*
	Rewrites one rule. Tags record what DebuggerWhere needs to find the source line again:
	the function, the rule's pattern names (to pick the overload) and, per variable, the ordinal of
	this node among the rule's Set (or Function) nodes binding that variable, in source order.
*)
instrumentRule[rule_RuleDelayed,ruleIndex_Integer,functionName_String,settings_Association]:=Module[
	{
		held,trackedQ,rhsQ,blockedPositions,allowedQ,patternInfo,patternNames,setPositions,functionPositions,
		withPositions,setVariables,functionVariables,setOrdinals,functionOrdinals,setOrdinalLookup,
		mutationPositions,mutationVariables,mutationOrdinals,
		setEdits,functionEdits,withEdits,patternEdits,mutationEdits,edits
	},

	held = HoldComplete[rule];
	trackedQ = StringMatchQ[#,RegularExpression[settings["ContextsRegex"]]]&;

	(* Only the right-hand side is rewritten, and never inside held, compiled or verbatim code *)
	rhsQ = MatchQ[#,{1,2,___}]&;
	blockedPositions = Select[
		Position[held,_Hold|_HoldComplete|_HoldForm|_HoldPattern|_Defer|_Inactive|_Compile|_FunctionCompile|_Verbatim],
		rhsQ
	];
	allowedQ = Function[position,
		And[
			rhsQ[position],
			NoneTrue[blockedPositions,prefixQ[#,position]&]
		]
	];

	patternInfo = DeleteDuplicatesBy[
		Cases[
			Extract[held,{1,1},HoldComplete],
			Verbatim[Pattern][symbol_Symbol,pattern_] :> Association[
				"Name" -> normalizeName[symbolString[symbol]],
				"Context" -> symbolContext[symbol],
				"Held" -> If[sequencePatternQ[pattern],HoldComplete[{symbol}],HoldComplete[symbol]]
			],
			{0,Infinity},
			Heads -> True
		],
		Lookup["Name"]
	];
	patternNames = DeleteDuplicates[Map[shortName,Lookup[patternInfo,"Name",{}]]];

	setPositions = preOrder[Select[Position[held,Verbatim[Set][_Symbol|{__Symbol},_]],rhsQ]];
	functionPositions = preOrder[Select[Position[held,Verbatim[Function][Except[Null,_Symbol]|{__Symbol},__]],rhsQ]];
	withPositions = preOrder[Select[Position[held,Verbatim[With][_List,_]],rhsQ]];

	(* Changes to part of a variable or by an operator (x[[i]] = v, AppendTo, +=, ++, AssociateTo, ...) *)
	mutationPositions = Select[
		preOrder[Select[Position[held,$mutationHeads[_,___]],rhsQ]],
		mutationInfo[Extract[held,#,HoldComplete]] =!= None&
	];
	mutationVariables = Map[{mutationInfo[Extract[held,#,HoldComplete]]}&,mutationPositions];
	mutationOrdinals = ordinals[mutationVariables];

	setVariables = Map[heldVariables[Extract[held,#,HoldComplete]]&,setPositions];
	functionVariables = Map[heldVariables[Extract[held,#,HoldComplete]]&,functionPositions];
	setOrdinals = ordinals[setVariables];
	functionOrdinals = ordinals[functionVariables];
	setOrdinalLookup = AssociationThread[setPositions,setOrdinals];

	(*
		Set: "x = (Sow[marker]; value)". A plain CompoundExpression leaves Return/Sequence values exactly as they were.
		Module/Block initializers are never reported by the set handler, so they log themselves instead:
		"a = logAssigned[tag, \"a\", value]"
	*)
	setEdits = MapThread[
		Function[{position,variables,ordinal},
			With[
				{
					tag = Association[
						"Function" -> functionName,
						"Rule" -> ruleIndex,
						"Kind" -> "Set",
						"Variables" -> Lookup[variables,"Name"],
						"Ordinals" -> ordinal,
						"PatternNames" -> patternNames
					]
				},
				Which[
					Or[!allowedQ[position],withInitializerQ[held,position],!AnyTrue[Lookup[variables,"Context"],trackedQ]],
						Nothing,
					scopeInitializerQ[held,position],
						setEdit[position,tag,"Initializer"],
					settings["TrackLocations"],
						setEdit[position,tag,"Marker"],
					True,
						Nothing
				]
			]
		],
		{setPositions,setVariables,setOrdinals}
	];

	(* Function: log the parameters first thing in the body *)
	functionEdits = If[settings["TrackInputs"],
		MapThread[
			Function[{position,variables,ordinal},
				With[{tracked = Select[variables,trackedQ[#["Context"]]&]},
					If[And[allowedQ[position],tracked =!= {}],
						With[
							{
								tagId = registerTag[Association[
									"Function" -> functionName,
									"Rule" -> ruleIndex,
									"Kind" -> "Function",
									"Variables" -> Lookup[tracked,"Name"],
									"Ordinals" -> ordinal,
									"PatternNames" -> patternNames
								]],
								names = Lookup[tracked,"Name"],
								values = Apply[Join,Lookup[tracked,"Held"]]
							},
							{
								position,
								Function[node,
									Replace[node,
										HoldComplete[Verbatim[Function][parameters_,body_,rest___]] :> functionPH[parameters,CompoundExpression[logInputs[tagId,names,values],body],rest]
									]
								]
							}
						],
						Nothing
					]
				]
			],
			{functionPositions,functionVariables,functionOrdinals}
		],
		{}
	];

	(* With: log the constants at the top of the body (inside a trailing Condition, so the idiom still works) *)
	withEdits = If[settings["TrackInputs"],
		Map[
			Function[position,
				With[
					{
						tracked = Select[heldVariables[Extract[held,position,HoldComplete]],trackedQ[#["Context"]]&],
						initializerOrdinals = Association[
							Map[
								Lookup[setOrdinalLookup,Key[#],Association[]]&,
								Select[setPositions,And[Length[#] === Length[position]+2,Take[#,Length[position]] === position,#[[-2]] === 1]&]
							]
						]
					},
					If[And[allowedQ[position],tracked =!= {}],
						With[
							{
								tagId = registerTag[Association[
									"Function" -> functionName,
									"Rule" -> ruleIndex,
									"Kind" -> "With",
									"Variables" -> Lookup[tracked,"Name"],
									"Ordinals" -> initializerOrdinals,
									"PatternNames" -> patternNames
								]],
								names = Lookup[tracked,"Name"],
								values = Apply[Join,Lookup[tracked,"Held"]]
							},
							{
								position,
								Function[node,
									Replace[
										{node,injectHeld[Extract[node,{1,2},HoldComplete],HoldComplete[logInputs[tagId,names,values]]]},
										{HoldComplete[Verbatim[With][variables_,_]],HoldComplete[newBody_]} :> withPH[variables,newBody]
									]
								]
							}
						],
						Nothing
					]
				]
			],
			withPositions
		],
		{}
	];

	(* Pattern inputs: a Condition around the whole left-hand side runs only once the rule has matched *)
	patternEdits = With[{tracked = Select[patternInfo,trackedQ[#["Context"]]&]},
		If[Or[!settings["TrackInputs"],tracked === {}],
			{},
			With[
				{
					tagId = registerTag[Association[
						"Function" -> functionName,
						"Rule" -> ruleIndex,
						"Kind" -> "Pattern",
						"Variables" -> Lookup[tracked,"Name"],
						"Ordinals" -> Association[],
						"PatternNames" -> patternNames
					]],
					names = Lookup[tracked,"Name"],
					values = Apply[Join,Lookup[tracked,"Held"]]
				},
				{{
					{1,1},
					Function[node,
						Replace[node,
							HoldComplete[Verbatim[HoldPattern][lhs_]] :> HoldPattern[Condition[lhs,CompoundExpression[logInputs[tagId,names,values],True]]]
						]
					]
				}}
			]
		]
	];

	(*
		Mutations the set handler reports (x[[i]] = v, assoc["k"] = v, AppendTo, PrependTo) get a marker, like a Set.
		The ones it never reports (+=, ++, AssociateTo, KeyDropFrom, ...) log the variable's new value themselves.
	*)
	mutationEdits = MapThread[
		Function[{position,variables,ordinal},
			With[
				{
					info = First[variables],
					tag = Association[
						"Function" -> functionName,
						"Rule" -> ruleIndex,
						"Kind" -> "Mutation",
						"Variables" -> Lookup[variables,"Name"],
						"Ordinals" -> ordinal,
						"PatternNames" -> patternNames
					]
				},
				Which[
					Or[!allowedQ[position],!trackedQ[info["Context"]]],
						Nothing,
					info["Reported"],
						If[settings["TrackLocations"],mutationEdit[position,tag,"Marker",info],Nothing],
					True,
						mutationEdit[position,tag,"Log",info]
				]
			]
		],
		{mutationPositions,mutationVariables,mutationOrdinals}
	];

	edits = Join[setEdits,functionEdits,withEdits,patternEdits,mutationEdits];

	If[edits === {},
		rule,
		(* Deepest first, so a node is rebuilt from children that were already rewritten *)
		First[
			ReplaceAll[
				Fold[
					applyEdit,
					held,
					SortBy[edits,-Length[First[#]]&]
				],
				placeholderRules
			]
		]
	]
];
instrumentRule[other_,___]:=other;

(*
	A named function on purpose: a pure Function with a With inside would have that With substitute into the
	edit's own builder Function (whose parameter shares the With variable's name) and break it
*)
applyEdit[current_HoldComplete,{position_List,builder_}]:=With[
	{rebuilt = builder[Extract[current,position,HoldComplete]]},
	(* An edit whose shape no longer matched comes back still wrapped; leave that node alone *)
	If[MatchQ[rebuilt,_HoldComplete],current,ReplacePart[current,position -> rebuilt]]
];

prefixQ[prefix_List,position_List]:=And[
	Length[prefix] <= Length[position],
	Take[position,Length[prefix]] === prefix
];

(* Positions in source order: a parent before its children, left before right *)
preOrder[positions_List]:=With[
	{depth = Max[0,Map[Length,positions]]},
	SortBy[positions,PadRight[#,depth,-1]&]
];

setEdit[position_List,tag_Association,"Marker"]:=With[
	{tagId = registerTag[tag]},
	{
		position,
		Function[node,
			Replace[node,
				HoldComplete[Verbatim[Set][lhs_,rhs_]] :> setPH[lhs,CompoundExpression[Sow[setMarker[tagId],"assignment"],rhs]]
			]
		]
	}
];
setEdit[position_List,tag_Association,"Initializer"]:=With[
	{tagId = registerTag[tag],name = First[tag["Variables"]]},
	{
		position,
		Function[node,
			Replace[node,
				HoldComplete[Verbatim[Set][lhs_,rhs_]] :> setPH[lhs,logAssigned[tagId,name,rhs]]
			]
		]
	}
];

mutationEdit[position_List,tag_Association,"Marker",_]:=With[
	{tagId = registerTag[tag]},
	{
		position,
		Function[node,
			Replace[node,HoldComplete[mutation_] :> sequencePH[Sow[setMarker[tagId],"assignment"],mutation]]
		]
	}
];
mutationEdit[position_List,tag_Association,"Log",info_Association]:=With[
	{tagId = registerTag[tag],name = info["Name"],baseHeld = info["Held"]},
	{
		position,
		Function[node,
			Replace[{node,baseHeld},{HoldComplete[mutation_],HoldComplete[base_]} :> mutationPH[tagId,name,mutation,base]]
		]
	}
];

(* Operator forms the set handler never reports; AppendTo/PrependTo and part assignments it does report *)
$mutationHeads = Set|AddTo|SubtractFrom|TimesBy|DivideBy|Increment|Decrement|PreIncrement|PreDecrement|AssociateTo|KeyDropFrom|AppendTo|PrependTo;

(* The variable a mutation changes, or None. Set with a plain symbol on the left is an ordinary Set, not a mutation. *)
mutationInfo[HoldComplete[Verbatim[Set][target_,_]]]:=mutationBase[HoldComplete[target],True,False];
mutationInfo[HoldComplete[(AppendTo|PrependTo)[target_,___]]]:=mutationBase[HoldComplete[target],True,True];
mutationInfo[HoldComplete[_[target_,___]]]:=mutationBase[HoldComplete[target],False,True];
mutationInfo[_]:=None;

mutationBase[HoldComplete[symbol_Symbol],reportedQ_,True]:=Append[variableInfo[symbol],"Reported" -> reportedQ];
mutationBase[HoldComplete[Verbatim[Part][symbol_Symbol,___]],reportedQ_,_]:=Append[variableInfo[symbol],"Reported" -> reportedQ];
(* symbol[key]: a variable holding an Association, never a function (a symbol with definitions, e.g. memoization) *)
mutationBase[HoldComplete[(symbol:Except[List|Part,_Symbol])[___]],reportedQ_,_]/;DownValues[symbol] === {}:=Append[variableInfo[symbol],"Reported" -> reportedQ];
mutationBase[___]:=None;

scopeInitializerQ[held_,position_List]:=And[
	Length[position] >= 2,
	position[[-2]] === 1,
	MatchQ[Extract[held,Drop[position,-2],HoldComplete],HoldComplete[(Module|Block)[_List,_]]]
];

withInitializerQ[held_,position_List]:=And[
	Length[position] >= 2,
	position[[-2]] === 1,
	MatchQ[Extract[held,Drop[position,-2],HoldComplete],HoldComplete[Verbatim[With][_List,_]]]
];

(* Running count of each variable over the nodes, in source order *)
ordinals[variableLists_List]:=Module[
	{counts = Association[]},
	Map[
		Function[variables,
			Association[
				Map[
					Function[name,
						counts[name] = Lookup[counts,name,0]+1;
						name -> counts[name]
					],
					Lookup[variables,"Name",{}]
				]
			]
		],
		variableLists
	]
];

variableInfo[symbol_Symbol]:=Association[
	"Name" -> normalizeName[symbolString[symbol]],
	"Context" -> symbolContext[symbol],
	"Held" -> HoldComplete[symbol]
];

heldVariables[HoldComplete[Verbatim[Set][symbol_Symbol,_]]]:={variableInfo[symbol]};
heldVariables[HoldComplete[Verbatim[Set][symbols:{__Symbol},_]]]:=Cases[HoldComplete[symbols],symbol_Symbol :> variableInfo[symbol],{2}];
heldVariables[HoldComplete[Verbatim[Function][symbol_Symbol,__]]]:={variableInfo[symbol]};
heldVariables[HoldComplete[Verbatim[Function][symbols:{__Symbol},__]]]:=Cases[HoldComplete[symbols],symbol_Symbol :> variableInfo[symbol],{2}];
heldVariables[HoldComplete[Verbatim[With][variables_List,_]]]:=Cases[HoldComplete[variables],Verbatim[Set][symbol_Symbol,_] :> variableInfo[symbol],{2}];
heldVariables[_]:={};

(* Sequence patterns (x__, opts:OptionsPattern[]) are logged as a list of what they matched *)
sequencePatternQ[(Verbatim[PatternTest]|Verbatim[Condition])[pattern_,_]]:=sequencePatternQ[pattern];
sequencePatternQ[_BlankSequence|_BlankNullSequence|_OptionsPattern|_Repeated|_RepeatedNull]:=True;
sequencePatternQ[_]:=False;

(* A body ending in Condition (through Module/Block/With) is the "value /; test" idiom: log inside the Condition *)
conditionTailQ[HoldComplete[Verbatim[Condition][_,_]]]:=True;
conditionTailQ[HoldComplete[(Module|Block|With|withPH)[_,body_]]]:=conditionTailQ[HoldComplete[body]];
conditionTailQ[_]:=False;

injectHeld[HoldComplete[Verbatim[Condition][value_,test_]],HoldComplete[log_]]:=HoldComplete[Condition[CompoundExpression[log,value],test]];
injectHeld[HoldComplete[(head:(Module|Block|With|withPH))[variables_,body_]],log_HoldComplete]/;conditionTailQ[HoldComplete[body]]:=Replace[
	injectHeld[HoldComplete[body],log],
	HoldComplete[newBody_] :> HoldComplete[head[variables,newBody]]
];
injectHeld[HoldComplete[body_],HoldComplete[log_]]:=HoldComplete[CompoundExpression[log,body]];


(* ::Subsection::Closed:: *)
(*Runtime logging*)


(* Called from rewritten code: one AssignmentLog entry per input, all tagged with the same call *)
SetAttributes[logInputs,HoldAll];
logInputs[tagId_Integer,names_List,HoldComplete[values___]]:=With[
	{time = UnixTime[],heldValues = Apply[List,Map[Hold,HoldComplete[values]]]},
	Sow[callMarker[tagId],"assignment"];
	If[Length[heldValues] === Length[names],
		MapThread[
			Sow[{time,#1,#2,DebuggerLocation[tagId]},"assignment"]&,
			{names,heldValues}
		]
	];
];
logInputs[___]:=Null;

(* A mutation the set handler does not report (+=, ++, AssociateTo, ...): log the variable's new value; return the mutation's own result *)
SetAttributes[logMutation,SequenceHold];
logMutation[tagId_Integer,name_String,result_,newValue_]:=(
	Sow[{UnixTime[],name,Hold[newValue],DebuggerLocation[tagId]},"assignment"];
	result
);

(* A Module/Block initializer's value, logged as a complete entry since the set handler does not report it *)
SetAttributes[logAssigned,SequenceHold];
logAssigned[tagId_Integer,name_String,value_]:=(
	Sow[{UnixTime[],name,Hold[value],DebuggerLocation[tagId]},"assignment"];
	value
);

(*
	Turns the raw Sow stream into AssignmentLog entries. A Set marker is Sown before its value is computed,
	so markers are kept as a stack per variable: an assignment takes the newest pending marker for its name,
	which pairs nested assignments of the same name (x = f[], with x = ... inside f) innermost first.
*)
pairLocations[items_List]:=Module[
	{pending = Association[],call = 0,paired},

	paired = Reap[
		Scan[
			Function[item,
				Switch[item,
					setMarker[_Integer],
						Scan[
							Function[variable,
								pending[variable] = Prepend[Lookup[pending,variable,{}],First[item]]
							],
							Lookup[Lookup[$locationTags,First[item],Association[]],"Variables",{}]
						],
					callMarker[_Integer],
						call = call+1,
					{_,_String,_Hold,_DebuggerLocation},
						Sow[ReplacePart[item,4 -> DebuggerLocation[First[Last[item]],call]],"paired"],
					{_,_String,_Hold},
						Sow[Append[item,popLocation[pending,baseName[item[[2]]]]],"paired"]
				]
			],
			items
		],
		"paired"
	][[2]];

	Replace[paired,{{entries_List} :> entries,_ -> {}}]
];

SetAttributes[popLocation,HoldFirst];
popLocation[pending_,variable_String]:=With[
	{ids = Lookup[pending,variable,{}]},
	If[ids === {},
		Missing["NotInstrumented"],
		pending[variable] = Rest[ids];
		DebuggerLocation[First[ids]]
	]
];

Format[DebuggerLocation[id_Integer,___]]:=With[
	{tag = Lookup[$locationTags,id,Association[]]},
	Row[{"\[LeftSkeleton]",shortName[Lookup[tag,"Function","?"]]," ",Lookup[tag,"Kind",""],"\[RightSkeleton]"}]
];


(* ::Section::Closed:: *)
(*Locations*)


(* ::Subsection::Closed:: *)
(*Source files*)


sourceFilesFor[function_String]:=With[
	{key = {function,$DebuggerSourceFiles}},
	If[KeyExistsQ[$sourceFilesCache,key],
		$sourceFilesCache[key],
		$sourceFilesCache[key] = findSourceFiles[function]
	]
];

findSourceFiles[function_String]:=Which[
	ListQ[$DebuggerSourceFiles],
		$DebuggerSourceFiles,
	$DebuggerSourceFiles =!= Automatic,
		Replace[Quiet[$DebuggerSourceFiles[function]],Except[{___String}] -> {}],
	packagerQ[],
		packagerSourceFiles[function],
	True,
		{}
];

packagerSourceFiles[function_String]:=Quiet[
	Module[{package},
		package = Symbol["Packager`FunctionPackage"][function];
		If[StringQ[package],
			With[{packageName = package},
				Replace[
					Symbol["Packager`PackageSources"][Symbol["Packager`PackageMetadata"][packageName]],
					Except[{___String}] -> {}
				]
			],
			{}
		]
	]
];

(* Lines of a file, re-read when it changes on disk; only a handful of files are kept *)
fileLines[file_String]:=Module[
	{date,cached,lines},

	date = Quiet[FileDate[file]];
	cached = Lookup[$fileLinesCache,file,None];
	If[And[AssociationQ[cached],cached["Date"] === date],
		cached["Lines"],
		lines = Replace[Quiet[ReadList[file,String,NullRecords -> True]],Except[{___String}] -> {}];
		lines = StringDelete[lines,"\r"~~EndOfString];
		If[Length[$fileLinesCache] >= 20,
			$fileLinesCache = Rest[$fileLinesCache]
		];
		$fileLinesCache[file] = Association["Date" -> date,"Lines" -> lines];
		lines
	]
];

(*
	Top-level definitions start in column 0 ("name[...") and run until the next line that starts in
	column 0 with a letter, $ or "(" (the next definition or a section comment). Closing brackets in
	column 0 stay inside the definition.
*)
fileIndex[file_String]:=Module[
	{date,cached,lines,boundaries,heads,ends,index},

	date = Quiet[FileDate[file]];
	cached = Lookup[$fileIndexCache,file,None];
	If[And[AssociationQ[cached],cached["Date"] === date],
		cached["Index"],
		lines = fileLines[file];
		boundaries = Flatten[Position[StringMatchQ[lines,RegularExpression["[A-Za-z$(].*"]],True]];
		index = If[boundaries === {},
			Association[],
			heads = StringCases[lines[[boundaries]],RegularExpression["^([A-Za-z$][A-Za-z0-9$]*)\\["] :> "$1"];
			ends = Append[Rest[boundaries]-1,Length[lines]];
			GroupBy[
				Select[Transpose[{heads,boundaries,ends}],#[[1]] =!= {}&],
				(#[[1,1]]&) -> ({#[[2]],#[[3]]}&)
			]
		];
		$fileIndexCache[file] = Association["Date" -> date,"Index" -> index];
		index
	]
];

sourceBlocks[function_String]:=Flatten[
	Map[
		Function[file,
			Map[
				Association["File" -> file,"Start" -> #[[1]],"End" -> #[[2]]]&,
				Lookup[fileIndex[file],shortName[function],{}]
			]
		],
		sourceFilesFor[function]
	]
];

(* Comments and strings are blanked out, keeping every newline so line numbers do not move *)
stripCode[lines_List]:=StringSplit[
	StringReplace[
		StringRiffle[lines,"\n"],
		{
			comment:("(*"~~Shortest[___]~~"*)") :> StringRepeat["\n",StringCount[comment,"\n"]],
			RegularExpression["\"(?:[^\"\\\\]|\\\\.)*\""] :> StringRepeat["\n",StringCount["$0","\n"]]
		}
	],
	"\n",
	All
];

blockLines[block_Association]:=With[
	{lines = fileLines[block["File"]]},
	stripCode[lines[[block["Start"];;Min[block["End"],Length[lines]]]]]
];


(* ::Subsection::Closed:: *)
(*Matching source*)


$nameBefore = "(?<![A-Za-z0-9$`])";
$nameAfter = "(?![A-Za-z0-9$`])";

regexEscape[text_String]:=StringReplace[text,character:Characters["\\^$.|?*+()[]{}"] :> "\\"<>character];

(* "x = ..." (also Module/Block/With initializers) and "{..., x, ...} = ..."; not ==, =!=, := *)
setRegex[variable_String]:=With[
	{name = regexEscape[variable]},
	StringJoin[
		$nameBefore,name,"\\s*=(?![=!])",
		"|",
		"\\{[^{}=;]*",$nameBefore,name,$nameAfter,"[^{}=;]*\\}\\s*=(?![=!])"
	]
];

(* x[[...]] = / += ..., x["k"] = ..., x += 2 (and -=, *=, /=, ++, --), ++x, --x, AppendTo[x, ...] (and the other mutating functions) *)
mutationRegex[variable_String]:=With[
	{name = regexEscape[variable],operator = "(?:=(?![=!])|\\+=|-=|\\*=|/=|\\+\\+|--)"},
	StringJoin[
		$nameBefore,name,"\\s*\\[\\[[^;]*?\\]\\]\\s*",operator,
		"|",
		$nameBefore,name,"\\s*\\[(?!\\[)[^;]*?\\]\\s*",operator,
		"|",
		$nameBefore,name,$nameAfter,"\\s*(?:\\+=|-=|\\*=|/=|\\+\\+|--)",
		"|",
		"(?:\\+\\+|--)\\s*",name,$nameAfter,
		"|",
		"(?:AppendTo|PrependTo|AssociateTo|KeyDropFrom|AddTo|SubtractFrom|TimesBy|DivideBy|Increment|Decrement|PreIncrement|PreDecrement)\\[\\s*",name,$nameAfter
	]
];

(* Function[x, ...], Function[{..., x, ...}, ...], x |-> ..., {..., x, ...} |-> ... *)
functionRegex[variable_String]:=With[
	{name = regexEscape[variable]},
	StringJoin[
		"Function\\[\\s*(?:\\{[^{}]*",$nameBefore,name,$nameAfter,"[^{}]*\\}|",name,$nameAfter,")",
		"|",
		"(?:\\{[^{}]*",$nameBefore,name,$nameAfter,"[^{}]*\\}|",$nameBefore,name,$nameAfter,")\\s*(?:\\|->|\\\\\\[Function\\])"
	]
];

(* {file, line} for every match in the block, in source order, one entry per match *)
blockMatches[block_Association,regex_String]:=Flatten[
	MapIndexed[
		Function[{line,position},
			ConstantArray[
				{block["File"],block["Start"]+First[position]-1},
				StringCount[line,RegularExpression[regex]]
			]
		],
		blockLines[block]
	],
	1
];

blockHeader[block_Association]:=With[
	{lines = fileLines[block["File"]]},
	First[
		StringSplit[
			StringRiffle[stripCode[lines[[block["Start"];;Min[block["End"],block["Start"]+40,Length[lines]]]]],"\n"],
			":=",
			2
		],
		""
	]
];

headerPatternNames[header_String]:=DeleteDuplicates[
	StringCases[header,RegularExpression[$nameBefore<>"([A-Za-z$][A-Za-z0-9$]*)\\s*(?:_|:(?!=))"] :> "$1"]
];

(* Overloads whose pattern names best match the rule's *)
chooseOverloads[blocks_List,patternNames_List]:=If[Length[blocks] <= 1,
	blocks,
	With[
		{scores = Map[-Length[SymmetricDifference[headerPatternNames[blockHeader[#]],patternNames]]&,blocks]},
		Pick[blocks,scores,Max[scores]]
	]
];

sourceLine[{file_String,line_Integer}]:=With[
	{lines = fileLines[file]},
	If[1 <= line <= Length[lines],StringTrim[lines[[line]]],Missing["NotAvailable"]]
];


(* ::Subsection::Closed:: *)
(*Resolving*)


resolveLocation[DebuggerLocation[id_Integer,call___Integer],variableName_String]:=Module[
	{tag,kind,variable,base,blocks,chosen,matches,ordinal,location},

	tag = Lookup[$locationTags,id,Missing["UnknownTag"]];
	If[MissingQ[tag],
		Return[Association["Function" -> tag,"File" -> tag,"Line" -> tag]]
	];

	kind = tag["Kind"];
	variable = baseName[variableName];
	base = Join[
		Association["Function" -> tag["Function"],"Kind" -> kind],
		If[{call} === {},Association[],Association["Call" -> call]]
	];

	blocks = sourceBlocks[tag["Function"]];
	If[blocks === {},
		Return[Join[base,Association["File" -> Missing["SourceNotFound"],"Line" -> Missing["SourceNotFound"]]]]
	];

	chosen = chooseOverloads[blocks,tag["PatternNames"]];
	ordinal = Lookup[tag["Ordinals"],variable,Missing[]];
	matches = Switch[kind,
		"Pattern", Map[{#["File"],#["Start"]}&,chosen],
		"Function", Flatten[Map[blockMatches[#,functionRegex[shortName[variable]]]&,chosen],1],
		"Mutation", Flatten[Map[blockMatches[#,mutationRegex[shortName[variable]]]&,chosen],1],
		_, Flatten[Map[blockMatches[#,setRegex[shortName[variable]]]&,chosen],1]
	];

	(* The nth node binding the variable in the rule is the nth textual match in its overload *)
	location = Which[
		kind === "Pattern",
			If[Length[matches] === 1,First[matches],Missing["Ambiguous"]],
		And[Length[chosen] === 1,IntegerQ[ordinal],1 <= ordinal <= Length[matches]],
			matches[[ordinal]],
		Length[matches] === 1,
			First[matches],
		True,
			Missing["Ambiguous"]
	];

	Join[
		base,
		Association[
			"File" -> If[MissingQ[location],location,location[[1]]],
			"Line" -> If[MissingQ[location],location,location[[2]]],
			"Code" -> If[MissingQ[location],location,sourceLine[location]],
			"Candidates" -> matches
		]
	]
];
resolveLocation[missing_,_]:=Association["Function" -> missing,"File" -> missing,"Line" -> missing];

assignmentLog[]:=Replace[
	DebuggerInformation,
	{information_Association :> Lookup[information,"AssignmentLog",{}],_ -> {}}
];

describeEntry[entry_List,index_Integer]:=Join[
	Association["Index" -> index,"Variable" -> entry[[2]]],
	resolveLocation[If[Length[entry] >= 4,entry[[4]],Missing["NotTracked"]],entry[[2]]],
	Association["Value" -> entry[[3]]]
];

lastTrackedIndex[log_List]:=SelectFirst[
	Range[Length[log],1,-1],
	MatchQ[log[[#,-1]],_DebuggerLocation]&,
	Length[log]
];

(* AssignmentLog positions of a variable; a name without a context matches it in any context *)
matchingIndices[log_List,name_String]:=With[
	{target = normalizeName[name]},
	Flatten[Position[Map[nameMatchQ[#,target]&,log[[All,2]]],True]]
];

nameMatchQ[name_String,target_String]:=With[
	{normalized = normalizeName[name],base = baseName[name]},
	Or[
		normalized === target,
		base === target,
		And[StringFreeQ[target,"`"],Or[shortName[normalized] === target,shortName[base] === target]]
	]
];

(* The last assignment that has a location; DebuggerWhere[-1] is the literal last entry *)
DebuggerWhere[]:=With[
	{log = assignmentLog[]},
	If[log === {},
		Missing["NoAssignments"],
		DebuggerWhere[lastTrackedIndex[log]]
	]
];
DebuggerWhere[index_Integer]:=With[
	{log = assignmentLog[]},
	If[Or[index === 0,Abs[index] > Length[log]],
		Missing["NoSuchAssignment",index],
		Quiet[describeEntry[log[[index]],If[index > 0,index,Length[log]+index+1]]]
	]
];
DebuggerWhere[name_String]:=With[
	{log = assignmentLog[]},
	Quiet[Map[describeEntry[log[[#]],#]&,matchingIndices[log,name]]]
];
DebuggerWhere[symbol_Symbol]:=DebuggerWhere[symbolString[symbol]];

locationLink[label_String,file_String,line_Integer]:=If[StringQ[$DebuggerLinkTemplate],
	Hyperlink[
		label,
		TemplateApply[
			StringTemplate[$DebuggerLinkTemplate],
			Association["File" -> StringReplace[file,"\\" -> "/"],"Line" -> line]
		]
	],
	label
];

whereLabel[where_Association]:=With[
	{
		function = Replace[Lookup[where,"Function",Missing[]],name_String :> shortName[name]],
		candidates = Lookup[where,"Candidates",{}]
	},
	Which[
		IntegerQ[Lookup[where,"Line"]],
			locationLink[
				StringJoin[function,"  ",FileNameTake[where["File"]],":",ToString[where["Line"]]],
				where["File"],
				where["Line"]
			],
		And[StringQ[function],MatchQ[candidates,{{_String,_Integer}..}]],
			StringJoin[
				function,"  ",FileNameTake[candidates[[1,1]]],": one of lines ",
				StringRiffle[Map[ToString,DeleteDuplicates[candidates[[All,2]]]],", "]
			],
		StringQ[function],
			StringJoin[function,"  (no source line found)"],
		True,
			"\[LongDash]"
	]
];

printLastLocation[reason_String]:=Quiet[
	With[
		{where = DebuggerWhere[]},
		If[AssociationQ[where],
			Print[
				Row[{
					Style["Debugger ("<>reason<>"): ",Bold],
					"last assignment #",where["Index"]," ",
					Style[shortName[where["Variable"]],Bold],
					" in ",
					whereLabel[where]
				}]
			]
		]
	]
];

DebuggerHistory[]:=DebuggerHistory[20];
DebuggerHistory[count_Integer?Positive]:=With[
	{length = Length[assignmentLog[]]},
	DebuggerHistory[Range[Max[1,length-count+1],length]]
];
DebuggerHistory[name_String]:=DebuggerHistory[matchingIndices[assignmentLog[],name]];
DebuggerHistory[indices:{___Integer}]:=Quiet[
	With[
		{log = assignmentLog[]},
		Grid[
			Prepend[
				Map[
					With[{where = describeEntry[log[[#]],#]},
						{
							where["Index"],
							shortName[where["Variable"]],
							whereLabel[where],
							Short[Apply[HoldForm,where["Value"]],1]
						}
					]&,
					Select[indices,1 <= # <= Length[log]&]
				],
				Map[Style[#,Bold]&,{"#","Variable","Where","Value"}]
			],
			Alignment -> Left,
			Dividers -> {None,{2 -> True}},
			Spacings -> {2,0.6}
		]
	]
];

functionNameMatchQ[name_String,query_String]:=Or[
	name === query,
	And[StringFreeQ[query,"`"],shortName[name] === query]
];

inputEntryQ[entry_List,function_String]:=And[
	Length[entry] >= 4,
	MatchQ[entry[[4]],DebuggerLocation[_Integer,_Integer]],
	With[
		{tag = Lookup[$locationTags,entry[[4,1]],Association[]]},
		And[
			Lookup[tag,"Kind",None] === "Pattern",
			functionNameMatchQ[Lookup[tag,"Function",""],function]
		]
	]
];

DebuggerInputs[function_String]:=With[
	{entries = Select[assignmentLog[],inputEntryQ[#,function]&]},
	Values[
		GroupBy[
			entries,
			(#[[4,2]]&) -> (shortName[#[[2]]] -> ReleaseHold[#[[3]]]&),
			Association
		]
	]
];
DebuggerInputs[symbol_Symbol]:=DebuggerInputs[fullName[symbol]];


(* ::Section::Closed:: *)
(*End Package*)


End[];
EndPackage[];
