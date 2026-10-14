(* ::Package:: *)

(*
	Debugger[code] evaluates code, logs every assignment to a variable in DebuggerContexts, and leaves the log in
	DebuggerInformation; a message stops the run (AbortOnMessage) and is re-issued at the end.

	Untracked, GeneralUtilities`WithSetHandler reports each Set to setHandler, which Sows it, and the Reap around the
	run collects the AssignmentLog. With TrackInputs/TrackLocations (the default) the run is observed with TraceScan
	instead (Tracing): no definition is changed, and the log also gets inputs, With constants and +=, AppendTo, ...,
	each tagged with the function it ran in. DebuggerWhere, DebuggerHistory and DebuggerInputs read that back (Locations).
*)

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
TrackedFunctions;
DebuggerWhere;
DebuggerHistory;
DebuggerInputs;
DebuggerLocation;
LastDebuggerAssignments;
ClearDebuggerCache;
$DebuggerSourceFiles;
$DebuggerLinkTemplate;

TrackInputs::usage = "TrackInputs is an option for Debugger, True by default. When True, values bound without Set are logged like assignments: function pattern inputs (f[x_] := ...), Function parameters and With constants.";
TrackLocations::usage = "TrackLocations is an option for Debugger, True by default. When True, every logged assignment remembers the function and source line that made it. Query it with DebuggerWhere or DebuggerHistory.";
TrackedFunctions::usage = "TrackedFunctions is an option for Debugger giving the functions whose calls are tracked (inputs logged, and assignments attributed to them) when TrackInputs or TrackLocations is on. Automatic uses every function in DebuggerContexts plus the public functions of those packages.";
DebuggerWhere::usage = "DebuggerWhere[] gives the source location of the last tracked assignment of the last Debugger run. DebuggerWhere[n] gives it for entry n of the AssignmentLog (negative n counts from the end). DebuggerWhere[\"var\"] gives every assignment of var with its location.";
DebuggerHistory::usage = "DebuggerHistory[n] shows the last n assignments of the last Debugger run with the function, file and line that made each one. DebuggerHistory[\"var\"] shows every assignment of var; DebuggerHistory[{i, ...}] shows those AssignmentLog entries.";
DebuggerInputs::usage = "DebuggerInputs[f] gives the pattern inputs of every call to f during the last Debugger run, one association per call. Requires TrackInputs -> True.";
DebuggerLocation::usage = "DebuggerLocation[function, kind, rule, observed, call] is the location stored as the fourth element of AssignmentLog entries of a tracked run. DebuggerWhere resolves it to a file and line.";
LastDebuggerAssignments::usage = "LastDebuggerAssignments[n] gives the names of the last n assigned variables (default 20; All for every one).";
ClearDebuggerCache::usage = "ClearDebuggerCache[] drops the cached source files and source-file indexes.";
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
	TrackInputs -> True,
	TrackLocations -> True,
	TrackedFunctions -> Automatic
};
(*
	One run: read the options; evaluate the code inside Reap, an abort guard and the message handler, observed by
	TraceScan when tracking and by the set handler otherwise; stash the raw log and build DebuggerInformation from it;
	re-issue messages and say where the run stopped.
*)
Debugger[codeBlock_,OptionsPattern[]]:=Module[
	{contextsRegex,settings,trackedQ,reapReturn,failures,userAborted = False},

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
		"TrackLocations" -> TrueQ[OptionValue[TrackLocations]],
		"ModuleNumbers" -> TrueQ[OptionValue[ModuleNumbers]]
	];
	trackedQ = Or[settings["TrackInputs"],settings["TrackLocations"]];

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
						(* Setting up tracking reads whole packages, so it runs here, inside the abort guard *)
						If[trackedQ,
							traceRun[codeBlock,settings,OptionValue[DebuggerContexts],OptionValue[TrackedFunctions]],
							WithSetHandler[
								codeBlock,
								setHandler[
									##,
									ContextsRegex -> contextsRegex,
									ModuleNumbers -> moduleNumbers
								]&
							]
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
		$lastRun = Association["Sowed" -> reapReturn[[2]],"TrackedQ" -> trackedQ];
		DebuggerInformation := buildDebuggerInformation[];
	];
	DebuggerInformation;

	failures = Lookup[reapReturn[[2]],"failure",{}];

	(* Throw messages *)
	Message/@failures;

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

(* The raw Sow stream of the last run, kept until DebuggerInformation has been built from it *)
$lastRun = Association["Sowed" -> {},"TrackedQ" -> False];

(* Every AssignmentLog entry is {time, variable, Hold[value], location} *)
buildDebuggerInformation[]:=With[
	{sowedAssignments = Lookup[$lastRun["Sowed"],"assignment",{}]},
	populateDebuggerInformation[
		If[TrueQ[$lastRun["TrackedQ"]],
			dropRepeatedTraced[sowedAssignments],
			Map[Append[#,Missing["NotTracked"]]&,dropRepeatedReports[sowedAssignments]]
		],
		Lookup[$lastRun["Sowed"],"failure",{}],
		Lookup[$lastRun["Sowed"],"messageStack",{}]
	]
];

(*
	Untracked runs: the set handler can report one Set more than once (twice on 14.2). Duplicates are identical reports
	of the same runtime symbol (module number included) back to back, so collapse those and drop the raw name. Nested
	assignments of the same variable name are different Module symbols, so both stay.
*)
dropRepeatedReports[items_List]:=Map[
	Replace[#,{time_,name_String,value_Hold,_String} :> {time,name,value}]&,
	Map[
		First,
		Split[items,And[MatchQ[#1,{_,_String,_Hold,_String}],#1 === #2]&]
	]
];

(* The keys a run leaves in DebuggerInformation *)
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

(* Called by WithSetHandler for every Set during the run; logs it when the variable's context is tracked *)
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
	{$$rawString,$$symbolString},

	If[StringMatchQ[Context[$$base],RegularExpression[OptionValue[ContextsRegex]]],
		$$rawString = partName[$$target,$$base];
		$$symbolString = If[TrueQ[OptionValue[ModuleNumbers]],
			$$rawString,
			StringReplace[$$rawString,RegularExpression["\\$[0-9]+"] -> ""]
		];
		Sow[{UnixTime[],$$symbolString,Hold[$$value],$$rawString},"assignment"]
	]
];

(* Called for every message that is not Quieted: records it and the Stack[] at that point, then stops the run
	unless AbortOnMessage -> False *)
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

(* With BreakOnAssert -> True, a failed Assert opens the Interrupt dialog so the run can be inspected *)
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
(*Tracing*)


(*
	TrackInputs and TrackLocations observe the run with TraceScan; no definition is changed. traceBefore and traceAfter
	keep a stack of the expressions currently evaluating (frames) and log, as it happens:
	- a tracked function's inputs, when its definition applies (the call is matched against its DownValues, read only)
	- Function parameters and With constants, as they are bound
	- every Set, part/key assignment and mutation (+=, AppendTo, ...), with the function it ran in
	An entry's location is DebuggerLocation[function, kind, rule, observed form, call]; DebuggerWhere turns it into a
	file and line only when asked (Locations).
*)

(* Session caches: the source files read to resolve locations *)
$fileIndexCache = Association[];
$fileLinesCache = Association[];
$sourceFilesCache = Association[];

ClearDebuggerCache[]:=(
	$fileIndexCache = Association[];
	$fileLinesCache = Association[];
	$sourceFilesCache = Association[];
);

(* Symbol names: symbolString is the form the log uses (context omitted when on $ContextPath), fullName always has the context *)
SetAttributes[{symbolString,symbolContext,fullName,variableInfo,sequencePatternQ,partName},HoldAll];
symbolString[symbol_Symbol]:=ToString[HoldForm[symbol]];
symbolContext[symbol_Symbol]:=Context[symbol];
fullName[symbol_Symbol]:=StringJoin[Context[symbol],SymbolName[Unevaluated[symbol]]];

(* Module numbers and lexical-scoping renames ("x$123", "x$") are not part of a variable's identity *)
normalizeName[name_String]:=StringReplace[name,RegularExpression["\\$[0-9]*$"] -> ""];
shortName[name_String]:=StringReplace[normalizeName[name],RegularExpression["^.*`"] -> ""];
(* The variable a log name refers to: "x[[i]]" and "assoc[\"k\"]" belong to x and assoc *)
baseName[name_String]:=normalizeName[StringReplace[name,RegularExpression["\\[.*$"] -> ""]];

(* The log name of a part or key assignment: the base keeps its context, contexts inside the part spec are dropped *)
partName[target_,base_Symbol]:=With[
	{baseString = ToString[Unevaluated[base],InputForm]},
	StringJoin[
		baseString,
		StringReplace[
			StringDrop[ToString[Unevaluated[target],InputForm],StringLength[baseString]],
			RegularExpression["[A-Za-z$][A-Za-z0-9$]*`"] -> ""
		]
	]
];


(* ::Subsection::Closed:: *)
(*Which functions are tracked*)


(* For "Pkg`Private`", the public symbols of package Pkg`: from Packager when it is loaded, otherwise every symbol in Pkg` *)
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

(* SLL's Packager is loaded, so it can say which package and source files a function belongs to *)
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

(* Per-run state for deciding which heads are tracked functions; TrackedFunctions -> {f, ...} replaces the automatic choice *)
trackingSetup[contexts_List,trackedFunctions_]:=(
	$publicTracked = If[trackedFunctions === Automatic,
		Association[Map[# -> True&,Flatten[Map[publicSymbolNames,contexts]]]],
		Association[]
	];
	$explicitTracked = If[ListQ[trackedFunctions],
		Association[Map[# -> True&,heldSymbols[trackedFunctions]]],
		None
	];
	$headCache = Association[];
	$bindingCache = Association[];
);

trackedContextQ[context_String]:=StringMatchQ[context,RegularExpression[$contextsRegex]];

(* A head is a tracked function when it has definitions and is in a tracked context, a tracked package's public symbol, or listed *)
SetAttributes[{trackedFunctionQ,computeTrackedFunctionQ},HoldAll];
trackedFunctionQ[head_Symbol]:=Replace[
	$headCache[HoldComplete[head]],
	_Missing :> ($headCache[HoldComplete[head]] = computeTrackedFunctionQ[head])
];
computeTrackedFunctionQ[head_Symbol]:=And[
	!StringStartsQ[Context[head],"Debugger`"|"System`"],
	DownValues[head] =!= {},
	If[$explicitTracked === None,
		Or[trackedContextQ[Context[head]],KeyExistsQ[$publicTracked,fullName[head]]],
		KeyExistsQ[$explicitTracked,HoldComplete[head]]
	]
];


(* ::Subsection::Closed:: *)
(*Observing the run*)


(* Evaluates body under TraceScan with fresh per-run state *)
SetAttributes[traceRun,HoldFirst];
traceRun[body_,settings_Association,contexts_List,trackedFunctions_]:=Block[
	{
		$contextsRegex = settings["ContextsRegex"],
		$trackInputs = settings["TrackInputs"],
		$moduleNumbers = settings["ModuleNumbers"],
		$frames = {},
		$frameCounter = 0,
		$pendingCalls = Association[],
		$functionStack = {},
		$withState = Association[],
		$callCounter = 0,
		$publicTracked,
		$explicitTracked,
		$headCache,
		$bindingCache
	},
	trackingSetup[contexts,trackedFunctions];
	TraceScan[traceBefore,body,_,traceAfter]
];

(*
	Frames are {id, event, kind, parent id}, kept as a linked list {frame, rest} so a push or pop is one assignment.
	kind is "Call" (a tracked function), "PureCall" (a Function applied), "With", "Set", "Mutation" or None.
*)
$eventKindRules = {
	HoldForm[_Set] -> "Set",
	HoldForm[(AddTo|SubtractFrom|TimesBy|DivideBy|Increment|Decrement|PreIncrement|PreDecrement|AppendTo|PrependTo|AssociateTo|KeyDropFrom)[_,___]] -> "Mutation",
	HoldForm[With[_List,_]] -> "With",
	HoldForm[(_Function)[___]] -> "PureCall",
	HoldForm[(head_Symbol)[___]] :> If[trackedFunctionQ[head],"Call",None],
	_ -> None
};

topFrame[]:=If[$frames === {},None,First[$frames]];

(* Before each step: settle a call waiting to see whether its definition applied, then push the step *)
traceBefore[event_]:=With[
	{parent = topFrame[]},
	If[And[parent =!= None,KeyExistsQ[$pendingCalls,First[parent]]],
		Quiet[settleCall[parent,event]]
	];
	pushFrame[event,Replace[event,$eventKindRules],If[parent === None,0,First[parent]]]
];

pushFrame[event_,kind_,parentId_Integer]:=(
	$frameCounter = $frameCounter+1;
	$frames = {{$frameCounter,event,kind,parentId},$frames};
	Which[
		MatchQ[kind,"Call"|"PureCall"],$pendingCalls[$frameCounter] = True,
		kind === "With",Quiet[startWith[$frameCounter,event]]
	];
);

(* After each step: pop its frame (and any frame a Throw or Abort skipped) and log what the step did. Only the logging
	is Quieted: a message there would otherwise reach the message handler and stop the run *)
SetAttributes[traceAfter,HoldAllComplete];
traceAfter[event_,result_]:=With[
	{frame = popFrame[event]},
	If[frame =!= None,
		finishFrame[frame,HoldComplete[result]]
	]
];

popFrame[event_]:=If[And[$frames =!= {},$frames[[1,2]] === event],
	With[{frame = $frames[[1]]},
		$frames = $frames[[2]];
		frame
	],
	unwindTo[event]
];

(* The frame is deeper in the stack when steps above it never reported back; drop those. A frame never pushed is ignored *)
unwindTo[event_]:=Module[
	{rest = $frames,found = False},
	While[And[rest =!= {},!found],
		found = rest[[1,2]] === event;
		If[!found,rest = rest[[2]]]
	];
	If[found,
		While[$frames[[1,2]] =!= event,
			finishFrame[$frames[[1]],None];
			$frames = $frames[[2]]
		];
		popFrame[event],
		None
	]
];

finishFrame[{id_,event_,kind_,parentId_},result_]:=(
	If[KeyExistsQ[$pendingCalls,id],$pendingCalls = KeyDrop[$pendingCalls,id]];
	If[And[$functionStack =!= {},$functionStack[[1,1]] === id],$functionStack = $functionStack[[2]]];
	If[result =!= None,
		Which[
			kind === "Set",If[!innerSetQ[event],Quiet[logSet[event,result]]],
			kind === "Mutation",Quiet[logMutationEvent[event]],
			kind === "With",$withState = KeyDrop[$withState,id]
		];
		If[KeyExistsQ[$withState,parentId],Quiet[withChildDone[parentId,result]]]
	];
);

(*
	A call's definition has applied once its first child step is neither its head nor one of its arguments.
	A child with the same head is the same call again with its arguments evaluated, which settles in turn.
*)
settleCall[{id_,call_,kind_,_},child_]:=If[!headOrArgumentQ[call,child],
	$pendingCalls = KeyDrop[$pendingCalls,id];
	If[!sameHeadQ[call,child],
		applied[id,call,kind]
	]
];

headOrArgumentQ[call_HoldForm,child_HoldForm]:=Or[
	child === Extract[call,{1,0},HoldForm],
	AnyTrue[Cases[call,argument_ :> HoldForm[argument],{2}],# === child&]
];

sameHeadQ[call_HoldForm,child_HoldForm]:=Extract[call,{1,0},HoldForm] === Extract[child,{1,0},HoldForm];

(* A tracked function's definition applied: it becomes the function later entries are attributed to, and its inputs are logged *)
applied[id_,call_,"Call"]:=With[
	{binding = bindCall[call]},
	If[AssociationQ[binding],
		$callCounter = $callCounter+1;
		$functionStack = {{id,binding["Function"],binding["Rule"]},$functionStack};
		If[$trackInputs,
			logBound[
				binding["Names"],
				binding["Values"],
				DebuggerLocation[binding["Function"],"Pattern",binding["Rule"],None,$callCounter]
			]
		]
	]
];
applied[_,call_,"PureCall"]:=If[$trackInputs,
	With[{bound = pureFunctionBinding[call]},
		If[ListQ[bound],
			logBound[bound[[1]],bound[[2]],currentLocation["Function",bound[[3]]]]
		]
	]
];

(* The function the run is in, for attributing an entry; None outside every tracked function *)
currentLocation[kind_String,observed_]:=If[$functionStack === {},
	DebuggerLocation[None,kind,None,observed,None],
	DebuggerLocation[$functionStack[[1,2]],kind,$functionStack[[1,3]],observed,None]
];

logName[raw_String]:=If[TrueQ[$moduleNumbers],raw,StringReplace[raw,RegularExpression["\\$[0-9]+"] -> ""]];

(* Entries are {time, name, Hold[value], location, raw name}; the raw name only serves to drop repeats *)
logBound[names_List,HoldComplete[values___],location_]:=With[
	{time = UnixTime[],held = Apply[List,Map[Hold,HoldComplete[values]]]},
	If[Length[held] === Length[names],
		MapThread[Sow[{time,#1,#2,location,#1},"assignment"]&,{names,held}]
	];
];


(* ::Subsection::Closed:: *)
(*Inputs*)


(* The rule a call matches and its tracked inputs, matched against the function's DownValues once per function per run *)
bindCall[HoldForm[call:(head_Symbol)[___]]]:=With[
	{binding = bindingFor[head]},
	Replace[
		Replace[HoldComplete[call],binding["Rules"]],
		{
			{index_Integer,values_HoldComplete} :> Association[
				"Function" -> binding["Function"],
				"Rule" -> binding["Held"][[index]],
				"Names" -> binding["Names"][[index]],
				"Values" -> values
			],
			_ -> None
		}
	]
];
bindCall[_]:=None;

SetAttributes[{bindingFor,buildBinding},HoldAll];
bindingFor[head_Symbol]:=Replace[
	$bindingCache[HoldComplete[head]],
	_Missing :> ($bindingCache[HoldComplete[head]] = buildBinding[head])
];

(* Rules without patterns (memoized values) bind nothing and are left out *)
buildBinding[head_Symbol]:=Module[
	{held,infos,tracked},
	held = Map[HoldComplete,Select[DownValues[head],!FreeQ[#,Verbatim[Pattern]]&]];
	infos = Map[patternInfo,held];
	tracked = Map[Select[#,trackedContextQ[#["Context"]]&]&,infos];
	Association[
		"Function" -> fullName[head],
		"Held" -> held,
		"Names" -> Map[Lookup[#,"Name",{}]&,tracked],
		"Rules" -> MapThread[bindingRule,{held,tracked,Range[Length[held]]}]
	]
];

(* HoldComplete[lhs] :> {index, HoldComplete[inputs...]}, built with Apply so no scoping construct renames the pattern names *)
bindingRule[HoldComplete[Verbatim[RuleDelayed][lhs_,_]],tracked_List,index_Integer]:=Apply[
	RuleDelayed,
	{HoldComplete[lhs],{index,heldValues[tracked]}}
];

heldValues[{}]:=HoldComplete[];
heldValues[tracked_List]:=Apply[Join,Lookup[tracked,"Held"]];

(* The pattern variables of a rule's left-hand side: log name, context, and the symbol held (a list for sequence patterns) *)
patternInfo[held_HoldComplete]:=DeleteDuplicatesBy[
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

(* Sequence patterns (x__, opts:OptionsPattern[]) are logged as a list of what they matched *)
sequencePatternQ[(Verbatim[PatternTest]|Verbatim[Condition])[pattern_,_]]:=sequencePatternQ[pattern];
sequencePatternQ[_BlankSequence|_BlankNullSequence|_OptionsPattern|_Repeated|_RepeatedNull]:=True;
sequencePatternQ[_]:=False;

(* {names, HoldComplete[values], observed Function} for the tracked parameters of an applied Function; slot functions have none *)
pureFunctionBinding[HoldForm[(function:Function[parameters:(Except[Null,_Symbol]|{___Symbol}),__])[arguments___]]]:=Module[
	{infos,positions},
	infos = parameterInfo[HoldComplete[parameters]];
	positions = Select[
		Flatten[Position[Map[trackedContextQ[#["Context"]]&,infos],True]],
		# <= Length[HoldComplete[arguments]]&
	];
	If[positions === {},
		None,
		{infos[[positions,"Name"]],HoldComplete[arguments][[positions]],HoldComplete[function]}
	]
];
pureFunctionBinding[_]:=None;

parameterInfo[HoldComplete[symbol_Symbol]]:={variableInfo[symbol]};
parameterInfo[HoldComplete[symbols:{___Symbol}]]:=Cases[HoldComplete[symbols],symbol_Symbol :> variableInfo[symbol],{2}];

(* A bound symbol's log name, its context (to check tracking) and the symbol, held *)
variableInfo[symbol_Symbol]:=Association[
	"Name" -> normalizeName[symbolString[symbol]],
	"Context" -> symbolContext[symbol],
	"Held" -> HoldComplete[symbol]
];

(*
	With constants: a With step's direct children are its head, then one step per initializer value, then the body,
	so the 2nd..(n+1)th child to finish carries the value of the 1st..nth constant
*)
startWith[id_,event:HoldForm[With[variables_List,_]]]:=$withState[id] = {
	Cases[HoldComplete[variables],Verbatim[Set][symbol_Symbol,_] :> variableInfo[symbol],{2}],
	0,
	Apply[HoldComplete,event]
};

withChildDone[withId_,HoldComplete[value_]]:=Module[
	{infos,count,observed},
	{infos,count,observed} = $withState[withId];
	count = count+1;
	$withState[withId] = {infos,count,observed};
	If[And[$trackInputs,2 <= count <= Length[infos]+1,trackedContextQ[infos[[count-1,"Context"]]]],
		Sow[
			{UnixTime[],infos[[count-1,"Name"]],Hold[value],currentLocation["With",observed],infos[[count-1,"Name"]]},
			"assignment"
		]
	]
];


(* ::Subsection::Closed:: *)
(*Assignments*)


(* A Set's own evaluation shows up twice (as written, then with its value): only the outer one is logged *)
innerSetQ[HoldForm[Verbatim[Set][lhs_,_]]]:=And[
	$frames =!= {},
	$frames[[1,3]] === "Set",
	Extract[$frames[[1,2]],{1,1},HoldComplete] === HoldComplete[lhs]
];
innerSetQ[_]:=False;

(* The Set as written, with Module's Unevaluated[...] around an initializer value removed, for matching against source *)
observedSet[event_HoldForm]:=Replace[
	Apply[HoldComplete,event],
	HoldComplete[Verbatim[Set][lhs_,Verbatim[Unevaluated][rhs_]]] :> HoldComplete[Set[lhs,rhs]]
];

logSet[event:HoldForm[Verbatim[Set][lhs_,_]],HoldComplete[value_]]:=logAssignment[HoldComplete[lhs],HoldComplete[value],observedSet[event]];

logAssignment[HoldComplete[symbol_Symbol],HoldComplete[value_],observed_]:=If[trackedContextQ[Context[symbol]],
	With[{raw = ToString[HoldForm[symbol]]},
		Sow[{UnixTime[],logName[raw],Hold[value],currentLocation["Set",observed],raw},"assignment"]
	]
];
logAssignment[HoldComplete[symbols:{___Symbol}],HoldComplete[values_List],observed_]:=Do[
	logAssignment[Extract[HoldComplete[symbols],{1,index},HoldComplete],Extract[HoldComplete[values],{1,index},HoldComplete],observed],
	{index,Min[Length[Unevaluated[symbols]],Length[values]]}
];
(*
	Assignments into part of a variable: x[[i]] = v, and assoc["k"] = v when assoc holds an Association, logged as
	x[[i]] / assoc["k"]. s[...] = v on a symbol without an Association value is a definition (e.g. memoization), not a variable.
*)
logAssignment[HoldComplete[target:Verbatim[Part][base_Symbol,___]],HoldComplete[value_],observed_]/;OwnValues[base] =!= {}:=
	logPartAssignment[HoldComplete[target],HoldComplete[base],HoldComplete[value],observed];
logAssignment[HoldComplete[target:(base:Except[List|Part,_Symbol])[___]],HoldComplete[value_],observed_]/;MatchQ[OwnValues[base],{_ :> _Association}]:=
	logPartAssignment[HoldComplete[target],HoldComplete[base],HoldComplete[value],observed];
logAssignment[___]:=Null;

logPartAssignment[HoldComplete[target_],HoldComplete[base_Symbol],HoldComplete[value_],observed_]:=If[trackedContextQ[Context[base]],
	With[{raw = partName[target,base]},
		Sow[{UnixTime[],logName[raw],Hold[value],currentLocation["Mutation",observed],raw},"assignment"]
	]
];

(* +=, ++, AppendTo, AssociateTo, ...: log the variable's whole new value *)
logMutationEvent[event:HoldForm[_[target_,___]]]:=mutationTarget[HoldComplete[target],Apply[HoldComplete,event]];

mutationTarget[HoldComplete[symbol_Symbol],observed_]:=logMutationValue[HoldComplete[symbol],observed];
mutationTarget[HoldComplete[Verbatim[Part][symbol_Symbol,___]],observed_]:=logMutationValue[HoldComplete[symbol],observed];
mutationTarget[HoldComplete[(symbol:Except[List|Part,_Symbol])[___]],observed_]/;MatchQ[OwnValues[symbol],{_ :> _Association}]:=
	logMutationValue[HoldComplete[symbol],observed];
mutationTarget[___]:=Null;

logMutationValue[HoldComplete[symbol_Symbol],observed_]:=If[trackedContextQ[Context[symbol]],
	With[{raw = ToString[HoldForm[symbol]]},
		Sow[{UnixTime[],logName[raw],Hold[Evaluate[symbol]],currentLocation["Mutation",observed],raw},"assignment"]
	]
];

(*
	Drops repeated reports of one assignment (the same runtime symbol and value back to back, e.g. AppendTo seen both as
	itself and as the Set it performs), keeping the last, then the raw name. Inputs are never merged.
*)
dropRepeatedTraced[items_List]:=Map[
	Take[#,4]&,
	Map[
		Last,
		Split[
			items,
			And[
				MatchQ[#1,{_,_,_,DebuggerLocation[_,"Set"|"Mutation",___],_}],
				MatchQ[#2,{_,_,_,DebuggerLocation[_,"Set"|"Mutation",___],_}],
				#1[[{3,5}]] === #2[[{3,5}]]
			]&
		]
	]
];

(* Shown in the log as <<function Kind>> *)
Format[DebuggerLocation[function_,kind_String,___]]:=Row[{
	"\[LeftSkeleton]",
	If[StringQ[function],shortName[function],"\[LongDash]"],
	" ",
	kind,
	"\[RightSkeleton]"
}];


(* ::Subsection::Closed:: *)
(*Source nodes*)


(* Positions in source order: a parent before its children, left before right *)
preOrder[positions_List]:=With[
	{depth = Max[0,Map[Length,positions]]},
	SortBy[positions,PadRight[#,depth,-1]&]
];

(* The symbols a Set, Function or With node binds *)
heldVariables[HoldComplete[Verbatim[Set][symbol_Symbol,_]]]:={variableInfo[symbol]};
heldVariables[HoldComplete[Verbatim[Set][symbols:{__Symbol},_]]]:=Cases[HoldComplete[symbols],symbol_Symbol :> variableInfo[symbol],{2}];
heldVariables[HoldComplete[Verbatim[Function][symbol_Symbol,__]]]:={variableInfo[symbol]};
heldVariables[HoldComplete[Verbatim[Function][symbols:{__Symbol},__]]]:=Cases[HoldComplete[symbols],symbol_Symbol :> variableInfo[symbol],{2}];
heldVariables[HoldComplete[Verbatim[With][variables_List,_]]]:=Cases[HoldComplete[variables],Verbatim[Set][symbol_Symbol,_] :> variableInfo[symbol],{2}];
heldVariables[_]:={};

$mutationHeads = Set|AddTo|SubtractFrom|TimesBy|DivideBy|Increment|Decrement|PreIncrement|PreDecrement|AssociateTo|KeyDropFrom|AppendTo|PrependTo;

(* The variable a mutation node changes, or None. Set with a plain symbol on the left is an ordinary Set, not a mutation. *)
mutationInfo[HoldComplete[Verbatim[Set][target_,_]]]:=mutationBase[HoldComplete[target],False];
mutationInfo[HoldComplete[_[target_,___]]]:=mutationBase[HoldComplete[target],True];
mutationInfo[_]:=None;

(* mutationBase[target, plainSymbolQ]: the variable that target (x, x[[i]] or x[key]) changes; a bare x counts only when plainSymbolQ *)
mutationBase[HoldComplete[symbol_Symbol],True]:=variableInfo[symbol];
mutationBase[HoldComplete[Verbatim[Part][symbol_Symbol,___]],_]:=variableInfo[symbol];
(* symbol[key]: a variable holding an Association, never a function (a symbol with definitions, e.g. memoization) *)
mutationBase[HoldComplete[(symbol:Except[List|Part,_Symbol])[___]],_]/;DownValues[symbol] === {}:=variableInfo[symbol];
mutationBase[___]:=None;

(*
	Which node of the rule made an entry: the nodes of the entry's kind binding the variable, in source order, and the
	one whose form matches what the run showed. Its position among them is the ordinal of its line among the textual
	matches of the variable in the overload; Missing when no single node matches.
*)
siteOrdinal[rule_HoldComplete,kind_String,variable_String,observed_HoldComplete]:=Module[
	{rhsQ,sites,matching},
	rhsQ = MatchQ[#,{1,2,___}]&;
	sites = Switch[kind,
		"Set"|"With",
			Select[
				preOrder[Select[Position[rule,Verbatim[Set][_Symbol|{__Symbol},_]],rhsQ]],
				MemberQ[Lookup[heldVariables[Extract[rule,#,HoldComplete]],"Name"],variable]&
			],
		"Mutation",
			Select[
				preOrder[Select[Position[rule,$mutationHeads[_,___]],rhsQ]],
				With[{info = mutationInfo[Extract[rule,#,HoldComplete]]},And[AssociationQ[info],info["Name"] === variable]]&
			],
		"Function",
			Select[
				preOrder[Select[Position[rule,Verbatim[Function][Except[Null,_Symbol]|{__Symbol},__]],rhsQ]],
				MemberQ[Lookup[heldVariables[Extract[rule,#,HoldComplete]],"Name"],variable]&
			],
		_,
			{}
	];
	(* A With constant is matched by its With node, two levels above its initializer *)
	matching = Select[
		Range[Length[sites]],
		siteMatchQ[observed,Extract[rule,If[kind === "With",Drop[sites[[#]],-2],sites[[#]]],HoldComplete]]&
	];
	Which[
		Length[matching] === 1,First[matching],
		Length[sites] === 1,1,
		True,Missing["Ambiguous"]
	]
];
siteOrdinal[___]:=Missing["Ambiguous"];

(*
	The node as a pattern: local symbols (renamed Module variables and substituted inputs at run time) match any one
	expression, also as a head as in assoc["k"], and any pattern syntax written in the code is made literal
*)
$patternConstructs = Blank|BlankSequence|BlankNullSequence|Pattern|Condition|PatternTest|Alternatives|Repeated|RepeatedNull|Optional|Except|Verbatim|HoldPattern|OptionsPattern|PatternSequence|Longest|Shortest|KeyValuePattern;

siteMatchQ[observed_HoldComplete,node_HoldComplete]:=Quiet[
	MatchQ[
		observed,
		ReplaceAll[
			ReplaceAll[node,construct:$patternConstructs :> Verbatim[construct]],
			symbol_Symbol/;localSymbolQ[symbol] :> Blank[]
		]
	]
];

(* A symbol with no definitions and no value outside the system: a variable or input as written, never a function *)
SetAttributes[localSymbolQ,HoldAll];
localSymbolQ[symbol_Symbol]:=And[
	!StringStartsQ[Context[symbol],"System`"],
	DownValues[symbol] === {},
	OwnValues[symbol] === {},
	SubValues[symbol] === {}
];

(* The short names of the pattern variables on a rule's left-hand side, used to pick its overload in the source *)
rulePatternNames[rule_HoldComplete]:=DeleteDuplicates[Map[shortName,Lookup[patternInfo[rule],"Name",{}]]];
rulePatternNames[_]:={};


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

(* $DebuggerSourceFiles when set, otherwise the function's package sources from Packager *)
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

(* Every definition block (one per overload) of function across its source files *)
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

(* A block's lines with comments and strings blanked out *)
blockLines[block_Association]:=With[
	{lines = fileLines[block["File"]]},
	stripCode[lines[[block["Start"];;Min[block["End"],Length[lines]]]]]
];


(* ::Subsection::Closed:: *)
(*Matching source*)


(* A variable name only matches whole: not preceded or followed by identifier characters *)
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

(* A definition's left-hand side (its text up to :=) and the pattern names in it, used to tell overloads apart *)
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

(* The trimmed text of one source line *)
sourceLine[{file_String,line_Integer}]:=With[
	{lines = fileLines[file]},
	If[1 <= line <= Length[lines],StringTrim[lines[[line]]],Missing["NotAvailable"]]
];


(* ::Subsection::Closed:: *)
(*Resolving*)


(*
	File, line and code for one log entry: find the function's source blocks, pick the overload by the pattern names of
	the rule that ran, then find which node of that rule made the entry and take the textual match at its ordinal.
*)
resolveLocation[DebuggerLocation[function_String,kind_String,rule_,observed_,call_],variableName_String]:=Module[
	{variable,base,blocks,chosen,matches,ordinal,location},

	variable = baseName[variableName];
	base = Join[
		Association["Function" -> function,"Kind" -> kind],
		If[IntegerQ[call],Association["Call" -> call],Association[]]
	];

	blocks = sourceBlocks[function];
	If[blocks === {},
		Return[Join[base,Association["File" -> Missing["SourceNotFound"],"Line" -> Missing["SourceNotFound"]]]]
	];

	chosen = chooseOverloads[blocks,rulePatternNames[rule]];
	ordinal = If[kind === "Pattern",Missing[],siteOrdinal[rule,kind,variable,observed]];
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
resolveLocation[DebuggerLocation[None,kind_String,___],_]:=Association[
	"Function" -> Missing["NotInTrackedFunction"],
	"Kind" -> kind,
	"File" -> Missing["NotInTrackedFunction"],
	"Line" -> Missing["NotInTrackedFunction"]
];
resolveLocation[missing_,_]:=Association["Function" -> missing,"File" -> missing,"Line" -> missing];

(* The AssignmentLog of the last run, or {} *)
assignmentLog[]:=Replace[
	DebuggerInformation,
	{information_Association :> Lookup[information,"AssignmentLog",{}],_ -> {}}
];

(* An entry with its resolved location, as DebuggerWhere returns it *)
describeEntry[entry_List,index_Integer]:=Join[
	Association["Index" -> index,"Variable" -> entry[[2]]],
	resolveLocation[If[Length[entry] >= 4,entry[[4]],Missing["NotTracked"]],entry[[2]]],
	Association["Value" -> entry[[3]]]
];

(* The last entry with a location, or the last entry when none has one *)
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

(* Display helpers: a clickable label when $DebuggerLinkTemplate is set, and "function  File.m:line" *)
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

(* Printed when a run is aborted, or stopped by a message while TrackLocations is on *)
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

(* DebuggerInputs: entries logged from the pattern inputs of the given function, grouped by call *)
functionNameMatchQ[name_String,query_String]:=Or[
	name === query,
	And[StringFreeQ[query,"`"],shortName[name] === query]
];

inputEntryQ[entry_List,function_String]:=MatchQ[
	entry,
	{_,_,_,DebuggerLocation[name_String,"Pattern",_,_,_Integer]}/;functionNameMatchQ[name,function]
];

DebuggerInputs[function_String]:=With[
	{entries = Select[assignmentLog[],inputEntryQ[#,function]&]},
	Values[
		GroupBy[
			entries,
			(#[[4,5]]&) -> (shortName[#[[2]]] -> ReleaseHold[#[[3]]]&),
			Association
		]
	]
];
DebuggerInputs[symbol_Symbol]:=DebuggerInputs[fullName[symbol]];


(* ::Section::Closed:: *)
(*End Package*)


End[];
EndPackage[];
