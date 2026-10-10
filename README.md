# mathematica-debugger

This package provides an implementation of a debugger for Wolfram's _Mathematica_.

## Installation

1. Clone repo
	```
	$ git clone git@github.com:teedr/mathematica-debugger.git
	```

2. Link or move the _Debugger_ folder to a folder listed in your _Mathematica_ `$Path`

	_Mac:_
	```
	$ cd <Some dir listed in your Mathematica $Path>
	$ ln -s <path to where you cloned mathematica-debugger>/mathematica-debugger/Debugger .
	```
	
	_Windows:_ Open cmd.exe *as administrator* (Open Start menu, search for cmd.exe, right click, select "Run as administrator")
	```
	$ cd <Some dir listed in your Mathematica $Path>
	$ mklink /D Debugger <path to where you cloned mathematica-debugger>\mathematica-debugger\Debugger
	```
	
3. Use ``Get["Debugger`"]`` or ``Needs["Debugger`"]`` to load the package in _Mathematica_

## Usage

* Wrap `Debugger` around a code block

* After execution, the symbol `DebuggerInformation` will be populated with keys: 
	* _"AssignmentLog"_
		* List of `{time, variable, Hold[value], location}` entries (in order of assignment) for each variable assigned during execution
		* `location` is a `DebuggerLocation[...]` tag when `TrackInputs`/`TrackLocations` are on, otherwise `Missing[...]`
	* _"CurrentAssignments"_
		* Association mapping each assigned variable to its current value
	* _"LastAssignment"_
		* The last variable name set during execution
	* _"Failures"_
		* A list of any messages represented by `Failure` objects
	* _"MessageStacks"_
		* `Stack[]` at the moment each message was issued


* All variables in _DebuggerInformation_ Associations will be ToString-ed and include Context

### Options 

* Populate the `DebuggerContexts` option with a list of contexts from which variables should be tracked
	> Defaults to `DebuggerContexts -> $DebuggerContexts` (where `$DebuggerContexts` is set by default to `{"Global`"}`)
	
* Use the `AbortOnMessage` option to stop evaluation upon the first message thrown
	> Defaults to `AbortOnMessage -> True`
	
* Use the `BreakOnAssert` option to interrupt evaluation on failed assertions (ie: `Assert[False]` can be used like a breakpoint)
	> Defaults to `BreakOnAssert -> False`
	
* Use the `ModuleNumbers` option to toggle module number suffix on variable names
	> Defaults to `ModuleNumbers -> False`

* Use the `TrackInputs` option to also log values that are bound without `Set`
	> Defaults to `TrackInputs -> False`
	* Function pattern inputs: `mySamples` in `ExperimentFoo[mySamples:ListableP[...], myOptions:OptionsPattern[]] := ...`
	* `Function` parameters: `element` in `Map[Function[{element}, ...], list]`
	* `With` constants: `doubled` in `With[{doubled = 2 a}, ...]`
	* Sequence patterns (`x__`, `opts:OptionsPattern[]`) are logged as a list of what they matched; inputs of `HoldAll` functions are logged unevaluated

* Use the `TrackLocations` option to record where each tracked assignment happened (function, file and line)
	> Defaults to `TrackLocations -> False`

* Use the `InstrumentSymbols` option to limit which functions are rewritten for `TrackInputs`/`TrackLocations`
	> Defaults to `InstrumentSymbols -> Automatic`: every function in `DebuggerContexts` plus the public functions of those packages

Set defaults once with `SetOptions`, e.g. `SetOptions[Debugger, TrackInputs -> True, TrackLocations -> True]`.

### Inputs and locations

With `TrackInputs`/`TrackLocations` on, every function under debug is rewritten the first time it is called inside `Debugger` (pattern inputs are logged from a `Condition` around the rule's left-hand side, `Function`/`With` bodies log their values first, and every `Set` value is prefixed with a marker). The rewrite lives in an `Internal`InheritedBlock`, so the original definitions are back as soon as `Debugger` returns; rewrites are cached until a function's definitions change. `ClearDebuggerCache[]` drops the cache.

* `DebuggerWhere[]` — function, file, line and code of the last tracked assignment (after a message: where you actually were)
* `DebuggerWhere[n]` — the same for entry `n` of the AssignmentLog (negative `n` counts from the end)
* `DebuggerWhere["var"]` — every assignment of `var`, each with its location (the context may be omitted)
* `DebuggerHistory[n]` — table of the last `n` assignments with function and `file:line`; `DebuggerHistory["var"]` is the same table for every assignment of `var`
* Aborting a run (`Abort[]`, Alt+.) restores the original definitions the same way a normal return does
* `DebuggerInputs[f]` — the pattern inputs of every call to `f`, one association per call
* When a message aborts a run with `TrackLocations -> True`, the last tracked assignment is printed

Locations are found by matching the rewritten rule back to its source text: the overload is chosen by its pattern names, and the n-th `x = ...` in that overload is the n-th `Set` of `x` in the rule. `$DebuggerSourceFiles` says where to look:

* `Automatic` (default): asks Packager (`FunctionPackage`/`PackageSources`) when it is loaded, otherwise nothing is found
* a list of files, or a function from a function's full name to its files

Set `$DebuggerLinkTemplate` to a `StringTemplate` string with `File` and `Line` slots, e.g. ``"vscode://file/`File`:`Line`"``, to make locations clickable.

### Breakpoints

* When `BreakOnAssert` is `True`, the Interrupt dialog is displayed upon execution of `Assert[False]`
* Click _Abort_ in the Interrupt dialog to kill the evaluation
* Click _Enter Subsession_ in Interrupt dialog to free the kernel for evaluations
* Once the dialog is dismissed, evaluation must be controlled by the Debugger Controls
	* CTRL + Shift + H: Halt
	* CTRL + Shift + C: Continue
	* CTRL + Shift + F: Finish

## Examples

### Without messages
Example function definition:
```
func[x_Integer]:=Module[
	{foo, bar},
	
	foo = x;
	bar = foo + 1;
	foo = x + bar;
	
	{foo, bar}
];
```
Use Debugger in function execution
```
In[1]:= Debugger[func[3]]

Out[1]= {7, 4}
```
Query `DebuggerInformation` to see variable set history
```
In[2]:= DebuggerInformation

Out[2]= Association[
	"AssignmentLog" -> {
		(* foo had multiple assignments *)
		{1487198514, foo, 3},
		{1487198515, bar, 4},
		{1487198516, foo, 7}
	},
	"CurrentAssignments" -> Association[
		foo -> 7,
		bar -> 4
	],
	"LastAssignment" -> Rule[
		foo,
		7
	],
	"Failures" -> {}
]
```

### With messages
Example function definition:
```
func::err:="Error!";
func[x_Integer]:=Module[
	{foo, bar},
	
	foo = x;
	bar = foo + 1;
	
	Message[func::err];
	
	foo = x + bar;
	
	{foo, bar}
];
```
Use Debugger in function execution
```
In[1]:= Debugger[func[3],ModuleNumbers->True]
	func:Error!

Out[1]= $Aborted[]
```
Query `DebuggerInformation` to see variable set history
```
In[2]:= DebuggerInformation

Out[2]= Association[
	"AssignmentLog" -> {
		(* execution aborted before foo's second assignment *)
		{1487198514, foo$577, 3},
		{1487198514, bar$577, 4}
	},
	"CurrentAssignments" -> Association[
		foo$577 -> 3,
		bar$577 -> 4
	],
	"LastAssignment" -> Rule[
		bar$577,
		4
	],
	"Failures" -> {
		Failure[
			func,
			Association[
				"MessageTemplate" :> func::err,
				"MessageParameters" -> {}
			]
		]
	}
]
```


### With breakpoints
Example function definition:
```
func::err:="Error!";
func[x_Integer]:=Module[
	{foo, bar},
	
	foo = x;
	bar = foo + 1;
	
	Assert[False];
	
	foo = x + bar;
	
	{foo, bar}
];
```
Use Debugger in function execution
```
In[1]:= Debugger[func[3], BreakOnAssert -> True, ModuleNumbers -> True]
	"Breakpoint at line: 3 of file: README.md"

```
Select _Enter Subsession_ in Interrupt dialog and query `DebuggerInformation` to see variable state at breakpoint
```
(Dialog) In[2]:= DebuggerInformation

(Dialog) Out[2]= Association[
	"AssignmentLog" -> {
		(* execution paused before foo's second assignment *)
		{1487198514, foo$577, 3},
		{1487198514, bar$577, 4}
	},
	"CurrentAssignments" -> Association[
		foo$577 -> 3,
		bar$577 -> 4
	],
	"LastAssignment" -> bar$577,
	"Failures" -> {}
]
```
CTRL + Shift + C to continue execution
```
In[1]:= Debugger[func[3], BreakOnAssert -> True]

Out[1]= {7, 4}
```

## Tests

```
TestReport[FileNameJoin[{$HomeDirectory, "mathematica-debugger", "Tests", "DebuggerTests.wlt"}]]
```

## Known Issues
* There is an issue where an `Unset::write` error is thrown from the Debugger after aborting from a message. It doesn't seem to affect any functionality but tracking down the error's source is ongoing.
* The log is collected with `Sow`, so code under debug that calls `Reap[expr]` without a tag pattern also reaps the Debugger's entries (and returns them to that code).
* `TrackInputs`/`TrackLocations` cost time: the first call of each function rewrites it, and every call logs its inputs. Narrow it with `InstrumentSymbols` when a run is too slow.
* `Function` objects created during a tracked run carry their logging call; if one is returned or stored, it still runs correctly but logs nothing outside `Debugger`.
* Locations come from the source files on disk; if a file was edited without reloading, lines can be off.
* `WithSetHandler` does not report `Module[{a = 1}, ...]` initializers, so untracked runs never log them; with `TrackInputs` or `TrackLocations` on, rewritten functions log their initializers themselves.
* On Mathematica 14.2 `WithSetHandler` reports every `Set` twice. Identical back-to-back reports of the same runtime symbol are collapsed into one entry.

## Contributing

Please! Fork this repository and open a pull request. Some potential future developments include:

* Log SetDelayed calls
* Figure out some way to populate `DebuggerInformation` upon `Assert[False]` without compromising Reap/Sow performance
* Perhaps some sort of Debugger dialog
* More stuff I can't think of
