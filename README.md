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
		* List of timestamped assignments (in order of assignment) for each variable assigned during execution
	* _"CurrentAssignments"_
		* Association mapping each assigned variable to its current value
	* _"LastAssignment"_
		* The last variable name set during execution
	* _"Failures"_
		* A list of any messages represented by `Failure` objects


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

## Known Issues
* There is an issue where an `Unset::write` error is thrown from the Debugger after aborting from a message. It doesn't seem to affect any functionality but tracking down the error's source is ongoing.

## Contributing

Please! Fork this repository and open a pull request. Some potential future developments include:

* Log SetDelayed calls / Log function calls
* Log what function was responsible for an assignment
* Figure out some way to populate `DebuggerInformation` upon `Assert[False]` without compromising Reap/Sow performance
* Perhaps some sort of Debugger dialog
* More stuff I can't think of
