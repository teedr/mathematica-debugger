# mathematica-debugger

This package provides an implementation of a debugger for Wolfram's _Mathematica_.

## Installation

1. Clone repo
	```sh
	$ git clone git@github.com:teedr/mathematica-debugger.git
	```

2. Link or move the _Debugger_ folder to a folder listed in `$Path`
	```
	$ cd <Some dir listed in your Mathematica $Path>
	$ ln -s /path/to/mathematica-debugger/Debugger .
	```
	
3. Use ``Get["Debugger`"]`` or ``Needs["Debugger`"]`` to load the package in _Mathematica_

## Usage

* Wrap `Debugger` around a code block

* After execution, the symbol `DebuggerInformation` will be populated with keys: 
	* _"AssignmentLog"_
		* Association mapping each assigned variable to a list of all values throughout the execution of the code block)
	* _"CurrentAssignments"_
		* Association mapping each assigned variable to its current value
	* _"LastAssignment"_
		* The last variable name set during execution
	* _"Failures"_
		* A list of any messages represented by `Failure` objects
		

* Populate the _IgnoreContexts_ option with a list of context names to ignore variables in specific contexts
	> Defaults to `IgnoreContexts -> {"System"}`

* Use the _AbortOnMessage_ option to stop evaluation upon the first message thrown
	> Defaults to `AbortOnMessage -> True`

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
Interrogate `DebuggerInformation` to see variable set history
```
In[2]:= DebuggerInformation

Out[2]= Association[
	"AssignmentLog" -> Association[
		(* foo had multiple assignments *)
		foo$576 -> {3,7},
		bar$576 -> {4}
	],
	"CurrentAssignments" -> Association[
		foo$576 -> 7,
		bar$576 -> 4
	],
	"LastAssignment" -> foo$576,
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
In[1]:= Debugger[func[3]]
	func:Error!

Out[1]= $Aborted[]
```
Interrogate `DebuggerInformation` to see variable set history
```
In[2]:= DebuggerInformation

Out[2]= Association[
	"AssignmentLog" -> Association[
		(* execution aborted before foo's second assignment *)
		foo$577 -> {3},
		bar$577 -> {4}
	],
	"CurrentAssignments" -> Association[
		foo$577 -> 3,
		bar$577 -> 4
	],
	"LastAssignment" -> bar$577,
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

## Contributing

Please! Fork this repository and open a pull request. Some potential future developments include:

* Log SetDelayed calls / Log function calls
* Log what function was responsible for an assignment
* Figure out how to interrogate ``DebuggerInformation`` such that you don't have to know the ``$ModuleNumber``
* Figure out some way to have breakpoints like the `Assert[False]` trick in the _Mathematica_ debugger
* Perhaps some sort of Debugger dialog
* More stuff I can't think of
