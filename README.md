# mathematica-debugger

This package provides an implementation of a debugger for Wolfram's Mathematica.

## Installation

1. Link or move the _Debugger_ folder to a folder listed in `$Path`
2. Use ``Get["Debugger`"]`` or ``Needs["Debugger`"]`` to load the package

## Usage

* Wrap `Debugger` around a code block

* After execution, the symbol `DebugInformation` will be populated with keys: 
	* _"AssignmentLog"_
		* Association mapping each assigned variable to a list of all values throughout the execution of the code block)
	* _"CurrentAssignments"_
		* Association mapping each assigned variable to its current value
	* _"LastAssignment"_
		* The last variable name set during execution
	* _"Failures"_
		* A list of any messages represented by `Failure` objects
		

* Populate the _IgnoreContexts_ option with a list of context names to ignore variables in specific contexts

* Use the _AbortOnMessage_ option to stop evaluation upon the first message thrown

## Contributing

Please fork this repository and open a pull request. Future development plan to be added soon.
