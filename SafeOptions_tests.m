(* ::Package:: *)

(* ::Title:: *)
(*Package Tests*)


(* ::Chapter:: *)
(*Test functions*)


allTests = {};

doTest[f_, output_] := Block[{testResult},
   testResult = VerificationTest[f, output];
   AppendTo[allTests, testResult];
   
   If[testResult["Outcome"] == "Success",
    	
    	Print["OK: ", f],
    	
    	Print[Style["FAILURE", 40, Bold, Red]];
    	Print["Result  : ", f];
    	Print["Expected: ", output];
    	];
   
   AppendTo[allTests, testResult];
   Return[f];
   ];


(* ::Chapter:: *)
(*Tests*)


<<SafeOptions`


(* ::Text:: *)
(*Required by Catch[...] to check expected Assert[] failures*)


On[Assert]
$AssertFunction=Throw[$Failed]&;


(* ::Text:: *)
(*User functions*)


?"SafeOptions`*"


Clear[a];Clear[A];
Clear[b];
Clear[c];


(* ::Subchapter:: *)
(*does it work?*)


doTest[1,1];


(* ::Subchapter:: *)
(*normalizeOptionPattern[]*)


<<SafeOptions`
?normalizeOptionPattern


foo[opts : OptionsPattern[]] := normalizeOptionPattern[opts];

doTest[foo[], {}];
doTest[foo[{}], {}];
doTest[foo[{{}}], {}];
doTest[foo[a -> 1], {a -> 1}];
doTest[foo[{a -> 1}], {a -> 1}];
doTest[foo[{{a -> 1}}], {a -> 1}];
doTest[foo[a -> 1, b -> 2], {a -> 1, b -> 2}];
doTest[foo[{a -> 1, b -> 2}], {a -> 1, b -> 2}];
doTest[foo[{{a -> 1, b -> 2}}], {a -> 1, b -> 2}];


(* ::Subchapter:: *)
(*normalizedOptionListQ[]*)


<<SafeOptions`
?normalizedOptionListQ


doTest[normalizedOptionListQ[1,2], False];
doTest[normalizedOptionListQ[], False];
doTest[normalizedOptionListQ[{}], True];
doTest[normalizedOptionListQ[{{}}], False];
doTest[normalizedOptionListQ[a -> 1], False];
doTest[normalizedOptionListQ[{a -> 1}], True];
doTest[normalizedOptionListQ[{{a -> 1}}], False];
doTest[normalizedOptionListQ[a -> 1, b -> 2], False];
doTest[normalizedOptionListQ[{a -> 1, b -> 2}], True];
doTest[normalizedOptionListQ[{{a -> 1, b -> 2}}], False];


(* ::Subchapter:: *)
(*hasOptionQ[]*)


<<SafeOptions`
?hasOptionQ


doTest[hasOptionQ[a], hasOptionQ[a]];
doTest[hasOptionQ[{},a], False];
doTest[hasOptionQ[{{}},a], hasOptionQ[{{}},a]];
doTest[hasOptionQ[a->1,a], hasOptionQ[a->1,a]];
doTest[hasOptionQ[{a -> 1},a], True];
doTest[hasOptionQ[{{a -> 1}},a], hasOptionQ[{{a -> 1}},a]];
doTest[hasOptionQ[a -> 1, b -> 2,a],hasOptionQ[a -> 1, b -> 2,a]];
doTest[hasOptionQ[a -> 1, b -> 2,a -> 3,a], hasOptionQ[a -> 1, b -> 2,a -> 3,a]]; (* caveat: also see HasUniqueOptionQ[] *)
doTest[hasOptionQ[{a -> 1, b -> 2},a], True];
doTest[hasOptionQ[{a -> 1, b -> 2,a -> 1},a], True];
doTest[hasOptionQ[{{a -> 1, b -> 2}},a], hasOptionQ[{{a -> 1, b -> 2}},a]];


(* ::Subchapter:: *)
(*hasUniqueOptionQ[]*)


<<SafeOptions`
?hasUniqueOptionQ


doTest[hasUniqueOptionQ[a],hasUniqueOptionQ[a]];
doTest[hasUniqueOptionQ[{},a], False];
doTest[hasUniqueOptionQ[{{}},a], hasUniqueOptionQ[{{}},a]];
doTest[hasUniqueOptionQ[a -> 1,a], hasUniqueOptionQ[a -> 1,a]];
doTest[hasUniqueOptionQ[{a -> 1},a], True];
doTest[hasUniqueOptionQ[{{a -> 1}},a], hasUniqueOptionQ[{{a -> 1}},a]];
doTest[hasUniqueOptionQ[a -> 1, b -> 2,a], hasUniqueOptionQ[a -> 1, b -> 2,a]];
doTest[hasUniqueOptionQ[a -> 1, b -> 2,a -> 3,a], hasUniqueOptionQ[a -> 1, b -> 2,a -> 3,a]]; 
doTest[hasUniqueOptionQ[{a -> 1, b -> 2},a], True];
doTest[hasUniqueOptionQ[{a -> 1, b -> 2,a -> 1},a], False]; (* caveat *)
doTest[hasUniqueOptionQ[{{a -> 1, b -> 2}},a], hasUniqueOptionQ[{{a -> 1, b -> 2}},a]];


(* ::Subchapter:: *)
(*overwriteOptions[]*)


<<SafeOptions`
?overwriteOptions


doTest[overwriteOptions[c->3,1,2],overwriteOptions[c->3,1,2]];
doTest[overwriteOptions[c->3], overwriteOptions[c->3]];
doTest[Catch[overwriteOptions[{},c->3]], $Failed];
doTest[overwriteOptions[c->3,{{}}], overwriteOptions[c->3,{{}}]];
doTest[overwriteOptions[c->3,a -> 1],overwriteOptions[c->3,a -> 1]];
doTest[Catch[overwriteOptions[{a -> 1},c->3]], $Failed];
doTest[overwriteOptions[{a -> 1,c->2},c->3], {a->1,c->3}];
doTest[Catch[overwriteOptions[{a -> 1,c->2,c->4},c->3]], $Failed];
doTest[overwriteOptions[{a -> 1,b->{a->1}},a->A], {a->A,b->{a->1}}];
doTest[overwriteOptions[{a -> 1,b->{a->1}},a->A,3], {a->A,b->{a->A}}];
doTest[overwriteOptions[{a -> 1,b->{a->1}},a->A,{3}], {a->1,b->{a->A}}];


(* ::Subchapter:: *)
(*addOptions[]*)


<< SafeOptions`
?addOptions


doTest[addOptions[c -> 3, 1, 2], addOptions[c -> 3, 1, 2]];
doTest[addOptions[c -> 3], addOptions[c -> 3]];
doTest[addOptions[{},c -> 3], {c -> 3}];
doTest[addOptions[c -> 3, {{}}], addOptions[c -> 3, {{}}]];
doTest[addOptions[c -> 3, a -> 1], addOptions[c -> 3, a -> 1]];
doTest[addOptions[{c -> 1}, c -> 3], $Failed];
doTest[addOptions[{a -> 1, b -> 2},c -> 3], {a -> 1, b -> 2, c -> 3}];


(* ::Subchapter:: *)
(*updateOptions[]*)


<< SafeOptions`
?updateOptions


doTest[updateOptions[c -> 3, 1, 2], updateOptions[c -> 3, 1, 2]];
doTest[updateOptions[c -> 3], updateOptions[c -> 3]];
doTest[updateOptions[c -> 3, {{}}], updateOptions[c -> 3, {{}}]];
doTest[updateOptions[c -> 3, a -> 1], updateOptions[c -> 3, a -> 1]];
doTest[updateOptions[{a -> 1, c -> 2},c -> 3], {a -> 1, c -> 3}];
doTest[updateOptions[{a -> 1, c -> 2, c -> 4},c -> 3], $Failed];
doTest[updateOptions[{a -> 1, b -> {a -> 1}},a -> A], {a -> A, b -> {a -> 1}}];

doTest[updateOptions[c -> 3, 1, 2], updateOptions[c -> 3, 1, 2]];
doTest[updateOptions[c -> 3], updateOptions[c -> 3]];
doTest[updateOptions[ {},c -> 3], {c -> 3}];
doTest[updateOptions[ {{}},c -> 3], updateOptions[ {{}},c -> 3]];
doTest[updateOptions[ a -> 1,c -> 3], updateOptions[ a -> 1,c -> 3]];
doTest[updateOptions[ {c -> 1},c -> 3], { c -> 3}];
doTest[updateOptions[ {a -> 1, b -> 2},c -> 3], {a -> 1, b -> 2, c -> 3}];


(* ::Subchapter:: *)
(*SaferOptions`Private`checkDuplicateFreeOptionsQ[]*)


doTest[SafeOptions`Private`checkDuplicateFreeOptionsQ[{}], True];
doTest[SafeOptions`Private`checkDuplicateFreeOptionsQ[{a->1}], True];
doTest[SafeOptions`Private`checkDuplicateFreeOptionsQ[{a->1,b->2}], True];
doTest[SafeOptions`Private`checkDuplicateFreeOptionsQ[{a->1,a->2}], False]; (* must print error msg *)


(* ::Subchapter:: *)
(*checkgetOptionListQ[]*)


<<SafeOptions`
?SafeOptions`Private`checkgetOptionListQ


doTest[SafeOptions`Private`checkgetOptionListQ[{}], True];
doTest[SafeOptions`Private`checkgetOptionListQ[{}, {{a -> 1}}], False]; (* note: must print an error message, do not worry :) *)
doTest[SafeOptions`Private`checkgetOptionListQ[{a -> 2}, {{a -> 1}}], True];
doTest[SafeOptions`Private`checkgetOptionListQ[{a -> 2, b -> 3}, {{a -> 1}}], True];
doTest[SafeOptions`Private`checkgetOptionListQ[{a -> 2, b -> 3}, a -> 1], True];
doTest[SafeOptions`Private`checkgetOptionListQ[{a -> 2}, {{a -> 1, b -> 3}}], False]; (* print an error *)
doTest[SafeOptions`Private`checkgetOptionListQ[{a -> 2, b -> 3, a -> 4}, {{a -> 1}}], False]; (* print an error *)
doTest[SafeOptions`Private`checkgetOptionListQ[{a -> 2, b -> 3}, {{a -> 1, a -> 2}}], False]; (* print an error *)


(* ::Subchapter:: *)
(*filterOptions[]*)


<<SafeOptions`
?filterOptions


doTest[filterOptions[{}], {}];
doTest[filterOptions[{},{}], {}];
doTest[filterOptions[{a->1},{}], {}];
doTest[filterOptions[{a->1},{a->2}], {a->2}];
doTest[filterOptions[{},{a->2}], {}];
doTest[filterOptions[{b->3},{a->2}], {}];
doTest[filterOptions[{a->1,b->3},{a->2,b->4}], {a->2,b->4}];
doTest[Catch[filterOptions[{a->1,b->3,a->2},{a->2,b->4}]], $Failed]; (* must print error msg *)
doTest[Catch[filterOptions[{a->1,b->3},{a->2,b->4,a->2}]], $Failed]; (* must print error msg *)


(* ::Subchapter:: *)
(*getOptionList[]*)


<<SafeOptions`
?getOptionList


doTest[getOptionList[{}], {}];
doTest[getOptionList[{},{}], {}];
doTest[getOptionList[{a->1},{}], {a->1}];
doTest[getOptionList[{a->1},{a->2}], {a->2}];
doTest[getOptionList[{},{a->2}], $Failed];
doTest[getOptionList[{b->3},{a->2}], $Failed];
doTest[getOptionList[{a->1,b->3},{a->2,b->4}], {a->2,b->4}];
doTest[Catch[getOptionList[{a->1,b->3,a->2},{a->2,b->4}]], $Failed]; (* must print error msg *)
doTest[Catch[getOptionList[{a->1,b->3},{a->2,b->4,a->2}]], $Failed]; (* must print error msg *)


(* ::Subchapter:: *)
(*createOptionList[]*)


<<SafeOptions`
?createOptionList


doTest[createOptionList[{},{}],{}];
doTest[Catch[createOptionList[{a->1},{{}},optionsToIgnore->{}]],$Failed]; (* SaferOptions::unknownOptions: *)
doTest[Catch[createOptionList[{a->1},{{}},optionKeysToIgnore->{a}]],$Failed]; (* SaferOptions::cannotAddAndIgnore: *)
doTest[createOptionList[{a->1},{{}},optionKeysToIgnore->{b}], {a->1}]; 
doTest[createOptionList[{a->1},{{}},optionKeysToIgnore->{}], {a->1}]; 
doTest[createOptionList[{a->1},{{b->2},{c->3}},optionKeysToIgnore->{}],{a->1,b->2,c->3} ]; 
doTest[createOptionList[{a->1},{{b->2},{c->3}},optionKeysToIgnore->{b}],{a->1,c->3} ]; 
doTest[createOptionList[{a->1},{{b->2},{b->3}},optionKeysToIgnore->{b}],{a->1} ]; (* even if incompatible b opts, this is ok as b is ignored *)
doTest[Catch[createOptionList[{a->1},{{b->2},{b->3}},optionKeysToIgnore->{}]],$Failed ]; 


(* ::Chapter:: *)
(*Report (must be 100%)*)


TestReport[allTests]


(* ::Chapter:: *)
(*Demo*)


Options[foo1] = {a -> 1, b -> 2};

foo1[arg__, opts : OptionsPattern[]] := 
   Print["foo1 a=", OptionValue[a], " b=", OptionValue[b]]; 

Options[foo2] = {b -> -2};

foo2[arg__, opts : OptionsPattern[]] := 
   Print["foo2 b=", OptionValue[b]];


Options[foo3] = createOptionList[{a -> 10, b -> 1, c -> 4}, Options /@ {foo1, foo2}];

foo3[opts : OptionsPattern[]] :=      
  Block[{safeOpts = getOptionList[Options[foo3],opts]},
   foo1[1, 2,filterOptions[Options[foo1],safeOpts]]; 
   foo2[4, 5,filterOptions[Options[foo2],safeOpts]];
  ];


foo3[a->2,b->3]


Options[foo3] = createOptionList[{a -> 10, c -> 4}, Options /@ {foo1, foo2}];

foo3[opts : OptionsPattern[]] :=      
  Block[{safeOpts = getOptionList[Options[foo3],opts]},
   foo1[1, 2,filterOptions[Options[foo1],safeOpts]]; 
   foo2[4, 5,filterOptions[Options[foo2],safeOpts]];
  ];


createOptionList[{a -> 10, c -> 4}, Options /@ {foo1, foo2}]


Off[Assert]
