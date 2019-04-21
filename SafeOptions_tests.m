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


doTest[hasOptionQ[a], False];
doTest[hasOptionQ[a,{}], False];
doTest[hasOptionQ[a,{{}}], False];
doTest[hasOptionQ[a,a -> 1], True];
doTest[hasOptionQ[a,{a -> 1}], True];
doTest[hasOptionQ[a,{{a -> 1}}], True];
doTest[hasOptionQ[a,a -> 1, b -> 2], True];
doTest[hasOptionQ[a,a -> 1, b -> 2,a -> 3], True]; (* caveat: also see HasUniqueOptionQ[] *)
doTest[hasOptionQ[a,{a -> 1, b -> 2}], True];
doTest[hasOptionQ[a,{{a -> 1, b -> 2}}], True];


(* ::Subchapter:: *)
(*hasUniqueOptionQ[]*)


<<SafeOptions`
?hasUniqueOptionQ


doTest[hasUniqueOptionQ[a], False];
doTest[hasUniqueOptionQ[a,{}], False];
doTest[hasUniqueOptionQ[a,{{}}], False];
doTest[hasUniqueOptionQ[a,a -> 1], True];
doTest[hasUniqueOptionQ[a,{a -> 1}], True];
doTest[hasUniqueOptionQ[a,{{a -> 1}}], True];
doTest[hasUniqueOptionQ[a,a -> 1, b -> 2], True];
doTest[hasUniqueOptionQ[a,a -> 1, b -> 2,a -> 3], False]; (* caveat *)
doTest[hasUniqueOptionQ[a,{a -> 1, b -> 2}], True];
doTest[hasUniqueOptionQ[a,{{a -> 1, b -> 2}}], True];


(* ::Subchapter:: *)
(*overwriteOptions[]*)


<<SafeOptions`
?overwriteOptions


doTest[overwriteOptions[c->3,1,2],overwriteOptions[c->3,1,2]];
doTest[overwriteOptions[c->3], overwriteOptions[c->3]];
doTest[Catch[overwriteOptions[c->3,{}]], $Failed];
doTest[overwriteOptions[c->3,{{}}], overwriteOptions[c->3,{{}}]];
doTest[overwriteOptions[c->3,a -> 1],overwriteOptions[c->3,a -> 1]];
doTest[Catch[overwriteOptions[c->3,{a -> 1}]], $Failed];
doTest[overwriteOptions[c->3,{a -> 1,c->2}], {a->1,c->3}];
doTest[Catch[overwriteOptions[c->3,{a -> 1,c->2,c->4}]], $Failed];
doTest[overwriteOptions[a->A,{a -> 1,b->{a->1}}], {a->A,b->{a->1}}];
doTest[overwriteOptions[a->A,{a -> 1,b->{a->1}},3], {a->A,b->{a->A}}];
doTest[overwriteOptions[a->A,{a -> 1,b->{a->1}},{3}], {a->1,b->{a->A}}];


(* ::Subchapter:: *)
(*addOptions[]*)


?addOptions


<< SafeOptions`
doTest[addOptions[c -> 3, 1, 2], addOptions[c -> 3, 1, 2]];
doTest[addOptions[c -> 3], addOptions[c -> 3]];
doTest[addOptions[c -> 3, {}], {c -> 3}];
doTest[addOptions[c -> 3, {{}}], addOptions[c -> 3, {{}}]];
doTest[addOptions[c -> 3, a -> 1], addOptions[c -> 3, a -> 1]];
doTest[Catch[addOptions[c -> 3, {c -> 1}]], $Failed];
doTest[addOptions[c -> 3, {a -> 1, b -> 2}], {a -> 1, b -> 2, c -> 3}];


(* ::Subchapter:: *)
(*updateOptions[]*)


<< SafeOptions`
?updateOptions


doTest[updateOptions[c -> 3, 1, 2], updateOptions[c -> 3, 1, 2]];
doTest[updateOptions[c -> 3], updateOptions[c -> 3]];
doTest[updateOptions[c -> 3, {{}}], updateOptions[c -> 3, {{}}]];
doTest[updateOptions[c -> 3, a -> 1], updateOptions[c -> 3, a -> 1]];
doTest[updateOptions[c -> 3, {a -> 1, c -> 2}], {a -> 1, c -> 3}];
doTest[Catch[updateOptions[c -> 3, {a -> 1, c -> 2, c -> 4}]], $Failed];
doTest[updateOptions[a -> A, {a -> 1, b -> {a -> 1}}], {a -> A, b -> {a -> 1}}];

doTest[updateOptions[c -> 3, 1, 2], updateOptions[c -> 3, 1, 2]];
doTest[updateOptions[c -> 3], updateOptions[c -> 3]];
doTest[updateOptions[c -> 3, {}], {c -> 3}];
doTest[updateOptions[c -> 3, {{}}], updateOptions[c -> 3, {{}}]];
doTest[updateOptions[c -> 3, a -> 1], updateOptions[c -> 3, a -> 1]];
doTest[updateOptions[c -> 3, {c -> 1}], { c -> 3}];
doTest[updateOptions[c -> 3, {a -> 1, b -> 2}], {a -> 1, b -> 2, c -> 3}];


(* ::Subchapter:: *)
(*SaferOptions`Private`checkDuplicateFreeOptionsQ[]*)


doTest[SafeOptions`Private`checkDuplicateFreeOptionsQ[{}], True];
doTest[SafeOptions`Private`checkDuplicateFreeOptionsQ[{a->1}], True];
doTest[SafeOptions`Private`checkDuplicateFreeOptionsQ[{a->1,b->2}], True];
doTest[SafeOptions`Private`checkDuplicateFreeOptionsQ[{a->1,a->2}], False]; (* must print error msg *)


(* ::Subchapter:: *)
(*checkGetOptionsQ[]*)


<<SafeOptions`
?SafeOptions`Private`checkGetOptionsQ


doTest[SafeOptions`Private`checkGetOptionsQ[{}], True];
doTest[SafeOptions`Private`checkGetOptionsQ[{}, {{a -> 1}}], False]; (* note: must print an error message, do not worry :) *)
doTest[SafeOptions`Private`checkGetOptionsQ[{a -> 2}, {{a -> 1}}], True];
doTest[SafeOptions`Private`checkGetOptionsQ[{a -> 2, b -> 3}, {{a -> 1}}], True];
doTest[SafeOptions`Private`checkGetOptionsQ[{a -> 2, b -> 3}, a -> 1], True];
doTest[SafeOptions`Private`checkGetOptionsQ[{a -> 2}, {{a -> 1, b -> 3}}], False]; (* print an error *)
doTest[SafeOptions`Private`checkGetOptionsQ[{a -> 2, b -> 3, a -> 4}, {{a -> 1}}], False]; (* print an error *)
doTest[SafeOptions`Private`checkGetOptionsQ[{a -> 2, b -> 3}, {{a -> 1, a -> 2}}], False]; (* print an error *)


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
(*getOptions[]*)


<<SafeOptions`
?getOptions


doTest[getOptions[{}], {}];
doTest[getOptions[{},{}], {}];
doTest[getOptions[{a->1},{}], {a->1}];
doTest[getOptions[{a->1},{a->2}], {a->2}];
doTest[getOptions[{},{a->2}], $Failed];
doTest[getOptions[{b->3},{a->2}], $Failed];
doTest[getOptions[{a->1,b->3},{a->2,b->4}], {a->2,b->4}];
doTest[Catch[getOptions[{a->1,b->3,a->2},{a->2,b->4}]], $Failed]; (* must print error msg *)
doTest[Catch[getOptions[{a->1,b->3},{a->2,b->4,a->2}]], $Failed]; (* must print error msg *)


(* ::Subchapter:: *)
(*collectOptions[]*)


<<SafeOptions`
?collectOptions


doTest[collectOptions[{},{}],{}];
doTest[Catch[collectOptions[{a->1},{{}},optionsToIgnore->{}]],$Failed]; (* SaferOptions::unknownOptions: *)
doTest[Catch[collectOptions[{a->1},{{}},optionKeysToIgnore->{a}]],$Failed]; (* SaferOptions::cannotAddAndIgnore: *)
doTest[collectOptions[{a->1},{{}},optionKeysToIgnore->{b}], {a->1}]; 
doTest[collectOptions[{a->1},{{}},optionKeysToIgnore->{}], {a->1}]; 
doTest[collectOptions[{a->1},{{b->2},{c->3}},optionKeysToIgnore->{}],{a->1,b->2,c->3} ]; 
doTest[collectOptions[{a->1},{{b->2},{c->3}},optionKeysToIgnore->{b}],{a->1,c->3} ]; 
doTest[collectOptions[{a->1},{{b->2},{b->3}},optionKeysToIgnore->{b}],{a->1} ]; (* even if incompatible b opts, this is ok as b is ignored *)
doTest[Catch[collectOptions[{a->1},{{b->2},{b->3}},optionKeysToIgnore->{}]],$Failed ]; 


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


Options[foo3] = collectOptions[{a -> 10, b -> 1, c -> 4}, Options /@ {foo1, foo2}];

foo3[opts : OptionsPattern[]] :=      
  Block[{safeOpts = getOptions[Options[foo3],opts]},
   foo1[1, 2,filterOptions[Options[foo1],safeOpts]]; 
   foo2[4, 5,filterOptions[Options[foo2],safeOpts]];
  ];


foo3[a->2,b->3]


Options[foo3] = collectOptions[{a -> 10, c -> 4}, Options /@ {foo1, foo2}];

foo3[opts : OptionsPattern[]] :=      
  Block[{safeOpts = getOptions[Options[foo3],opts]},
   foo1[1, 2,filterOptions[Options[foo1],safeOpts]]; 
   foo2[4, 5,filterOptions[Options[foo2],safeOpts]];
  ];


Off[Assert]
