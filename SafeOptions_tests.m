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


(* ::Print:: *)
(*PaneSelector[{False -> RowBox[{OpenerBox[Dynamic[Typeset`open$$], ImageSize -> Small], ErrorBox["SafeOptions`"]}], True -> GridBox[{{RowBox[{OpenerBox[Dynamic[Typeset`open$$], ImageSize -> Small], ErrorBox["SafeOptions`"]}]}, {GridBox[{{ButtonBox["addOptions", BaseStyle -> "InformationLink", ButtonData :> {"Info3764791612-6331364", {"addOptions", "SafeOptions`"}}, ButtonNote -> "SafeOptions`"], ButtonBox["filterOptions", BaseStyle -> "InformationLink", ButtonData :> {"Info3764791612-6331364", {"filterOptions", "SafeOptions`"}}, ButtonNote -> "SafeOptions`"], ButtonBox["hasUniqueOptionQ", BaseStyle -> "InformationLink", ButtonData :> {"Info3764791612-6331364", {"hasUniqueOptionQ", "SafeOptions`"}}, ButtonNote -> "SafeOptions`"], ButtonBox["normalizedOptionListQ", BaseStyle -> "InformationLink", ButtonData :> {"Info3764791612-6331364", {"normalizedOptionListQ", "SafeOptions`"}}, ButtonNote -> "SafeOptions`"], ButtonBox["optionKeysToIgnore", BaseStyle -> "InformationLink", ButtonData :> {"Info3764791612-6331364", {"optionKeysToIgnore", "SafeOptions`"}}, ButtonNote -> "SafeOptions`"], ButtonBox["getOptions", BaseStyle -> "InformationLink", ButtonData :> {"Info3764791612-6331364", {"retrieveOptions", "SafeOptions`"}}, ButtonNote -> "SafeOptions`"], ButtonBox["updateOptions", BaseStyle -> "InformationLink", ButtonData :> {"Info3764791612-6331364", {"updateOptions", "SafeOptions`"}}, ButtonNote -> "SafeOptions`"]}, {ButtonBox["collectOptions", BaseStyle -> "InformationLink", ButtonData :> {"Info3764791612-6331364", {"collectOptions", "SafeOptions`"}}, ButtonNote -> "SafeOptions`"], ButtonBox["hasOptionQ", BaseStyle -> "InformationLink", ButtonData :> {"Info3764791612-6331364", {"hasOptionQ", "SafeOptions`"}}, ButtonNote -> "SafeOptions`"], ButtonBox["ignoreOption", BaseStyle -> "InformationLink", ButtonData :> {"Info3764791612-6331364", {"ignoreOption", "SafeOptions`"}}, ButtonNote -> "SafeOptions`"], ButtonBox["normalizeOptionPattern", BaseStyle -> "InformationLink", ButtonData :> {"Info3764791612-6331364", {"normalizeOptionPattern", "SafeOptions`"}}, ButtonNote -> "SafeOptions`"], ButtonBox["overwriteOptions", BaseStyle -> "InformationLink", ButtonData :> {"Info3764791612-6331364", {"overwriteOptions", "SafeOptions`"}}, ButtonNote -> "SafeOptions`"], ButtonBox["SaferOptions", BaseStyle -> "InformationLink", ButtonData :> {"Info3764791612-6331364", {"SaferOptions", "SafeOptions`"}}, ButtonNote -> "SafeOptions`"], ""}}, DefaultBaseStyle -> "InfoGrid", GridBoxItemSize -> {"Columns" -> {{Scaled[0.1357142857142857]}}}]}}]}, Dynamic[Typeset`open$$], ImageSize -> Automatic]*)


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


<<SafeOptions`
collectOptions[{a->1},{{}},optionsToIgnore->{b}]






