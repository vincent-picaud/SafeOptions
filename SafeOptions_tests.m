(* ::Package:: *)

(* ::Title:: *)
(*Package Tests*)


(* ::Chapter:: *)
(*Test functions*)


allTests = {};

doTest[f_, output_, errorMsg_List: {}] := Block[{testResult},
      testResult = VerificationTest[f, output, errorMsg];
      AppendTo[allTests, testResult];
      
      If[testResult["Outcome"] == "Success",
        	
        	Print["OK: ", testResult["ActualOutput"]],
        	
        	Print[Style[testResult["Outcome"], 40, Bold, Red]];
        	If[testResult["Outcome"] == "MessagesFailure",
         	Print["Actual Message  : ", testResult["ActualMessages"]];
         	Print["Expected Message: ", testResult["ExpectedMessages"]]
         	];
        	If[testResult["Outcome"] == "Failure",
         	Print["Actual Output  : ", testResult["ActualOutput"]];
         	Print["Expected Output: ", testResult["ExpectedOutput"]]
         	];
    ];
      
      AppendTo[allTests, testResult];
      Return[f];
      ];
SetAttributes[doTest, HoldAllComplete];


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
(*PaneSelector[{False -> RowBox[{OpenerBox[Dynamic[Typeset`open$$], ImageSize -> Small], ErrorBox["SafeOptions`"]}], True -> GridBox[{{RowBox[{OpenerBox[Dynamic[Typeset`open$$], ImageSize -> Small], ErrorBox["SafeOptions`"]}]}, {GridBox[{{ButtonBox["appendOptions", BaseStyle -> "InformationLink", ButtonData :> {"Info1863765272179-6151880", {"appendOptions", "SafeOptions`"}}, ButtonNote -> "SafeOptions`"], ButtonBox["filterOptionList", BaseStyle -> "InformationLink", ButtonData :> {"Info1863765272179-6151880", {"filterOptions", "SafeOptions`"}}, ButtonNote -> "SafeOptions`"], ButtonBox["hasOptionQ", BaseStyle -> "InformationLink", ButtonData :> {"Info1863765272179-6151880", {"hasOptionQ", "SafeOptions`"}}, ButtonNote -> "SafeOptions`"], ButtonBox["ignoreOption", BaseStyle -> "InformationLink", ButtonData :> {"Info1863765272179-6151880", {"ignoreOption", "SafeOptions`"}}, ButtonNote -> "SafeOptions`"], ButtonBox["normalizeOptionPattern", BaseStyle -> "InformationLink", ButtonData :> {"Info1863765272179-6151880", {"normalizeOptionPattern", "SafeOptions`"}}, ButtonNote -> "SafeOptions`"], ButtonBox["overwriteOptions", BaseStyle -> "InformationLink", ButtonData :> {"Info1863765272179-6151880", {"overwriteOptions", "SafeOptions`"}}, ButtonNote -> "SafeOptions`"], ButtonBox["updateOptions", BaseStyle -> "InformationLink", ButtonData :> {"Info1863765272179-6151880", {"updateOptions", "SafeOptions`"}}, ButtonNote -> "SafeOptions`"]}, {ButtonBox["createOptionList", BaseStyle -> "InformationLink", ButtonData :> {"Info1863765272179-6151880", {"createOptionList", "SafeOptions`"}}, ButtonNote -> "SafeOptions`"], ButtonBox["getOptionList", BaseStyle -> "InformationLink", ButtonData :> {"Info1863765272179-6151880", {"getOptionList", "SafeOptions`"}}, ButtonNote -> "SafeOptions`"], ButtonBox["hasUniqueOptionQ", BaseStyle -> "InformationLink", ButtonData :> {"Info1863765272179-6151880", {"hasUniqueOptionQ", "SafeOptions`"}}, ButtonNote -> "SafeOptions`"], ButtonBox["normalizedOptionListQ", BaseStyle -> "InformationLink", ButtonData :> {"Info1863765272179-6151880", {"normalizedOptionListQ", "SafeOptions`"}}, ButtonNote -> "SafeOptions`"], ButtonBox["optionKeysToIgnore", BaseStyle -> "InformationLink", ButtonData :> {"Info1863765272179-6151880", {"optionKeysToIgnore", "SafeOptions`"}}, ButtonNote -> "SafeOptions`"], ButtonBox["SaferOptions", BaseStyle -> "InformationLink", ButtonData :> {"Info1863765272179-6151880", {"SaferOptions", "SafeOptions`"}}, ButtonNote -> "SafeOptions`"], ""}}, DefaultBaseStyle -> "InfoGrid", GridBoxItemSize -> {"Columns" -> {{Scaled[0.1357142857142857]}}}]}}]}, Dynamic[Typeset`open$$], ImageSize -> Automatic]*)


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
doTest[overwriteOptions[{},c->3], $Failed,{SaferOptions::unknownOptions}];
doTest[overwriteOptions[c->3,{{}}], overwriteOptions[c->3,{{}}]];
doTest[overwriteOptions[c->3,a -> 1],overwriteOptions[c->3,a -> 1]];
doTest[overwriteOptions[{a -> 1},c->3], $Failed,{SaferOptions::unknownOptions}];
doTest[overwriteOptions[{a -> 1,c->2},c->3], {a->1,c->3}];
doTest[overwriteOptions[{a -> 1,c->2,c->4},c->3], $Failed,{SaferOptions::duplicateOptions}];
doTest[overwriteOptions[{a -> 1,b->{a->1}},a->A], {a->A,b->{a->1}}];
doTest[overwriteOptions[{a -> 1,b->{a->1}},a->A,3], {a->A,b->{a->A}}];
doTest[overwriteOptions[{a -> 1,b->{a->1}},a->A,{3}], {a->1,b->{a->A}}];


(* ::Subchapter:: *)
(*appendOptions[]*)


<< SafeOptions`
?appendOptions


doTest[appendOptions[c -> 3, 1, 2], appendOptions[c -> 3, 1, 2]];
doTest[appendOptions[c -> 3], appendOptions[c -> 3]];
doTest[appendOptions[{},c -> 3], {c -> 3}];
doTest[appendOptions[c -> 3, {{}}], appendOptions[c -> 3, {{}}]];
doTest[appendOptions[c -> 3, a -> 1], appendOptions[c -> 3, a -> 1]];
doTest[appendOptions[{c -> 1}, c -> 3], $Failed,{SaferOptions::duplicateOptions}];
doTest[appendOptions[{a -> 1, b -> 2},c -> 3], {a -> 1, b -> 2, c -> 3}];


(* ::Subchapter:: *)
(*updateOptions[]*)


<< SafeOptions`
?updateOptions


doTest[updateOptions[c -> 3, 1, 2], updateOptions[c -> 3, 1, 2]];
doTest[updateOptions[c -> 3], updateOptions[c -> 3]];
doTest[updateOptions[c -> 3, {{}}], updateOptions[c -> 3, {{}}]];
doTest[updateOptions[c -> 3, a -> 1], updateOptions[c -> 3, a -> 1]];
doTest[updateOptions[{a -> 1, c -> 2},c -> 3], {a -> 1, c -> 3}];
doTest[updateOptions[{a -> 1, c -> 2, c -> 4},c -> 3], $Failed,{SaferOptions::duplicateOptions}];
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
doTest[SafeOptions`Private`checkDuplicateFreeOptionsQ[{a->1,a->2}], False,{SaferOptions::duplicateOptions}]; (* must print error msg *)


(* ::Subchapter:: *)
(*checkGetOptionListQ[]*)


<<SafeOptions`


doTest[SafeOptions`Private`checkGetOptionListQ[{}], True];
doTest[SafeOptions`Private`checkGetOptionListQ[{}, {{a -> 1}}], False,{SaferOptions::unknownOptions}]; 
doTest[SafeOptions`Private`checkGetOptionListQ[{a -> 2}, {{a -> 1}}], True];
doTest[SafeOptions`Private`checkGetOptionListQ[{a -> 2, b -> 3}, {{a -> 1}}], True];
doTest[SafeOptions`Private`checkGetOptionListQ[{a -> 2, b -> 3}, a -> 1], True];
doTest[SafeOptions`Private`checkGetOptionListQ[{a -> 2}, {{a -> 1, b -> 3}}], False,{SaferOptions::unknownOptions}];
doTest[SafeOptions`Private`checkGetOptionListQ[{a -> 2, b -> 3, a -> 4}, {{a -> 1}}], False,{SaferOptions::duplicateOptions}];
doTest[SafeOptions`Private`checkGetOptionListQ[{a -> 2, b -> 3}, {{a -> 1, a -> 2}}], False,{SaferOptions::duplicateOptions}]; 


(* ::Subchapter:: *)
(*filterOptionList[]*)


<<SafeOptions`
?filterOptionList


doTest[filterOptionList[{}], {}];
doTest[filterOptionList[{},{}], {}];
doTest[filterOptionList[{a->1},{}], {}];
doTest[filterOptionList[{a->1},{a->2}], {a->2}];
doTest[filterOptionList[{},{a->2}], {}];
doTest[filterOptionList[{b->3},{a->2}], {}];
doTest[filterOptionList[{a->1,b->3},{a->2,b->4}], {a->2,b->4}];
doTest[filterOptionList[{a->1,b->3,a->2},{a->2,b->4}], $Failed,{SaferOptions::duplicateOptions}]; 
doTest[filterOptionList[{a->1,b->3},{a->2,b->4,a->2}], $Failed,{SaferOptions::duplicateOptions}]; 


(* ::Subchapter:: *)
(*getOptionList[]*)


<<SafeOptions`
?getOptionList


doTest[getOptionList[{}], {}];
doTest[getOptionList[{},{}], {}];
doTest[getOptionList[{a->1},{}], {a->1}];
doTest[getOptionList[{a->1},{a->2}], {a->2}];
doTest[getOptionList[{},{a->2}], $Failed,{SaferOptions::unknownOptions}];
doTest[getOptionList[{b->3},{a->2}], $Failed,{SaferOptions::unknownOptions}];
doTest[getOptionList[{a->1,b->3},{a->2,b->4}], {a->2,b->4}];
doTest[getOptionList[{a->1,b->3,a->2},{a->2,b->4}], $Failed,{SaferOptions::duplicateOptions}]; 
doTest[getOptionList[{a->1,b->3},{a->2,b->4,a->2}], $Failed,{SaferOptions::duplicateOptions}]; 


(* ::Subchapter:: *)
(*createOptionList[]*)


<<SafeOptions`
?createOptionList


doTest[createOptionList[{},{}],{}];
doTest[createOptionList[{a->1},{{}},optionsToIgnore->{}],$Failed,{SaferOptions::unknownOptions}]; 
doTest[createOptionList[{a->1},{{}},optionKeysToIgnore->{a}],$Failed,{SaferOptions::cannotAddAndIgnore}]; 
doTest[createOptionList[{a->1},{{}},optionKeysToIgnore->{b}], {a->1}]; 
doTest[createOptionList[{a->1},{{}},optionKeysToIgnore->{}], {a->1}]; 
doTest[createOptionList[{a->1},{{b->2},{c->3}},optionKeysToIgnore->{}],{a->1,b->2,c->3} ]; 
doTest[createOptionList[{a->1},{{b->2},{c->3}},optionKeysToIgnore->{b}],{a->1,c->3} ]; 
doTest[createOptionList[{a->1},{{b->2},{b->3}},optionKeysToIgnore->{b}],{a->1}]; 
doTest[createOptionList[{a->1},{{b->2},{b->3}},optionKeysToIgnore->{}],$Failed,{SaferOptions::incompatibleOptions}]; 


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
   foo1[1, 2,filterOptionList[Options[foo1],safeOpts]]; 
   foo2[4, 5,filterOptionList[Options[foo2],safeOpts]];
  ];


foo3[a->2,b->3]


Options[foo3] = createOptionList[{a -> 10, c -> 4}, Options /@ {foo1, foo2}];

foo3[opts : OptionsPattern[]] :=      
  Block[{safeOpts = getOptionList[Options[foo3],opts]},
   foo1[1, 2,filterOptionList[Options[foo1],safeOpts]]; 
   foo2[4, 5,filterOptionList[Options[foo2],safeOpts]];
  ];


createOptionList[{a -> 10, c -> 4}, Options /@ {foo1, foo2}]



