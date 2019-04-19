#!/usr/bin/env wolframscript
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
(*?"SafeOptions`*"*)


(* ::Subchapter:: *)
(*does it work?*)


doTest[1,1];


(* ::Subchapter:: *)
(*normalizeOptionPattern[]*)


Options[foo] = {a -> 1};
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
(*hasOption[]*)


<<SafeOptions`
doTest[hasOptionQ[a], False];
doTest[hasOptionQ[a,{}], False];
doTest[hasOptionQ[a,{{}}], False];
doTest[hasOptionQ[a,a -> 1], True];
doTest[hasOptionQ[a,{a -> 1}], True];
doTest[hasOptionQ[a,{{a -> 1}}], True];
doTest[hasOptionQ[a,a -> 1, b -> 2], True];
doTest[hasOptionQ[a,a -> 1, b -> 2,a -> 3], False]; (* caveat *)
doTest[hasOptionQ[a,{a -> 1, b -> 2}], True];
doTest[hasOptionQ[a,{{a -> 1, b -> 2}}], True];


(* ::Chapter:: *)
(*Report (must be 100%)*)


TestReport[allTests]
