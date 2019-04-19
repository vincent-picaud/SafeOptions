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
(*normalizedOptionList[]*)


Need["SafeOptions`"];
doTest[normalizedOptionList[1,2], False];
doTest[normalizedOptionList[], False];
doTest[normalizedOptionList[{}], True];
doTest[normalizedOptionList[{{}}], False];
doTest[normalizedOptionList[a -> 1], False];
doTest[normalizedOptionList[{a -> 1}], True];
doTest[normalizedOptionList[{{a -> 1}}], False];
doTest[normalizedOptionList[a -> 1, b -> 2], False];
doTest[normalizedOptionList[{a -> 1, b -> 2}], True];
doTest[normalizedOptionList[{{a -> 1, b -> 2}}], False];


(* ::Chapter:: *)
(*Report (must be 100%)*)


TestReport[allTests]
