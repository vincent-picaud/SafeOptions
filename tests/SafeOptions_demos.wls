#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Title:: *)
(*SaferOption package demo*)


(* ::Chapter:: *)
(*Problems with Options*)


(* ::Text:: *)
(*I often encountered these problems which discouraged me from using the options:*)
(*-  OptionPattern is very permissive*)
(*-  Default values are not forwarded*)


(* ::Subchapter:: *)
(*OptionPattern is very permissive*)


foo[opts:OptionsPattern[]]:=opts


foo[2]
foo[{}]
foo[{{}}]
foo[a->1,b->2]
foo[{a->1,b->2}]
foo[{{a->1,b->2}}]
foo[{{{a->1,b->2}}}]


(* ::Text:: *)
(*This is problematic when we want to process options:*)


foo[k_Symbol,opts:OptionsPattern[]]:=MemberQ[Keys[opts],k]


foo[a,{}]            (* OK *)
foo[a,{a->1,b->2}]     (* OK *)
foo[a,{{a->1,b->2}}]   (* Problem: would expect true *)
foo[a,a->1,b->2]       (* Problem: would expect true*)


(* ::Subchapter:: *)
(*Option default values are not forwarded*)


Options[foo1] = {a -> 1, b -> 2};

foo1[arg__, opts : OptionsPattern[]] := 
   Print["foo1 a=", OptionValue[a], " b=", OptionValue[b]]; 

Options[foo2] = {b -> -2};

foo2[arg__, opts : OptionsPattern[]] := 
   Print["foo2 b=", OptionValue[b]];
   
Options[foo3] = {a -> 10, b -> 20};   

foo3[opts : OptionsPattern[]] :=      
  Block[{},
   foo1[1, 2,FilterRules[{opts},Options[foo1]]]; 
   foo2[4, 5,FilterRules[{opts},Options[foo2]]]; 
  ];


foo3[] (* Problem: would expect a\[Rule]10 and b\[Rule]20, moreover we have two incompatible values for b: +2 and -2 *)


foo3[a->5] (* ok for a, but error prone... here we forgot b *)


(* ::Chapter:: *)
(*SaferOption package*)


(* ::Text:: *)
(*The ideas are simples:*)
(*- we systematically create an exhaustive list of options for each function (thanks to the createOptionList[] function).*)
(*- we systematically use a normalized representation for option set (thanks to the normalizeOptionPattern[] function)*)


<<SafeOptions`
?"SafeOptions`*"


?safeOptions


(* ::Subchapter:: *)
(*OptionsPattern is very permissive: solution use normalizeOptionPattern[]*)


foo[opts:OptionsPattern[]]:=normalizeOptionPattern[opts]


foo[2]
foo[{}]
foo[{{}}]
foo[a->1,b->2]
foo[{a->1,b->2}]
foo[{{a->1,b->2}}]
foo[{{{a->1,b->2}}}]


(* ::Text:: *)
(*using normalized representation simplifies further processing*)


foo[k_Symbol,opts:OptionsPattern[]]:=MemberQ[Keys[normalizeOptionPattern[opts]],k]


foo[a,{}]            (* OK *)
foo[a,{a->1,b->2}]     (* OK *)
foo[a,{{a->1,b->2}}]   (* No more problem *)
foo[a,a->1,b->2]       (* No more problem *)


(* ::Subchapter:: *)
(*Safer option management use: createOptionList[], getOptionList[], filterOptionList[]*)


(* ::Text:: *)
(*An example reusing  foo1, foo2 with MMA's option system*)


Options[foo1] = {a -> 1, b -> 2};

foo1[arg__, opts : OptionsPattern[]] := 
   Print["foo1 a=", OptionValue[a], " b=", OptionValue[b]]; 

Options[foo2] = {b -> -2};

foo2[arg__, opts : OptionsPattern[]] := 
   Print["foo2 b=", OptionValue[b]];


(* ::Text:: *)
(*Now Foo3 use SaferOptions system*)


Options[foo3] = createOptionList[{a -> 10, b -> 1, c -> 4}, Options /@ {foo1, foo2}];

foo3[opts : OptionsPattern[]] :=      
  Block[{safeOpts = getOptionList[Options[foo3],opts]},
   If[safeOpts===$Failed,Return[$Failed]];
   foo1[1, 2,filterOptionList[Options[foo1],safeOpts]]; 
   foo2[4, 5,filterOptionList[Options[foo2],safeOpts]];
  ];


(* ::Text:: *)
(*Default option values are forwarded:*)


foo3[]


(* ::Text:: *)
(*Obviously, you can still explicitly provide their values*)


foo3[a->5]


(* ::Text:: *)
(*Incompatible values for "b" are detected*)


Options[foo3] = createOptionList[{a -> 10, c -> 4}, Options /@ {foo1, foo2}]


(* ::Text:: *)
(*You must explicitly define the value*)


Options[foo3] = createOptionList[{a -> 10, b->1,c -> 4}, Options /@ {foo1, foo2}]


(* ::Text:: *)
(*You can ignore some options*)


Options[foo3] = createOptionList[{ b->1,c -> 4}, Options /@ {foo1, foo2},optionKeysToIgnore->{a}]


(* ::Text:: *)
(*In that case initial default values are used*)


foo3[]


(* ::Text:: *)
(*Note: you can not use ignored options nor undefined ones:*)


foo3[a->3]
foo3[d->3]


(* ::Text:: *)
(*Note : you cannot add option and ignore them in the same time*)


Options[foo3] = createOptionList[{a -> 10, b->1,c -> 4}, Options /@ {foo1, foo2},optionKeysToIgnore->{a}]


(* ::Section:: *)
(*Another example where foo1 & foo2 also use SafeOptions package*)


Options[foo1] = createOptionList[{a -> 1, b -> 2}];

foo1[arg__, opts : OptionsPattern[]] := 
Block[{safeOpts},
   If[(safeOpts = getOptionList[Options[foo1],opts])===$Failed,Return[$Failed]];
   Print["foo1 a=", optionValue[safeOpts,a], " b=", optionValue[safeOpts,b]]; 
   ];

Options[foo2] = createOptionList[{b -> -2}];

foo2[arg__, opts : OptionsPattern[]] := 
 Block[{safeOpts},
   If[(safeOpts = getOptionList[Options[foo2],opts])===$Failed,Return[$Failed]];
   Print["foo2 b=", optionValue[safeOpts,b]];
   ];


(* ::Text:: *)
(*foo3 is defined as before:*)


Options[foo3] = createOptionList[{a -> 10, b -> 1, c -> 4}, Options /@ {foo1, foo2}];

foo3[opts : OptionsPattern[]] :=      
  Block[{safeOpts},
 
   If[(safeOpts = getOptionList[Options[foo3],opts])===$Failed,Return[$Failed]];
   
   safeOpts=updateOptions[safeOpts,b->3];
   
   foo1[1, 2,filterOptionList[Options[foo1],safeOpts]]; 
   foo2[4, 5,filterOptionList[Options[foo2],safeOpts]];
  ];


foo3[c->2]


foo3[a->5]


(* ::Text:: *)
(*etc.*)
