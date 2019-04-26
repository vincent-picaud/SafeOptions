(* ::Package:: *)

BeginPackage["SafeOptions`"];
Unprotect @@ Names["SafeOptions`*"];
ClearAll @@ Names["SafeOptions`*"];


safeOptions::usage=
"The general pattern to use this package is as follows:\n\n"<>
"Options[foo3] = createOptionList[{a -> 10, b -> 1, c -> 4}, Options /@ {foo1, foo2}];\n"<>
"\n"<>
"foo3[opts : OptionsPattern[]] :=  \n"<>    
"    Block[{safeOpts = getOptionList[Options[foo3], opts]},\n"<>
"      If[safeOpts === $Failed, Return[$Failed]];\n"<>
"      foo1[1, 2, filterOptionList[Options[foo1], safeOpts]];\n"<>
"      foo2[4, 5, filterOptionList[Options[foo2], safeOpts]];\n"<>
"     ];";


(* ::Chapter:: *)
(*Error messages*)


safeOptions::unknownOptions = "Unknown options `1`";
safeOptions::duplicateOptions = "Duplicate options `1`";


safeOptions::notNormalized="Options `1` are not in a normalized form.";


safeOptions::incompatibleOptions="Some options are incompatibles `1`";
safeOptions::cannotAddAndIgnore="Cannot add and ignore in the same times these options: `1`";


(* ::Chapter:: *)
(*Options*)


optionKeysToIgnore::usage="An option that takes the form of a (delayed)rules { ((_->_)|(_:>_))... }. Used by createOptionList[...].";


(* ::Chapter:: *)
(*User functions*)


normalizeOptionPattern::usage=
"normalizeOptionPattern[opts:OptionsPattern[]] turns options into an unified list representation.";


normalizedOptionListQ::usage=
"normalizedOptionListQ[opts:OptionsPattern[]] tests if opts is in the \"unified list representation\" form.";


hasOptionQ::usage=
"hasOptionQ[normalizedOptions_?normalizedOptionListQ,k_] checks if option k->? is in allOptions. Caveat: returns true even in cases of multi-occurences.";
hasUniqueOptionQ::usage=
"hasUniqueOptionQ[normalizedOptions_?normalizedOptionListQ,k_] checks if option k->? is in allOptions. Caveat: returns false in cases of multi-occurences.";


overwriteOptions::usage=
"overwriteOptions[normalizedOptList_?normalizedOptionListQ, k_->v_] overwrites an already existing option in normalizedOptList.\n"<>
"overwriteOptions[normalizedOptList_?normalizedOptionListQ, k_:>v_] overwrites an already existing option in normalizedOptList.\n"<>
"overwriteOptions[normalizedOptList_?normalizedOptionListQ, optionsToModify_?normalizedOptionListQ] overwrites a list of already existing options in normalizedOptList.";


appendOptions::usage=
"appendOptions[normalizedOptList_?normalizedOptionListQ, k_->v_] adds an option at the end of normalizedOptList.\n"<>
"appendOptions[normalizedOptList_?normalizedOptionListQ, k_:>v_] adds an option at the end of normalizedOptList.\n"<>
"appendOptions[normalizedOptList_?normalizedOptionListQ, optionsToAppend_?normalizedOptionListQ] adds options at the end of normalizedOptList.";


updateOptions::usage=
"updateOptions[normalizedOptList_?normalizedOptionListQ, k_->v_]\n"<>
"updateOptions[normalizedOptList_?normalizedOptionListQ, k_:>v_]\n"<>
"updateOptions[normalizedOptList_?normalizedOptionListQ, optionsToUpdate_?normalizedOptionListQ]\n\n"<>
"checks if the option k is already defined and switch to the right appendOptions[...] or updateOptions[...] functions according to the test result.";


ignoreOption::usage="An option to defined ignored option list (used in createOptionList[...])";
createOptionList::usage=
"createOptionList[addedOptions_?normalizedOptionListQ, inheritedOptions : {{_ ...} ..}, opts : OptionsPattern[]]\n"<>
"createOptionList[addedOptions_?normalizedOptionListQ, inheritedOptions_?normalizedOptionListQ, opts : OptionsPattern[]]\n"<>
"createOptionList[addedOptions_?normalizedOptionListQ]\n\n"<>
"collects all the options, generally used to create Options[foo]=createOptionList[...]";


filterOptionList::usage=
"filterOptionList[allowedOptions_?normalizedOptionListQ, opts : OptionsPattern[]] filters allowedOptions returning only those in opts, similar to FilterRules[{opt},allowedOptions].";


getOptionList::usage=
"getOptionList[allowedOptions_?normalizedOptionListQ, opts : OptionsPattern[]] returns the whole allowedOptions taking into account potential modifications from opts.";


optionValue::usage=
"optionValue[allowedOptions_?normalizedOptionListQ, k_Symbol] returns k-option value. $Failed in case of undefined key k.";


(* ::Chapter:: *)
(*Code*)


Begin["`Private`"];


normalizeOptionPatternHelper[opts:((_->_)|(_:>_))...]:={opts};
normalizeOptionPatternHelper[opts:{_ ...}]:=normalizeOptionPatternHelper[Apply[Sequence,opts]];
normalizeOptionPattern[opts:OptionsPattern[]]:=normalizeOptionPatternHelper[opts];


normalizedOptionListQ[x___]:=False;
normalizedOptionListQ[{((_->_)|(_:>_))...}]:=True;


checkNormalizedOptionListQ[x___] :=
        If[normalizedOptionListQ[x], Return[True],
           Message[safeOptions::notNormalized, x];
           Return[False];
        ];


hasOptionHelper[normalizedOptions_?normalizedOptionListQ, k_] := Count[normalizedOptions, k -> _]
hasOptionQ[normalizedOptions_?normalizedOptionListQ, k_] := hasOptionHelper[normalizedOptions, k] >= 1;
hasUniqueOptionQ[normalizedOptions_?normalizedOptionListQ, k_] := hasOptionHelper[normalizedOptions, k] == 1;
checkHasUniqueOptionQ[normalizedOptions_?normalizedOptionListQ, k_] :=
  Switch[hasOptionHelper[normalizedOptions, k],
   0, Message[safeOptions::unknownOptions, k]; Return[False],
   1, Return[True],
   _, Message[safeOptions::duplicateOptions, k]; Return[False]
   ];


overwriteOptions[normalizedOptList_?normalizedOptionListQ, k_ -> v_, levelSpec_: 1] :=
          If[hasUniqueOptionQ[normalizedOptList, k],
             Return[Replace[normalizedOptList, (k -> _) -> (k -> v), levelSpec]],
             If[hasOptionQ[normalizedOptList, k],
              Message[safeOptions::duplicateOptions, k],
              Message[safeOptions::unknownOptions, k]]; 
          Return[$Failed]];
  overwriteOptions[normalizedOptList_?normalizedOptionListQ, k_ :> v_, levelSpec_: 1] :=
        If[hasUniqueOptionQ[normalizedOptList, k],
           Return[Replace[normalizedOptList, (k :> _) -> (k :> v), levelSpec]],
           If[hasOptionQ[normalizedOptList, k],
            Message[safeOptions::duplicateOptions, k],
            Message[safeOptions::unknownOptions, k]]; 
        Return[$Failed]];
overwriteOptions[normalizedOptList_?normalizedOptionListQ, optionsToModify_?normalizedOptionListQ, levelSpec_: {1}] :=
      Fold[overwriteOptions[#1, #2, levelSpec] &, normalizedOptList, optionsToModify];


appendOptions[normalizedOptList_?normalizedOptionListQ, k_ -> v_] :=
  If[hasOptionQ[normalizedOptList, k],
   Message[safeOptions::duplicateOptions, k]; Return[$Failed],
   Return[Append[normalizedOptList, k -> v]]];
appendOptions[normalizedOptList_?normalizedOptionListQ, k_ :> v_] :=
  If[hasOptionQ[normalizedOptList, k],
   Message[safeOptions::duplicateOptions, k]; Return[$Failed],
   Return[Append[normalizedOptList, k :> v]]];
appendOptions[normalizedOptList_?normalizedOptionListQ, optionsToAdd_?normalizedOptionListQ] :=
  Fold[appendOptions, normalizedOptList, optionsToAdd];


updateOptions[normalizedOptList_?normalizedOptionListQ, k_->v_]:=If[hasOptionQ[normalizedOptList,k],overwriteOptions[normalizedOptList,k->v],appendOptions[normalizedOptList,k->v]];
updateOptions[normalizedOptList_?normalizedOptionListQ, k_:>v_]:=If[hasOptionQ[normalizedOptList,k],overwriteOptions[normalizedOptList,k:>v],appendOptions[normalizedOptList,k:>v]];
updateOptions[normalizedOptList_?normalizedOptionListQ, optionsToModify_?normalizedOptionListQ]:=Fold[updateOptions,normalizedOptList,optionsToModify];


checkDuplicateFreeOptionsQ[allowedOptions_?normalizedOptionListQ] :=
        Block[{keys},
              keys = Keys[allowedOptions];
              If[Not[DuplicateFreeQ[keys]],
                 Message[safeOptions::duplicateOptions, Select[Tally[keys], (Last@# > 1) &][[All, 1]]];
                 Return[False];
              ];
              Return[True];
        ];


(* Note: compared to FilterRules[...] the arguiment order is reversed,
,* however this is on purpose as full benefice of "opts:OptionsPattern[]" impose to
,* put this pattern at the end
,*)
filterOptionList[allowedOptions_?normalizedOptionListQ, opts : OptionsPattern[]] :=
        Block[{normalizedOpts},
              If[Not[checkDuplicateFreeOptionsQ[allowedOptions]], Return[$Failed]];
              normalizedOpts = normalizeOptionPattern[opts];
              If[Not[checkDuplicateFreeOptionsQ[normalizedOpts]], Return[$Failed]];
              Return[FilterRules[normalizedOpts, allowedOptions]];
        ];


(* ::Subchapter:: *)
(*getOptionList*)


checkGetOptionListQ[allowedOptions_?normalizedOptionListQ, opts : OptionsPattern[]] :=
        Block[{normalizedOpts, normalizedOptKeys, allowedOptKeys},
              normalizedOpts = normalizeOptionPattern[opts];
              
              If[Not[checkDuplicateFreeOptionsQ[normalizedOpts]], Return[False]];
              If[Not[checkDuplicateFreeOptionsQ[allowedOptions]], Return[False]];
              
              normalizedOptKeys = Keys[normalizedOpts];
              allowedOptKeys = Keys[allowedOptions];
              
              If[Not[SubsetQ[allowedOptKeys, normalizedOptKeys]], 
                 Message[safeOptions::unknownOptions, Complement[normalizedOptKeys, Intersection[allowedOptKeys, normalizedOptKeys]]];
                 Return[False];
              ];
              
              Return[True];
        ];


getOptionList[allowedOptions_?normalizedOptionListQ, opts : OptionsPattern[]] :=
        Block[{normalizedOpts},
              If[Not[checkGetOptionListQ[allowedOptions, opts]],Return[$Failed]];
              normalizedOpts = normalizeOptionPattern[opts];
              Return[updateOptions[allowedOptions,normalizedOpts]]
        ];


(* ::Subchapter:: *)
(*createOptionList*)


checkAddVsIgnoreQ[addedOptKeys_List, ignoredOptKeys_List] :=
        Block[{commonKeys},
              commonKeys = Intersection[addedOptKeys, ignoredOptKeys];
              If[commonKeys == {},
                 Return[True],
                 Message[safeOptions::cannotAddAndIgnore, commonKeys];
                 Return[False]
              ];
        ];


checkIncompatibleOptionsQ[forwardedOptions_?normalizedOptionListQ] :=
        Block[{incompatible},
              If[DuplicateFreeQ[Keys[forwardedOptions]],
                 Return[True],
                 incompatible = Select[Tally[Keys[forwardedOptions]], (#[[2]] > 1) &][[All, 1]];
                 incompatible = Select[forwardedOptions, MemberQ[incompatible, Keys[#]] &];
                 incompatible = Normal[Merge[incompatible, Identity]];
                 Message[safeOptions::incompatibleOptions, incompatible];
                 Return[False];
              ];
        ];


Options[createOptionList] = {optionKeysToIgnore -> {}}; 

createOptionList[addedOptions_?normalizedOptionListQ, inheritedOptions : {{_ ...} ..}, opts : OptionsPattern[]] :=
        Block[{addedOptKeys, safeOpts, ignoredOptKeys, forwardedOptions},
              safeOpts = getOptionList[Options[createOptionList], opts];
              If[safeOpts===$Failed,Return[$Failed]];
              ignoredOptKeys = optionKeysToIgnore /. safeOpts;
              addedOptKeys = Keys[addedOptions];
              If[Not[checkAddVsIgnoreQ[addedOptKeys, ignoredOptKeys]],Return[$Failed]];
              
              forwardedOptions = Flatten[inheritedOptions];
              forwardedOptions = Select[forwardedOptions, Not[MemberQ[ignoredOptKeys, Keys[#]]] &];
              forwardedOptions = Select[forwardedOptions, Not[MemberQ[addedOptKeys, Keys[#]]] &]; (* added options always overwrite inherited options *)
              forwardedOptions = DeleteDuplicates[forwardedOptions];
              
              If[Not[checkIncompatibleOptionsQ[forwardedOptions]],Return[$Failed]];
              
              Return[Join[addedOptions, forwardedOptions]];
        ] /; Apply[And, Map[normalizedOptionListQ, inheritedOptions]]

createOptionList[addedOptions_?normalizedOptionListQ, inheritedOptions_?normalizedOptionListQ, opts : OptionsPattern[]] :=
    createOptionList[addedOptions, {inheritedOptions}, opts];
        
createOptionList[addedOptions_?normalizedOptionListQ]:=
	createOptionList[addedOptions, {}];


optionValue[allowedOptions_?normalizedOptionListQ, k_Symbol]:=
If[checkHasUniqueOptionQ[allowedOptions,k],Return[k/.allowedOptions],Return[$Failed]];


End[]; (* private *)
Protect @@ Names["SafeOptions`*"];
EndPackage[];
