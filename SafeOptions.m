(* ::Package:: *)

BeginPackage["SafeOptions`"];
Unprotect @@ Names["SafeOptions`*"];
ClearAll @@ Names["SafeOptions`*"];


(* ::Chapter:: *)
(*Error messages*)


SaferOptions::unknownOptions = "Unknown options `1`";
SaferOptions::duplicateOptions = "Duplicate options `1`";


SaferOptions::notNormalized="Options `1` are not in a normalized form.";


SaferOptions::incompatibleOptions="Some options are incompatibles `1`";
SaferOptions::cannotAddAndIgnore="Cannot add and ignore in the same times these options: `1`";


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


(* ::Subsection:: *)
(*TODO: change arguments order! -> better coherence with other routines like filters etc...*)


overwriteOptions::usage=
"overwriteOptions[k_->v_,normalizedOptList_?normalizedOptionListQ] overwrite an already existing option in normalizedOptList.\n"<>
"overwriteOptions[k_:>v_,normalizedOptList_?normalizedOptionListQ] overwrite an already existing option in normalizedOptList.\n"<>
"overwriteOptions[modifiedOptions_?normalizedOptionListQ,normalizedOptList_?normalizedOptionListQ] overwrite a list of already existing options in normalizedOptList.\n\n"<>
"Example: (by default only the first level is affected)\n"<>
"overwriteOptions[a\[Rule]A,{a -> 1,b\[Rule]{a\[Rule]1}}]\n"<>
"{a\[Rule]A,b\[Rule]{a\[Rule]1}}\n\n"<>
"overwriteOptions[a\[Rule]A,{a -> 1,b\[Rule]{a\[Rule]1}},{3}]\n"<>
"{a\[Rule]1,b\[Rule]{a\[Rule]A}}";


addOptions::usage=
"addOptions[k_->v_,normalizedOptList_?normalizedOptionListQ] adds an option at the end of normalizedOptList.\n"<>
"addOptions[k_:>v_,normalizedOptList_?normalizedOptionListQ] adds an option at the end of normalizedOptList.\n"<>
"addOptions[modifiedOptions_?normalizedOptionListQ,normalizedOptList_?normalizedOptionListQ] adds options at the end of normalizedOptList.\n\n"<>
"Example:\n"<>
"addOptions[a\[Rule]1,{b -> 2}]\n"<>
"{b\[Rule]2,a\[Rule]1}\n\n"<>
"caveat: if the option already exists an Assert[False] occurs\n"<>
"addOptions[a\[Rule]1,{a -> 2}]"


updateOptions::usage=
"updateOptions[k_->v_,normalizedOptList_?normalizedOptionListQ]\n"<>
"updateOptions[k_:>v_,normalizedOptList_?normalizedOptionListQ]\n"<>
"updateOptions[modifiedOptions_?normalizedOptionListQ,normalizedOptList_?normalizedOptionListQ]\n\n"<>
"checks if the option k is already defined and switch to the right addOptions[...] or updateOptions[...] functions according to the test result.";


ignoreOption::usage="An option to defined ignored option list (used in createOptionList[...])";
createOptionList::usage=
"collects all the option, generally used to create Options[foo]=createOptionList[...]";


filterOptions::usage=
"filterOptions[allowedOptions_?normalizedOptionListQ, opts : OptionsPattern[]] filters allowedOptions returning only those in opts. Equivalent to FilterRules[{opt},allowedOptions]."<>
"Example:\n"<>
"Foo[opts:OptionsPattern]:=\n"<>
"  Block[{},\n"<>
"    subroutine[filterOptions[Options[subroutines],opts]];\n"<>
"    ...\n"<>
"];";


getOptionList::usage=
"getOptionList[allowedOptions_?normalizedOptionListQ, opts : OptionsPattern[]] compared to filter returns the whole allowedOptions taking into account potential modifications from opts."<>
"Example:\n"<>
"Foo[opts:OptionsPattern]:=\n"<>
"  Block[{allOptions},\n"<>
"    allOptions=getOptionList[Options[Foo],opts];\n"<>
"    ...\n"<>
"];";


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
           Message[SaferOptions::notNormalized, x];
           Return[False];
        ];


hasOptionHelper[normalizedOptions_?normalizedOptionListQ,k_]:=Count[normalizedOptions,k->_]
hasOptionQ[normalizedOptions_?normalizedOptionListQ,k_]:=hasOptionHelper[normalizedOptions,k]>=1;
hasUniqueOptionQ[normalizedOptions_?normalizedOptionListQ,k_]:=hasOptionHelper[normalizedOptions,k]==1;


overwriteOptions[k_->v_,normalizedOptList_?normalizedOptionListQ,levelSpec_:1]:=Block[{},Assert[hasUniqueOptionQ[k,normalizedOptList]];Return[Replace[normalizedOptList,(k->_)->(k->v),levelSpec]]];
overwriteOptions[k_:>v_,normalizedOptList_?normalizedOptionListQ,levelSpec_:1]:=Block[{},Assert[hasUniqueOptionQ[k,normalizedOptList]];Return[Replace[normalizedOptList,(k:>_)->(k:>v),levelSpec]]];
overwriteOptions[modifiedOptions_?normalizedOptionListQ,normalizedOptList_?normalizedOptionListQ,levelSpec_:{1}]:=Fold[overwriteOptions[#2,#1,levelSpec]&,normalizedOptList,modifiedOptions];


addOptions[k_->v_,normalizedOptList_?normalizedOptionListQ]:=Block[{},Assert[Not[hasOptionQ[normalizedOptList,k]]];Return[Append[normalizedOptList,k->v]]];
addOptions[k_:>v_,normalizedOptList_?normalizedOptionListQ]:=Block[{},Assert[Not[hasOptionQ[normalizedOptList,k]]];Return[Append[normalizedOptList,k:>v]]];
addOptions[addedOptions_?normalizedOptionListQ,normalizedOptList_?normalizedOptionListQ]:=Fold[addOptions[#2,#1]&,normalizedOptList,addedOptions];


updateOptions[k_->v_,normalizedOptList_?normalizedOptionListQ]:=If[hasOptionQ[normalizedOptList,k],overwriteOptions[k->v,normalizedOptList],addOptions[k->v,normalizedOptList]];
updateOptions[k_:>v_,normalizedOptList_?normalizedOptionListQ]:=If[hasOptionQ[normalizedOptList,k],overwriteOptions[k:>v,normalizedOptList],addOptions[k:>v,normalizedOptList]];
updateOptions[modifiedOptions_?normalizedOptionListQ,normalizedOptList_?normalizedOptionListQ]:=Fold[updateOptions[#2,#1]&,normalizedOptList,modifiedOptions];


checkDuplicateFreeOptionsQ[allowedOptions_?normalizedOptionListQ] :=
        Block[{keys},
              keys = Keys[allowedOptions];
              If[Not[DuplicateFreeQ[keys]],
                 Message[SaferOptions::duplicateOptions, Select[Tally[keys], (Last@# > 1) &][[All, 1]]];
                 Return[False];
              ];
              Return[True];
        ];


(* Note: compared to FilterRules[...] the arguiment order is reversed,
,* however this is on purpose as full benefice of "opts:OptionsPattern[]" impose to
,* put this pattern at the end
,*)
filterOptions[allowedOptions_?normalizedOptionListQ, opts : OptionsPattern[]] :=
        Block[{normalizedOpts},
              If[Not[checkDuplicateFreeOptionsQ[allowedOptions]], Return[$Failed]];
              normalizedOpts = normalizeOptionPattern[opts];
              If[Not[checkDuplicateFreeOptionsQ[normalizedOpts]], Return[$Failed]];
              Return[FilterRules[normalizedOpts, allowedOptions]];
        ];


(* ::Subchapter:: *)
(*getOptionList*)


checkgetOptionListQ[allowedOptions_?normalizedOptionListQ, opts : OptionsPattern[]] :=
        Block[{normalizedOpts, normalizedOptKeys, allowedOptKeys},
              normalizedOpts = normalizeOptionPattern[opts];
              
              If[Not[checkDuplicateFreeOptionsQ[normalizedOpts]], Return[False]];
              If[Not[checkDuplicateFreeOptionsQ[allowedOptions]], Return[False]];
              
              normalizedOptKeys = Keys[normalizedOpts];
              allowedOptKeys = Keys[allowedOptions];
              
              If[Not[SubsetQ[allowedOptKeys, normalizedOptKeys]], 
                 Message[SaferOptions::unknownOptions, Complement[normalizedOptKeys, Intersection[allowedOptKeys, normalizedOptKeys]]];
                 Return[False];
              ];
              
              Return[True];
        ];


getOptionList[allowedOptions_?normalizedOptionListQ, opts : OptionsPattern[]] :=
        Block[{normalizedOpts},
              If[Not[checkgetOptionListQ[allowedOptions, opts]],Return[$Failed]];
              normalizedOpts = normalizeOptionPattern[opts];
              Return[updateOptions[normalizedOpts, allowedOptions]]
        ];


(* ::Subchapter:: *)
(*createOptionList*)


checkAddVsIgnoreQ[addedOptKeys_List, ignoredOptKeys_List] :=
        Block[{commonKeys},
              commonKeys = Intersection[addedOptKeys, ignoredOptKeys];
              If[commonKeys == {},
                 Return[True],
                 Message[SaferOptions::cannotAddAndIgnore, commonKeys];
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
                 Message[SaferOptions::incompatibleOptions, incompatible];
                 Return[False];
              ];
        ];


Options[createOptionList] = {optionKeysToIgnore -> {}}; 

createOptionList[addedOptions_?normalizedOptionListQ, inheritedOptions : {{_ ...} ..}, opts : OptionsPattern[]] :=
        Block[{addedOptKeys, safeOpts, ignoredOptKeys, forwardedOptions},
              safeOpts = getOptionList[Options[createOptionList], opts];
              ignoredOptKeys = optionKeysToIgnore /. safeOpts;
              addedOptKeys = Keys[addedOptions];
              Assert[checkAddVsIgnoreQ[addedOptKeys, ignoredOptKeys]];
              
              forwardedOptions = Flatten[inheritedOptions];
              forwardedOptions = Select[forwardedOptions, Not[MemberQ[ignoredOptKeys, Keys[#]]] &];
              forwardedOptions = Select[forwardedOptions, Not[MemberQ[addedOptKeys, Keys[#]]] &]; (* added options always overwrite inherited options *)
              forwardedOptions = DeleteDuplicates[forwardedOptions];
              
              Assert[checkIncompatibleOptionsQ[forwardedOptions]];
              
              Return[Join[addedOptions, forwardedOptions]];
        ] /; Apply[And, Map[normalizedOptionListQ, inheritedOptions]]

createOptionList[addedOptions_?normalizedOptionListQ, inheritedOptions_?normalizedOptionListQ, opts : OptionsPattern[]] :=
        createOptionList[addedOptions, {inheritedOptions}, opts];


End[]; (* private *)
Protect @@ Names["SafeOptions`*"];
EndPackage[];
