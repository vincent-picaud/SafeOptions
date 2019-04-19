(* ::Package:: *)

BeginPackage["SafeOptions`"];
Unprotect @@ Names["SafeOptions`*"];
ClearAll @@ Names["SafeOptions`*"];


(* ::Chapter:: *)
(*Error messages*)


SaferOptions::incompatibleOptions="Some options are incompatibles `1`";
SaferOptions::cannotAddAndIgnore="Cannot add and ignore in the same times these options: `1`";


(* ::Chapter:: *)
(*Options*)


(* ::Chapter:: *)
(*User functions*)


normalizeOptionPattern::usage=
"normalizeOptionPattern[opts:OptionsPattern[]] turns options into an unified list representation.";


normalizedOptionListQ::usage=
"normalizeOptionPattern[opts:OptionsPattern[]] tests if opts is in the \"unified list representation\" form.";


hasOptionQ::usage=
"hasOptionQ[k_,opts:OptionsPattern[]] checks if option k->? is in allOptions. Caveat: returns true even in cases of multi-occurences.";
hasUniqueOptionQ::usage=
"hasUniqueOptionQ[k_,opts:OptionsPattern[]] checks if option k->? is in allOptions. Caveat: returns false in cases of multi-occurences.";


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


(* ::Chapter:: *)
(*Code*)


Begin["`Private`"];


normalizeOptionPatternHelper[opts:((_->_)|(_:>_))...]:={opts};
normalizeOptionPatternHelper[opts:{_ ...}]:=normalizeOptionPatternHelper[Apply[Sequence,opts]];
normalizeOptionPattern[opts:OptionsPattern[]]:=normalizeOptionPatternHelper[opts];


normalizedOptionListQ[x___]:=False;
normalizedOptionListQ[{((_->_)|(_:>_))...}]:=True;


hasOptionQ[k_,allOptions:OptionsPattern[]]:=MemberQ[Keys@normalizeOptionPattern[allOptions],k];
hasUniqueOptionQ[k_,allOptions:OptionsPattern[]]:=Cases[Keys@normalizeOptionPattern[allOptions],k]=={k};


overwriteOptions[k_->v_,normalizedOptList_?normalizedOptionListQ,levelSpec_:1]:=Block[{},Assert[hasUniqueOptionQ[k,normalizedOptList]];Return[Replace[normalizedOptList,(k->_)->(k->v),levelSpec]]];
overwriteOptions[k_:>v_,normalizedOptList_?normalizedOptionListQ,levelSpec_:1]:=Block[{},Assert[hasUniqueOptionQ[k,normalizedOptList]];Return[Replace[normalizedOptList,(k:>_)->(k:>v),levelSpec]]];
overwriteOptions[modifiedOptions_?normalizedOptionListQ,normalizedOptList_?normalizedOptionListQ,levelSpec_:{1}]:=Fold[overwriteOptions[#2,#1,levelSpec]&,normalizedOptList,modifiedOptions];


addOptions[k_->v_,normalizedOptList_?normalizedOptionListQ]:=Block[{},Assert[!hasOptionQ[k,normalizedOptList]];Return[Append[normalizedOptList,k->v]]];
addOptions[k_:>v_,normalizedOptList_?normalizedOptionListQ]:=Block[{},Assert[!hasOptionQ[k,normalizedOptList]];Return[Append[normalizedOptList,k:>v]]];
addOptions[addedOptions_?normalizedOptionListQ,normalizedOptList_?normalizedOptionListQ]:=Fold[addOptions[#2,#1]&,normalizedOptList,addedOptions];


updateOptions[k_->v_,normalizedOptList_?normalizedOptionListQ]:=If[hasOptionQ[k,normalizedOptList],overwriteOptions[k->v,normalizedOptList],addOptions[k->v,normalizedOptList]];
updateOptions[k_:>v_,normalizedOptList_?normalizedOptionListQ]:=If[hasOptionQ[k,normalizedOptList],overwriteOptions[k:>v,normalizedOptList],addOptions[k:>v,normalizedOptList]];
updateOptions[modifiedOptions_?normalizedOptionListQ,normalizedOptList_?normalizedOptionListQ]:=Fold[updateOptions[#2,#1]&,normalizedOptList,modifiedOptions];


(* ::Input:: *)
(*completeOptions[addedOptions:{((_->_)|(_:>_))...},ignoredKeys:{(_Symbol|_String)...}:{},previousOptions:{{((_->_)|(_:>_))...}..}]:=Block[{addedKeys,forwardedOptions,incompatible},addedKeys=Keys[addedOptions];*)
(*forwardedOptions=Intersection[addedKeys,ignoredKeys];(*forwardedOptions=temporary*)If[forwardedOptions!={},Message[SaferOptions::cannotAddAndIgnore,forwardedOptions];*)
(*Return[$Failed];];*)
(*forwardedOptions=Flatten[previousOptions];*)
(*forwardedOptions=Select[forwardedOptions,(!MemberQ[ignoredKeys,Keys[#]])&];*)
(*forwardedOptions=Select[forwardedOptions,(!MemberQ[addedKeys,Keys[#]])&];*)
(*forwardedOptions=DeleteDuplicates[forwardedOptions];*)
(*If[!DuplicateFreeQ[Keys[forwardedOptions]],incompatible=Select[Tally[Keys[forwardedOptions]],(#[[2]]>1)&][[All,1]];*)
(*incompatible=Select[forwardedOptions,MemberQ[incompatible,Keys[#]]&];*)
(*incompatible=Merge[incompatible,Identity];*)
(*Message[SaferOptions::incompatibleOptions,incompatible];*)
(*Return[$Failed];];*)
(*Return[Join[addedOptions,forwardedOptions]];];*)
(**)
(*completeOptions[addedOptions:{((_->_)|(_:>_))...},ignoredKeys:{(_Symbol|_String)...}:{},previousOptions:{((_->_)|(_:>_))...}]:=completeOptions[addedOptions,ignoredKeys,{previousOptions}];*)
(**)
(*SaferOptions::unknownOption="Options `1` are unknown";*)
(*SaferOptions::duplicateOption="Duplicate options `1`";*)
(**)
(*checkCompleteOptionsQ[localOptions:{((_->_)|(_:>_))...},allOptions:{((_->_)|(_:>_))...}]:=Block[{kAllOptions=Keys[allOptions],kLocalOptions=Keys[localOptions]},If[!DuplicateFreeQ[kAllOptions],Message[SaferOptions::duplicateOption,Select[Tally[kAllOptions],(Last@#>1)&][[All,1]]];*)
(*Return[False];];*)
(*If[!DuplicateFreeQ[kLocalOptions],Message[SaferOptions::duplicateOption,Select[Tally[kLocalOptions],(Last@#>1)&][[All,1]]];*)
(*Return[False];];*)
(*If[SubsetQ[kAllOptions,kLocalOptions],Return[True],Message[SaferOptions::unknownOption,Complement[kLocalOptions,Intersection[kAllOptions,kLocalOptions]]];*)
(*Return[False]];];*)


End[]; (* private *)
Protect @@ Names["SafeOptions`*"];
EndPackage[];
