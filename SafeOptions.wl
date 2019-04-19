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
"normalizeOptionPattern[opts:OptionsPattern[]] turns options into an unified list representation";


(* ::Chapter:: *)
(*Code*)


Begin["`Private`"];


normalizeOptionPatternHelper[opts:((_->_)|(_:>_))...]:={opts};
normalizeOptionPatternHelper[opts:{_ ...}]:=normalizeOptionPatternHelper[Apply[Sequence,opts]];
normalizeOptionPattern[opts:OptionsPattern[]]:=normalizeOptionPatternHelper[opts];


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
(**)
(*overwriteOptions[(k_->v_),allOptions:{((_->_)|(_:>_))..}]:=Block[{},Assert[Cases[Keys@allOptions,k]=={k}];Return[(allOptions/.(k->_)->(k->v))]];*)
(*overwriteOptions[(k_:>v_),allOptions:{((_->_)|(_:>_))..}]:=Block[{},Assert[Cases[Keys@allOptions,k]=={k}];Return[(allOptions/.(k:>_)->(k:>v))]];*)
(*overwriteOptions[modifiedOptions:{((_->_)|(_:>_))..},allOptions:{((_->_)|(_:>_))...}]:=Fold[overwriteOptions,allOptions,modifiedOptions];*)
(**)
(*addOptions[(k_->v_),allOptions:{((_->_)|(_:>_))...}]:=Block[{},Assert[Cases[Keys@allOptions,k]=={}];Return[Append[allOptions,k->v]]];*)
(*addOptions[(k_:>v_),allOptions:{((_->_)|(_:>_))...}]:=Block[{},Assert[Cases[Keys@allOptions,k]=={}];Return[Append[allOptions,k:>v]]];*)
(*addOptions[modifiedOptions:{((_->_)|(_:>_))..},allOptions:{((_->_)|(_:>_))...}]:=Fold[addOptions,allOptions,modifiedOptions];*)
(**)
(*updateOptions[k_->v_,allOptions:{((_->_)|(_:>_))...}]:=If[MemberQ[Keys@allOptions,k],overwriteOptions[k->v,allOptions],addOptions[k->v,allOptions]];*)
(*updateOptions[k_:>v_,allOptions:{((_->_)|(_:>_))...}]:=If[MemberQ[Keys@allOptions,k],overwriteOptions[k:>v,allOptions],addOptions[k:>v,allOptions]];*)
(*updateOptions[modifiedOptions:{((_->_)|(_:>_))..},allOptions:{((_->_)|(_:>_))...}]:=Fold[updateOptions[#2,#1]&,allOptions,modifiedOptions];*)
(**)


End[]; (* private *)
Protect @@ Names["SafeOptions`*"];
EndPackage[];



