#!/usr/bin/env wolframscript
(* ::Package:: *)

If[ $FrontEnd === Null,
                $FeynCalcStartupMessages = False;
                Print["Computation"];
];
Off[FrontEndObject::notavail]
$LoadAddOns={"FeynHelpers","FeynArts"};
<<FeynCalc`;
$FAVerbose =0;
Off[Paint::nolevel];


(* FEYNARTS AND FEYNCALC *)

Topol = CreateTopologies[1,1->2,ExcludeTopologies->{Internal},Adjacencies->{3,4} ];

(* Paint[Topol,ColumnsXRows->1,FieldNumbers->True]; *)

exclusions = {S[2],S[3],
F[1,{_}],F[2,{_}],F[3,{_}],F[4,{_}],
U[1 | 2 | 3 | 4 | 5 ],
V[5]};


InitializeModel[{SM, UnitarySM}, GenericModel -> {Lorentz, UnitaryLorentz}];
DiagF =InsertFields[Topol,{S[1]}->{V[2],V[1]},Model -> {SM, UnitarySM}, 
        GenericModel -> {Lorentz, UnitaryLorentz}, 
                         InsertionLevel -> {Particles},
                         ExcludeParticles->exclusions];    


SetOptions[Paint,ColumnsXRows->{3,1},FieldNumbers->True,Numbering->False];

s =Paint[DiagF];
Export["Diagram.pdf",s,"PDF"]; 

Amp=CreateFeynAmp[DiagF, PreFactor -> 1,Truncated->True, GaugeRules -> {FAGaugeXi[W | Z] -> Infinity}];


AmpFC=FCFAConvert[Amp,  ChangeDimension->D,              
                    IncomingMomenta->{p1},
                    OutgoingMomenta->{p2,p3},
                    LoopMomenta->{l}] // Simplify ;


changes = {FCGV[a_]:>ToExpression[a],EL->e,ME->me,
Lor1->\[Mu],Lor2->\[Nu],Lor3->\[Sigma],Lor4->\[Lambda],Lor5->\[Rho],Lor6->\[Alpha],Lor7->\[Beta],Lor8->\[Epsilon],MH-> mh,MW-> mw};
Amp2 = AmpFC //. changes ;


FCClearScalarProducts[];
SPD[p3,p3]=  0;
SPD[p2,p2]=mz^2;
SPD[p1,p1]=mh^2;
SPD[p2,p3]=  (mh^2-mz^2)/2;


Ampw =Total[Amp2]//Simplify // Contract //FCE ;


ChangeDenToPV = {expression_:>TID[expression,l,ToPaVe -> True]};
Redu = TID[Ampw,l]//Simplify;

(* Recognizing denominators and then change them to PV function *)

(*PV functions *)
PVfunctions = Cases[ Redu, FeynAmpDenominator[__],Infinity]/(I*Pi^2)  /. ChangeDenToPV // DeleteDuplicates ;
ChangePVToLaurent = Table[(PVfunctions[[i]] -> PaXEvaluate[#,PaXImplicitPrefactor->1/(2*Pi)^(4-2*Epsilon)]) & [PVfunctions[[i]]],{i,1,Length[PVfunctions]}];
Redu1 = ToPaVe[Redu,l]//FCE;


(* Selecting only contributing tensor structures via Ward Identities *)
ReduF4 =  Coefficient[Redu1,FVD[p3, \[Mu]]*FVD[p2, \[Nu]] ]//Simplify;
RedAmp = (ReduF4 /. ChangePVToLaurent) // FCReplaceD[#, D -> 4 - 2*Epsilon] & // Series[#, {Epsilon, 0, 0}] & // Normal //Simplify //FCE;
Coefficient[RedAmp,Epsilon^-1] // Simplify;


F4 = RedAmp// ChangeDimension[#, 4] & 


SP[p3,p3]=0;
SP[p2,p3]=  (mh^2-mz^2)/2;
SP[p2,p2]= mz^2;
CW = mw/mz;
SW = Sqrt[1-CW^2];
SF4 = F4*(ComplexConjugate[F4]) //Simplify // FCE ;  


DWidth = mh^3/(32*Pi)*(1- mz^2/mh^2)^3*SF4/.mw->80.36/.mz->91.18/.e->Sqrt[4*Pi/137]//Simplify;

Print["Partial decay width saved in \"ExpressionDWidth\" "]

DWidth1 = StringReplace[ToString[DWidth, InputForm], {"*^" -> "*10^", ".*" -> "*"}]
text = "    DW= mathematica(\"" <> DWidth1 <> "\")";
Export["HZADwidth", text, "Text"];
Export["HZAexpressionDW", DWidth, "Text"];
Quit[]
