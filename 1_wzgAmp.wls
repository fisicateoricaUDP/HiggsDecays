
var = ToExpression[$ScriptCommandLine[[2]]]
name = ToExpression[$ScriptCommandLine[[3]]]
exc = $ScriptCommandLine[[4]]
keyw = ToExpression[$ScriptCommandLine[[5]]]

Print["This script is in: ", Directory[]]                                                                                             SetDirectory[ Directory[ ]];                                                                                                          If[ !DirectoryQ[ ToString[outputs]], CreateDirectory["outputs"]];


If[ $FrontEnd === Null,
    $FeynCalcStartupMessages = False;
    Print["Computation ",var," Begins!"];
    ];
Off[FrontEndObject::notavail]
$LoadAddOns={"FeynHelpers","FeynArts"};
<<FeynCalc`;
$FAVerbose =0;
Off[Paint::nolevel]


(* Generation of Feynman diagrams *)

Topol = CreateTopologies[1, 1->2, ExcludeTopologies -> ToExpression[exc],
      			    	  Adjacencies -> {3,4} ];


exclusions = { F[1,{_}],F[2,{_}],
	       F[3,{1}],F[3,{2}],
	       F[4,{_}], V[5]};


Diags = InsertFields[Topol,{S[1]}->{V[2],V[1]}, Model -> ToString[name],
                                                InsertionLevel -> {Particles},
                          			ExcludeParticles->exclusions]
DiagF = DiagramExtract[Diags, {var}];
Print["Diagram Generated!"]


(*
Note 2:  Just turn on this command lines if you want to paint
         all diagrams contributing to the calculation.

SetOptions[Paint, ColumnsXRows->{5,5},
		  FieldNumbers->True,
		  Numbering->True];
savediag = Paint[Diags];
Export["outputs/"<>ToString[keyw]<>ToString[name]<>"diagrams.pdf",savediag,"PDF"];
Print["Diagrams Exported!"]
*)


(* Generation of Feynman Amplitudes *)

Amp = CreateFeynAmp[DiagF, PreFactor->1,
			   Truncated->True,
			   GaugeRules->{}];

AmpFC = FCFAConvert[Amp, IncomingMomenta -> {q}, 
      			 OutgoingMomenta -> {p1, p2},
			 LoopMomenta -> {l},
			 List -> False,
			 Contract -> True,
			 ChangeDimension -> D,
			 DropSumOver -> True,
			 UndoChiralSplittings -> False]

changes = { FCGV[a_]:>ToExpression[a], EL -> e, ME -> m,
            Lor1 -> u, Lor2 -> v, Lor3 -> u2, Lor4 -> v2,
	    Lor5 -> u3, Lor6 -> v3, Lor7 -> u4, Lor8 -> v4,
	    MH -> Mh, MW -> Mw};
	    
Amp2 = AmpFC //. changes;
Print["FC Amplitudes Generated!"]

FCClearScalarProducts[];
SPD[q,q] = Mh^2;
SPD[p2,p2] = 0;
SPD[p1,p1] = Mz^2;
SPD[p1,p2] = (Mh^2-Mz^2)/2; 


(* Form Factor of Metric Tensor *)

FFactor = Coefficient[ TID[Amp2, l] // FCE, MTD[u, v]];
changes2 = {GaugeXi[a_]:> ToExpression["X"<>ToString[a]],
	    EpsilonUV -> eps,
	    Epsilon -> eps,
	    ScaleMu -> mu,
	    EulerGamma -> EG,
	    FAD[{p2, MZ}] :> -Mz^(-2)}

$FCAdvice=False;
FFactorPV = PaXEvaluate[ FFactor, l, FCE -> True] /. changes2 ;
Lorexp = Normal[Series[FFactorPV,{eps,0,0}]];

Put[ Lorexp, "outputs/ffactor"<>ToString[keyw]<>ToString[name]<>ToString[var]]
Print["Form Factor ", keyw, var," Saved! "]
Print["The End!"]

Quit[]