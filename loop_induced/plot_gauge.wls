#!/usr/bin/env wolframscript
(* ::Package:: *)

Dir = "/path/to/your/folder/"

tmp=StringJoin[Dir,"/outputs"]

SetDirectory[tmp]


name = Input["Key Word: "] (* name = tanb *)
a = Input["Initial data: "] (* a = 1 *)
b = Input["Final data: "] (* b = 2 *)


datos[n_] := Import[ToString[name]<>"dataHiggs"<>ToString[n]<>".csv", "CSV"];

datosv2[m_] := Map[ToExpression@StringSplit[First[#], ","]&,datos[m]]

redondear[x_, n_] := 
  Round[x, 10^Floor[Log10[Abs[x]]] * 10^(-n + 1)]yu

SMvalues = {1 (*tanb*), 
			0.00407 (*\[CapitalGamma]_H [GeV]*), 
			0.00154 (*Br_Z\[Gamma]*), 
			125.09 (*M_h [GeV]]*), 
			0.584 (*Br_bb*), 
			0.215 (*Br_WW*), 
			0.086 (*Br_gg*), 
			0.0627 (*Br_\[Tau]\[Tau]*), 
			0.0290 (*Br_cc*), 
			0.0264 (*Br_ZZ*), 
			0.00227 (*Br_\[Gamma]\[Gamma]*)};


(* Signal Strength = Br(MSSM)/Br(SM) *)
ParesG[k_,m_] := Map[redondear[#,5]&, 
				  SortBy[datosv2[k][[All, {1,2,m}]],First] /. {x_, y_, z_} :> {x, (z/y)/SMvalues[[m]]}, {2}] // N 


(* Ratio of decay widths = \[CapitalGamma](MSSM)/\[CapitalGamma](SM) *)
ParesG2[k_,m_] := Map[redondear[#,5]&, 
				  SortBy[datosv2[k][[All, {1,m}]],First] /. {x_, y_} :> {x, y/(SMvalues[[m]]*SMvalues[[2]])}, {2}] // N 


paresG = Flatten[Table[Map[ParesG[k,#]&,{3,5,6,7,8,9,10,11}],{k,a,b}],1];
paresG2 = Flatten[Table[Map[ParesG2[k,#]&,{3,5,6,7,8,9,10,11}],{k,a,b}],1];


ParesMh[k_] := Map[redondear[#,5]&, SortBy[datosv2[k][[All, {1,4}]],First]/. {x_, y_} :> {x, y}, {2}] // N 


ParesGh[k_] := Map[redondear[#,5]&, SortBy[datosv2[k][[All, {1,2}]],First]/. {x_, y_} :> {x, y*1000}, {2}] // N 


paresMh = Table[ParesMh[k],{k,a,b}];
paresGh = Table[ParesGh[k],{k,a,b}];


(*Signal Strengths for gauge bosons*)
plotGauge = ListPlot[{paresG[[1]],paresG[[9]],paresG[[4]],paresG[[12]],paresG[[8]],paresG[[16]]},
					Joined -> True, Filling->{1->{2},
					3->{4},5->{6},7->{8},9->{10}},
					PlotStyle->{{Thickness[0.004],Red},{Thickness[0.004],Red},
							   {Thickness[0.004],Purple},{Thickness[0.004],Purple},
							   {Thickness[0.004],Yellow},{Thickness[0.004],Yellow}},
					Frame -> {{True, True}, {True, True}},  (* izquierda y abajo *) 
					FrameTicks -> {{Automatic, Automatic}, {Automatic, Automatic}}, 
					FrameTicksStyle -> {{Black,None},{Black,None}},
					Axes -> False, 
					(*ImageSize -> {500, 200},*) 
					AspectRatio->Full,
					PlotRangePadding -> {None, {Scaled[0.1], None}}, 
					FrameStyle -> Directive[Black], 
					PlotRange -> {{8,51}, {0.75,1.05}},
					PlotLegends-> {Z*\[Gamma],None,gg,None,\[Gamma]\[Gamma],None},
					FrameLabel->{Style[ToString[tan \[Beta],StandardForm],15],
								Style[ToString[Subscript[\[Mu], " XY"],StandardForm],15]},
					Epilog->{Inset[ "2TeV \[LessEqual] "<>ToString[Subscript[m,0],StandardForm]<>" \[LessEqual] 8 TeV",{38,0.86}],
							Inset[ ToString[Subscript[m,"1/2"],StandardForm]<>" = 1.5 TeV;  "<>ToString[Subscript[m,"A"],StandardForm]<>" = 1.5 TeV",{38,0.84}],
							Inset[ ToString[\[Mu],StandardForm]<>" = 100 GeV",{38,0.82}]}]


Export["plotGauge.pdf",plotGauge];


(* Ratio of decay widths = \[CapitalGamma](MSSM)/\[CapitalGamma](SM) *)
plotG2 = ListPlot[{paresG2[[9]],paresG2[[11]],paresG2[[12]],paresG2[[15]],paresG2[[16]],paresG2[[10]],paresG2[[13]],paresG2[[14]]},
					Joined -> True, 
					PlotStyle->{{Thickness[0.004],Red},{Thickness[0.004],Blue},
							   {Thickness[0.004],Purple},{Thickness[0.004],Gray},
							   {Thickness[0.005],Yellow},{Thickness[0.003],{Dashed,Black}},
							   {Thickness[0.004],{Dashed,Green}},{Thickness[0.004],{Dashed,Orange}}},
					Frame -> {{True, True}, {True, True}},  (* izquierda y abajo *) 
					FrameTicks -> {{Automatic, Automatic}, {Automatic, Automatic}}, 
					FrameTicksStyle -> {{Black,None},{Black,None}},
					Axes -> False, 
					(*ImageSize -> {500, 200},*) 
					AspectRatio->Full,
					PlotRangePadding -> {None, {Scaled[0.1], None}}, 
					FrameStyle -> Directive[Black], 
					PlotRange -> {{7,51}, {0.84,1.43}},
					PlotLegends-> {"Z\[Gamma]","WW","gg","ZZ","\[Gamma]\[Gamma]","bb","\[Tau]\[Tau]","cc"},
					FrameLabel->{Style[ToString[tan \[Beta],StandardForm],15],
								Style[ToString[Superscript[Subscript[\[CapitalGamma],"XY"],"RNS"],StandardForm]<>" / "<>ToString[Superscript[Subscript[\[CapitalGamma],"XY"],"SM"],StandardForm],15]}]


(* Higgs boson mass in the region of interest *)
plotMh = ListPlot[paresMh, 
					Joined -> True, Filling->{1->{2}},
					FillingStyle -> Directive[LightBlue, Opacity[0.4], AbsoluteThickness[1.2], Dashing[{0.3, 0.3}] ],
					PlotStyle->{{Thickness[0.003],{Dashed,Pink}},{Thickness[0.004],Red}},
					Frame -> {{True, True}, {True, True}},  (* izquierda y abajo *) 
					FrameTicks -> {{Automatic, Automatic}, {Automatic, Automatic}}, 
					FrameTicksStyle -> {{Black,None},{Black,None}},
					Axes -> False, 
					(*ImageSize -> {500, 120},*) 
					(*AspectRatio -> Full,*) 
					PlotRangePadding -> {None, {None, Scaled[0.1]}}, 
					FrameStyle -> Directive[Black], 
					PlotRange -> {{7,50}, Automatic},
					FrameLabel->{Style[ToString[Subscript[t,\[Beta]],StandardForm],15],  
					Style[ToString[Subscript[M,h],StandardForm]<>"  [GeV]",15]}]


(* Total decay width for the RNS region of parameters *)
plotGh = ListPlot[paresGh, 
					Joined -> True, Filling->{1->{2}},
					PlotStyle->{{Thickness[0.003],{Dashed,Gray}},{Thickness[0.004],Black}},
					Frame -> {{True, True}, {True, True}},  (* izquierda y abajo *) 
					FrameTicks -> {{Automatic, Automatic}, {Automatic, Automatic}}, 
					FrameTicksStyle -> {{Black,None},{Black,None}},
					Axes -> False, 
					(*ImageSize -> {500, 120},*) 
					(*AspectRatio -> Full,*) 
					PlotRangePadding -> {None, {None, Scaled[0.1]}}, 
					FrameStyle -> Directive[Black], 
					PlotRange -> {{7,50}, Automatic},
					FrameLabel->{Style[ToString[Subscript[t,\[Beta]],StandardForm],15],  
					Style[ToString[Superscript[Subscript[\[CapitalGamma],"h"],"RNS"],StandardForm]<>"  [MeV]",15]}]


Quit[]
