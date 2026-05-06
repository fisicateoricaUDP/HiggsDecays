#!/usr/bin/env wolframscript
(* ::Package:: *)

Dir = "/paht/to/your/folder"

tmp=StringJoin[Dir,"/outputs2"]

SetDirectory[tmp]


name = Input["Key Word: "] (* name = tanb *)
a = Input["Initial data: "] (* a = 1 *)
b = Input["Final data: "] (* b = 7 *)


datos[n_] := Import[ToString[name]<>"dataHiggs"<>ToString[n]<>".csv", "CSV"];
datosv2[m_] := Map[ToExpression@StringSplit[First[#], ","]&,datos[m]]

redondear[x_, n_] := 
  Round[x, 10^Floor[Log10[Abs[x]]] * 10^(-n + 1)]

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


paresG = Flatten[Table[Map[ParesG[k,#]&,{7}],{k,a,b}],1];
paresG2 = Flatten[Table[Map[ParesG2[k,#]&,{7}],{k,a,b}],1];


ParesMh[k_] := Map[redondear[#,5]&, SortBy[datosv2[k][[All, {1,4}]],First]/. {x_, y_} :> {x, y}, {2}] // N 
ParesGh[k_] := Map[redondear[#,5]&, SortBy[datosv2[k][[All, {1,3}]],First]/. {x_, y_} :> {x, y*1000000}, {2}] // N 


paresMh = Table[ParesMh[k],{k,a,b}];
paresGh = Table[ParesGh[k],{k,a,b}];


(*Signal strength for a Higgs decaying into two gluons *)
plotmugg = ListPlot[paresG,
					Joined -> True,
					PlotStyle->{{Thickness[0.005],{Dashed,Gray}}, {Thickness[0.003],{Dashed,Gray}},
					           {Thickness[0.003],{Dashed,Gray}},{Thickness[0.003],{Dashed,Gray}},
					           {Thickness[0.003],{Dashed,Gray}},{Thickness[0.003],{Dashed,Gray}},
					           {Thickness[0.005],Purple}},
					Frame -> {{True, True}, {True, True}},  (* izquierda y abajo *) 
					FrameTicks -> {{Automatic, Automatic}, {Automatic, Automatic}}, 
					FrameTicksStyle -> {{Black,None},{Black,None}},
					Axes -> False, 
					(*ImageSize -> {500, 200},*) 
					AspectRatio->Full,
					PlotRangePadding -> {None, {Scaled[0.1], None}}, 
					FrameStyle -> Directive[Black], 
					PlotRange -> {{7,51}, {0.767,0.82}},
					(*PlotLegends-> {"Z\[Gamma]",None,"WW",None,"gg",None,"ZZ",None,"\[Gamma]\[Gamma]",None},*)
					FrameLabel->{Style[ToString[tan \[Beta],StandardForm],15], 
								Style[ToString[Subscript[\[Mu], " gg"],StandardForm],15]},
					Epilog->{Inset[ "|"<>ToString[Subscript[A,0],StandardForm]<>"/"<>ToString[Subscript[m,0],StandardForm]<>"|"<>" = 1.75",{40,0.817}],
							Inset[ "2TeV \[LessEqual] "<>ToString[Subscript[m,0],StandardForm]<>" \[LessEqual] 8 TeV",{40,0.815}],
							Inset[ ToString[Subscript[m,"1/2"],StandardForm]<>" = 1500 GeV",{40,0.813}],
							Inset[ ToString[Subscript[m,"A"],StandardForm]<>" = 600 GeV",{40,0.811}],
							Inset[ ToString[\[Mu],StandardForm]<>" = 100 GeV",{40,0.809}],
							Inset[ Rotate[" 2 TeV ",-Pi/25],{36,0.8025}],
							Inset[ Rotate[" 3 TeV ",-Pi/20],{36,0.7938}],
							Inset[ Rotate[" 4 TeV ",-Pi/20],{36,0.7875}],
							Inset[ Rotate[" 5 TeV ",-Pi/20],{36,0.7825}],
							Inset[ Rotate[" 6 TeV ",-Pi/20],{36,0.7781}],
							Inset[ Rotate[" 7 TeV ",-Pi/20],{36,0.774}],
							Inset[ Rotate[" 8 TeV ",-Pi/20],{36,0.7704}]}]


(* Ratio of decay widths = \[CapitalGamma](MSSM)/\[CapitalGamma](SM) *)
plotggratio = ListPlot[paresG2,
					Joined -> True, 
					PlotStyle->{{Thickness[0.005],{Dashed,Gray}},{Thickness[0.003],{Dashed,Gray}}, 
					           {Thickness[0.003],{Dashed,Gray}},{Thickness[0.003],{Dashed,Gray}},
					           {Thickness[0.003],{Dashed,Gray}},{Thickness[0.003],{Dashed,Gray}},
					           {Thickness[0.005],Purple}},
					Frame -> {{True, True}, {True, True}},  (* izquierda y abajo *) 
					FrameTicks -> {{Automatic, Automatic}, {Automatic, Automatic}}, 
					FrameTicksStyle -> {{Black,None},{Black,None}},
					Axes -> False, 
					(*ImageSize -> {500, 200},*) 
					AspectRatio->Full,
					PlotRangePadding -> {None, {Scaled[0.1], None}}, 
					FrameStyle -> Directive[Black], 
					PlotRange -> {{7,51}, {0.77,0.89}},
					FrameLabel->{Style[ToString[tan \[Beta],StandardForm],15],
								Style[ToString[Superscript[Subscript[\[CapitalGamma],"gg"],"RNS"],StandardForm]<>" / "<>ToString[Superscript[Subscript[\[CapitalGamma],"gg"],"SM"],StandardForm],15]}, 
								Epilog->{Inset[ "|"<>ToString[Subscript[A,0],StandardForm]<>"/"<>ToString[Subscript[m,0],StandardForm]<>"|"<>" = 1.75",{40,0.78}],
							Inset[ "2TeV \[LessEqual] "<>ToString[Subscript[m,0],StandardForm]<>" \[LessEqual] 8 TeV",{40,0.775}],
							Inset[ Rotate[" 2 TeV ",0],{45,0.8035}],
							Inset[ Rotate[" 3 TeV ",0],{45,0.824}],
							Inset[ Rotate[" 4 TeV ",0],{45,0.8385}],
							Inset[ Rotate[" 5 TeV ",0],{45,0.8505}],
							Inset[ Rotate[" 6 TeV ",0],{45,0.861}],
							Inset[ Rotate[" 7 TeV ",0],{45,0.8715}],
							Inset[ Rotate[" 8 TeV ",0],{45,0.8805}]}]


Export["plotggratio.pdf",plotggratio];


(* Evolution of M_h in the region of interest *)
plotMh = ListPlot[paresMh, 
					Joined -> True,
					PlotStyle->{{Thickness[0.003],{Dashed,Pink}},{Thickness[0.003],{Dashed,Pink}},
					{Thickness[0.003],{Dashed,Pink}},{Thickness[0.003],{Dashed,Pink}},
					{Thickness[0.003],{Dashed,Pink}},{Thickness[0.003],{Dashed,Pink}},
					{Thickness[0.005],Red}},
					Frame -> {{True, True}, {True, True}},  (* izquierda y abajo *) 
					FrameTicks -> {{Automatic, Automatic}, {Automatic, Automatic}}, 
					FrameTicksStyle -> {{Black,None},{Black,None}},
					Axes -> False, 
					(*ImageSize -> {500, 120},*) 
					(*AspectRatio -> Full,*) 
					PlotRangePadding -> {None, {None, Scaled[0.1]}}, 
					FrameStyle -> Directive[Black], 
					PlotRange -> {{7,50}, Automatic},
					FrameLabel->{Style[ToString[tan \[Beta],StandardForm],15],  
					Style[ToString[Subscript[M,h],StandardForm]<>"  [GeV]",15]}]


Quit[]
