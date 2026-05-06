#!/usr/bin/env wolframscript
(* ::Package:: *)

Dir = "/home/edilson/Dropbox/Research/HdecaysRNS"


tmp=StringJoin[Dir,"/outputs2"]


SetDirectory[tmp]


name = Input["Key Word: "] (* name = tanb *)


a = Input["Initial data: "] (* a= 1 *)


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


paresG = Flatten[Table[Map[ParesG[k,#]&,{11}],{k,a,b}],1];
paresG2 = Flatten[Table[Map[ParesG2[k,#]&,{11}],{k,a,b}],1];


ParesMh[k_] := Map[redondear[#,5]&, SortBy[datosv2[k][[All, {1,4}]],First]/. {x_, y_} :> {x, y}, {2}] // N 


ParesGh[k_] := Map[redondear[#,5]&, SortBy[datosv2[k][[All, {1,2}]],First]/. {x_, y_} :> {x, y*1000}, {2}] // N 


paresMh = Table[ParesMh[k],{k,a,b}];
paresGh = Table[ParesGh[k],{k,a,b}];


(* Signal strength for h to gamma gamma *)
plotmuAA = ListPlot[paresG,
					Joined -> True,
					PlotStyle->{{Thickness[0.003],{Dashed,Gray}},{Thickness[0.003],{Dashed,Gray}}, 
					           {Thickness[0.003],{Dashed,Gray}},{Thickness[0.003],{Dashed,Gray}},
					           {Thickness[0.003],{Dashed,Gray}},{Thickness[0.003],{Dashed,Gray}},
					           {Thickness[0.007],Yellow}},
					Frame -> {{True, True}, {True, True}},  (* izquierda y abajo *) 
					FrameTicks -> {{Automatic, Automatic}, {Automatic, Automatic}}, 
					FrameTicksStyle -> {{Black,None},{Black,None}},
					Axes -> False, 
					(*ImageSize -> {500, 200},*) 
					AspectRatio->Full,
					PlotRangePadding -> {None, {Scaled[0.1], None}}, 
					FrameStyle -> Directive[Black], 
					PlotRange -> {{7,51}, {0.95,0.968}},
					(*PlotLegends-> {"Z\[Gamma]",None,"WW",None,"gg",None,"ZZ",None,"\[Gamma]\[Gamma]",None},*)
					FrameLabel->{Style[ToString[tan \[Beta],StandardForm],15], 
								Style[ToString[Subscript[\[Mu], "\[Gamma]\[Gamma]"],StandardForm],15]},
					Epilog->{Inset[ "|"<>ToString[Subscript[A,0],StandardForm]<>"/"<>ToString[Subscript[m,0],StandardForm]<>"|"<>" = 1.75",{40,0.965}],
							Inset[ "2TeV \[LessEqual] "<>ToString[Subscript[m,0],StandardForm]<>" \[LessEqual] 8 TeV",{40,0.964}],
							Inset[ ToString[Subscript[m,"1/2"],StandardForm]<>" = 1500 GeV",{40,0.963}],
							Inset[ ToString[Subscript[m,"A"],StandardForm]<>" = 600 GeV",{40,0.962}],
							Inset[ ToString[\[Mu],StandardForm]<>" = 100 GeV",{40,0.961}],
							Inset[ Rotate[" 3 TeV ",-Pi/8],{36,0.955}],
							Inset[ Rotate[" 8 TeV ",-Pi/8],{32,0.9535}]}]


Export["plotmuAA.pdf",plotmuAA];


(* Ratio of decay widths = \[CapitalGamma](MSSM)/\[CapitalGamma](SM) *)
plotAAratio = ListPlot[paresG2,
					Joined -> True, 
					PlotStyle->{{Thickness[0.003],{Dashed,Gray}},{Thickness[0.003],{Dashed,Gray}}, 
					           {Thickness[0.003],{Dashed,Gray}},{Thickness[0.003],{Dashed,Gray}},
					           {Thickness[0.003],{Dashed,Gray}},{Thickness[0.003],{Dashed,Gray}},
					           {Thickness[0.007],Yellow}},
					Frame -> {{True, True}, {True, True}},  (* izquierda y abajo *) 
					FrameTicks -> {{Automatic, Automatic}, {Automatic, Automatic}}, 
					FrameTicksStyle -> {{Black,None},{Black,None}},
					Axes -> False, 
					(*ImageSize -> {500, 200},*) 
					AspectRatio->Full,
					PlotRangePadding -> {None, {Scaled[0.1], None}}, 
					FrameStyle -> Directive[Black], 
					PlotRange -> {{7,51}, {0.90,1.1}},
					FrameLabel->{Style[ToString[tan \[Beta],StandardForm],15],
								Style[ToString[Superscript[Subscript[\[CapitalGamma],"\[Gamma]\[Gamma]"],"RNS"],StandardForm]<>" / "<>ToString[Superscript[Subscript[\[CapitalGamma],"\[Gamma]\[Gamma]"],"SM"],StandardForm],15]}, 
								Epilog->{Inset[ "|"<>ToString[Subscript[A,0],StandardForm]<>"/"<>ToString[Subscript[m,0],StandardForm]<>"|"<>" = 1.75",{40,0.91}],
							Inset[ "2TeV \[LessEqual] "<>ToString[Subscript[m,0],StandardForm]<>" \[LessEqual] 8 TeV",{40,0.90}],
							Inset[ Rotate[" 2 TeV ",0],{45,0.954}],
							Inset[ Rotate[" 3 TeV ",0],{45,0.992}],
							Inset[ Rotate[" 4 TeV ",0],{45,1.019}],
							Inset[ Rotate[" 5 TeV ",0],{45,1.04}],
							Inset[ Rotate[" 6 TeV ",0],{45,1.058}],
							Inset[ Rotate[" 7 TeV ",0],{45,1.076}],
							Inset[ Rotate[" 8 TeV ",0],{45,1.091}]}]


Export["plotAAratio.pdf",plotAAratio];


(* Variation of M_h in the region of interest *)
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


(* Total decay width for the Higgs boson *)
plotGh = ListPlot[paresGh, 
					Joined -> True, 
					PlotStyle->{{Thickness[0.005],{Dashed,Gray}},{Thickness[0.003],{Dashed,Gray}},
					{Thickness[0.003],{Dashed,Gray}},{Thickness[0.003],{Dashed,Gray}},
					{Thickness[0.003],{Dashed,Gray}},{Thickness[0.003],{Dashed,Gray}},
					{Thickness[0.005],Red}},
					Frame -> {{True, True}, {True, True}},  (* izquierda y abajo *) 
					FrameTicks -> {{Automatic, Automatic}, {Automatic, Automatic}}, 
					FrameTicksStyle -> {{Black,None},{Black,None}},
					Axes -> False, 
					(*ImageSize -> {500, 120},*) 
					(*AspectRatio -> Full,*) 
					PlotRangePadding -> {None, {None, Scaled[0.1]}}, 
					FrameStyle -> Directive[Black], 
					PlotRange -> {{7,51}, Automatic},
					FrameLabel->{Style[ToString[tan \[Beta],StandardForm],15],  
					Style[ToString[Superscript[Subscript[\[CapitalGamma],"h"],"RNS"],StandardForm]<>"  [MeV]",15]},
					Epilog->{{Gray,Inset[ Rotate[" 2 TeV ",0],{42,4.097}]},
							{Gray,Inset[ Rotate[" 3 TeV ",0],{42,4.245}]},
							{Gray,Inset[ Rotate[" 4 TeV ",0],{42,4.355}]},
							{Gray,Inset[ Rotate[" 5 TeV ",0],{42,4.445}]},
							{Gray,Inset[ Rotate[" 6 TeV ",0],{42,4.525}]},
							{Gray,Inset[ Rotate[" 7 TeV ",0],{42,4.605}]},
							{Gray,Inset[ Rotate[ToString[Subscript[m,0],StandardForm]<>" =  8 TeV ",0],{40.2,4.677}]},
							{Gray,Inset[ "|"<>ToString[Subscript[A,0],StandardForm]<>"/"<>ToString[Subscript[m,0],StandardForm]<>"|"<>" = 1.75",{40.5,3.95}]}}]


Export["plotGh.pdf",plotGh];


Quit[]
