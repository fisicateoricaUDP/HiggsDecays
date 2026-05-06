#!/usr/bin/env wolframscript
(* ::Package:: *)

Dir = "/path/to/your/folder"

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


hgZcentral=Table[{x,8.1},{x,7,51,1}];
hgZup=Table[{x,8.1+3.6},{x,7,51,1}];
hgZdown=Table[{x,8.1-3.2},{x,7,51,1}];
hgZSM=Table[{x,6.2},{x,7,51,1}]; 
hgZSMup=Table[{x,6.19+0.15},{x,7,51,1}]; 
hgZSMdown=Table[{x,6.19-0.15},{x,7,51,1}]; 


(* Comparison between SM, MSSM+RNS predictions and combined ATLAS results (2026) *)
plotGh = ListPlot[Append[Append[Append[Append[Append[Append[paresGh,hgZcentral],hgZup],hgZdown],hgZSM],hgZSMup],hgZSMdown], 
					Joined -> True, Filling->{9->{{10},Directive[Opacity[0.4],LightGreen]},12->{{13},Directive[Opacity[0.8],LightGray]}},
					PlotStyle->{{Thickness[0.001],{Dashed,Pink}},{Thickness[0.001],{Dashed,Pink}},
					{Thickness[0.001],{Dashed,Pink}},{Thickness[0.001],{Dashed,Pink}},
					{Thickness[0.001],{Dashed,Pink}},{Thickness[0.001],{Dashed,Pink}},
					{Thickness[0.004],Red},{Thickness[0.004],Green},{Thickness[0.003],LightGreen},
					{Thickness[0.003],LightGreen},{Thickness[0.003],Gray},{Thickness[0.004],LightGray},{Thickness[0.004],LightGray}},
					Frame -> {{True, True}, {True, True}},  (* izquierda y abajo *) 
					FrameTicks -> {{Automatic, Automatic}, {Automatic, Automatic}}, 
					FrameTicksStyle -> {{Black,None},{Black,None}},
					Axes -> False, 
					(*ImageSize -> {500, 120},*) 
					(*AspectRatio -> Full*)
					PlotRangePadding -> {None, {None, Scaled[0.1]}}, 
					FrameStyle -> Directive[Black], 
					PlotRange -> {{8,51}, {4,12}},
					FrameLabel->{Style[ToString[tan \[Beta],StandardForm],15],  
					Style[ToString[Subscript[\[CapitalGamma],"Z\[Gamma]"],StandardForm]<>"  [keV]",15]},
					Epilog->{{Gray,Inset[ Rotate["Average SM: "<>ToString[SubsuperscriptBox["6.19","-0.15","+0.15"]//DisplayForm,StandardForm]<>" keV ",0],{20,11}]},
							{Red,Inset[ Rotate[" MSSM + RNS: \[LessEqual] 7.5 keV ",0],{18.8,10}]},
							{Green,Inset[ Rotate["ATLAS RUN2 + RUN3: "<>ToString[SubsuperscriptBox["8.1","-3.2","+3.6"]//DisplayForm,StandardForm]<>" keV",0],{21.7,9}]}
							,{Pink,Inset[ Rotate[ToString[SubsuperscriptBox["m","0",""]//DisplayForm,StandardForm]<>" = 8 TeV ",0],{46.5,7.73}]},
							{Pink,Inset[ Rotate[ToString[SubsuperscriptBox["m","0",""]//DisplayForm,StandardForm]<>" = 2 TeV ",0],{46.5,5.45}]}}]


Export["plotGhZgv2.pdf",plotGh];


(* Values of M_h in the considered region of RNS parameters *)
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
