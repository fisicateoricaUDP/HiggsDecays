#!/usr/bin/env wolframscript
(* ::Package:: *)

Dir = "/wherever/you/save/this/code"

(*save the folder outputs in the same folder as this code was saved*)
tmp=StringJoin[Dir,"/outputs"]


SetDirectory[tmp]


(* Key words: mu, mhalf, A0m0, A0, m0, tanb *)
name = Input["Key Word: "]

(*see folder outputs to look up how to set a and b*)
a = Input["Initial data: "]
b = Input["Final data: "]


datos[n_] := Import[ToString[name]<>"dataHiggs"<>ToString[n]<>".csv", "CSV"];
datosv2[m_] := Map[ToExpression@StringSplit[First[#], ","]&,datos[m]]


redondear[x_, n_] := 
  Round[x, 10^Floor[Log10[Abs[x]]] * 10^(-n + 1)]


ParesG[k_] := Map[redondear[#,5]&, SortBy[datosv2[k][[All, {1,3}]],First] /. {x_, y_} :> {x, y/10^-6}, {2}] // N 


paresG = Table[ParesG[k],{k,a,b}]


ParesMh[k_] := Map[redondear[#,5]&, SortBy[datosv2[k][[All, {1,4}]],First]/. {x_, y_} :> {x, y}, {2}] // N 


paresMh = Table[ParesMh[k],{k,a,b}]


valuesY1 = Flatten@Table[ParesG[j][[All,2]],{j,a,b}]


ticksY1 = 
  Table[
    {val, 
     NumberForm[val, 5]}, 
    {val, 
     Min[valuesY1], 
     Max[valuesY1], 
     (Max[valuesY1] - Min[valuesY1])/5}
  ]


plotG = ListPlot[paresG,
					Joined -> True, 
					Frame -> True, 
					FrameTicks -> {{ticksY1,Automatic}, {None, All}}, 
					FrameTicksStyle -> {{Black,None},{Automatic,Automatic}},
					Axes -> False, 
					ImageSize -> {500, 200}, 
					AspectRatio->Full,
					PlotRangePadding -> {None, {Scaled[0.1], None}}, 
					FrameStyle -> Directive[Black], 
					PlotRange -> {Automatic, Automatic},
					PlotLegends-> Range[a,b],
					FrameLabel->{None, Style[Subscript[\[CapitalGamma],\[Gamma]Z]["keV"],11]}]


valuesY2 = Flatten@Table[ParesMh[j][[All,2]],{j,a,b}]


ticksY2 = 
  Table[
    {val, 
     NumberForm[val, 5]}, 
    {val, 
     Min[valuesY2], 
     Max[valuesY2], 
     (Max[valuesY2] - Min[valuesY2])/5}
  ]


plotMh = ListPlot[paresMh, 
					Joined -> True, 
					PlotStyle->{{Thickness[0.002],{Dashed,Pink}},{Thickness[0.002],{Dashed,Pink}},
							   {Thickness[0.002],{Dashed,Pink}}, {Thickness[0.002],{Dashed,Pink}},
							   {Thickness[0.002],{Dashed,Pink}},{Thickness[0.002],{Dashed,Pink}},
							   {Thickness[0.004],Red}},
					Frame -> {{True, True}, {True, True}},  (* izquierda y abajo *) 
					FrameTicks -> {{ticksY2, Automatic}, {Automatic, Automatic}}, 
					FrameTicksStyle -> {{Black,None},{Black,None}},
					Axes -> False, 
					ImageSize -> {500, 120}, 
					AspectRatio -> Full, 
					PlotRangePadding -> {None, {None, Scaled[0.1]}}, 
					FrameStyle -> Directive[Black], 
					PlotRange -> {Automatic, Automatic},
					FrameLabel->{Style[ToString[Subscript[X,x],StandardForm]<>" / "<>ToString[GeV],15],  
					Style[Subscript[M,h]["GeV"],12]}]


plot=Graphics[
 {
  Inset[plotG, {0, 1}, {Center, Bottom}],
  Inset[plotMh, {0, 0}, {Center, Top}]
 },
 
 PlotRange -> {{-250, 250}, {-125, 210}}, 
 ImageSize -> 500
]


Export["plot.pdf",plot]

Quit[]


(*
Epilog for tanb:

Epilog\[Rule]{Inset[ ToString[Subscript[m,0],StandardForm]<>" = 8.0 TeV",{17,6.40}],
							Inset[ ToString[Subscript[A,0],StandardForm]<>" = -13 TeV",{17.05,6.32}],
							Inset[ ToString[Subscript[m,A],StandardForm]<>" = 1.0 TeV",{16.95,6.24}],
							Inset[ ToString[\[Mu],StandardForm]<>" = 0.1 ",{16.85,6.16}],
							Inset[ ToString[Subscript[m,1/2],StandardForm]<>" = 0.6 TeV",{16.9,6.06}]}
*)


(*
Epilog for m_A:

Epilog\[Rule]{Inset[ ToString[Subscript[m,0],StandardForm]<>" = 8.0 TeV",{1250,6.85}],
							Inset[ ToString[Subscript[A,0],StandardForm]<>" = -15 TeV",{1258,6.75}],
							Inset[ ToString[Subscript[t,\[Beta]],StandardForm]<>" = 20.0 ",{1240,6.65}],
							Inset[ ToString[\[Mu],StandardForm]<>" = 0.35 TeV ",{1278,6.57}],
							Inset[ ToString[Subscript[m,1/2],StandardForm]<>" = 1.5 TeV",{1240,6.45}]}
*)


(*
Epilog for A_0/m_0:

Epilog->{Inset[ "|"ToString[Subscript[A,0],StandardForm]<>"/"<>ToString[Subscript[m,0],StandardForm]<>"|"<>" = 1.75",{4700,6.63}],
							Inset[ "   1.50",{4962,6.15}],Inset[ "   1.25",{4962,5.70}],
                                                        Inset[ "   1.00",{4962,5.35}],Inset[ "   0.75",{4962,5.05}],
							Inset[ "   0.50",{4962,4.85}],Inset[ "   0.25",{4962,4.65}]}
*)
