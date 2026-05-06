
var1 = ToExpression[$ScriptCommandLine[[2]]]
var2 = ToExpression[$ScriptCommandLine[[3]]]
name = ToExpression[$ScriptCommandLine[[4]]] 

SetDirectory[ Directory[]];
If[ !DirectoryQ[ ToString[results]], CreateDirectory["results"]];
Print["The amps are in: ", ToFileName[{Directory[],"outputs"}]]

Amp1pi[n_] := Get["ffactoropi"<>ToString[name]<>ToString[n],
		  Path->{ToFileName[{Directory[],"outputs"}]}]

Ampn1pi[n_] := Get["ffactornopi"<>ToString[name]<>ToString[n],
                   Path->{ToFileName[{Directory[],"outputs"}]}]
		  
(* Divergent and Finite parts of 1PI amplitudes*)

Div1PI = Plus@@ParallelTable[ Coefficient[Amp1pi[j],eps,-1],
       	 		                 {j, 1, var1}] // Simplify ;
Put[ Div1PI, "results/div1pi"<>ToString[name]];

Fin1PI = Plus@@ParallelTable[ Coefficient[Amp1pi[j],eps,0],
			                 {j, 1, var1}] ;
Put[ Fin1PI, "results/fin1pi"<>ToString[name]]

Print["UV div and finite parts of 1PI ", name," amps saved! "]


(* Divergent and Finite parts of non-1PI amps *)

DivN1PI = Plus@@ParallelTable[ Coefficient[Ampn1pi[j],eps,-1],
			                  {j, 1, var2}] // Simplify ;
Put[ DivN1PI, "results/divn1pi"<>ToString[name]];

FinN1PI = Plus@@ParallelTable[ Coefficient[Ampn1pi[j],eps,0],
	  		                  {j, 1, var2}] ;
Put[ FinN1PI, "results/finn1pi"<>ToString[name]]


Print["UV div and finite parts of non-1PI ", name," amps saved! "]

Print["The End!"]

Quit[];


