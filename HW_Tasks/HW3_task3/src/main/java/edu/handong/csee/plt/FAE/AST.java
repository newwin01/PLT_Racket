package edu.handong.csee.plt.FAE;
//BNF

/*
<RFAE> ::= <num>
		| {+ <RFAE> <RFAE>}
		| {- <RFAE> <RFAE>}
		| {* <RFAE> <RFAE>}
		| <id>
		| {fun {<id>} <RFAE>}
		| {<RFAE> <RFAE>}
		| {if <Orop> <RFAE> RFAE>}
<Orop> ::= {or {<Cond>} {<Cond>} } | {<Cond>}
<Cond> ::= <Condstate> <RFAE> <RFAE>
<CondState> ::= = | < | > | <= | >=
 */
public class AST {
	
	public String getASTCode() {
		String astCode="";
		if(this instanceof Add)
			astCode = ((Add)this).getASTCode();
		
		if(this instanceof Num)
			astCode = ((Num)this).getASTCode();

		if(this instanceof Sub)
			astCode = ((Sub)this).getASTCode();

		if(this instanceof Id)
			astCode = ((Id)this).getASTCode();

		if(this instanceof Fun)
			astCode = ((Fun)this).getASTCode();

		if(this instanceof App)
			astCode = ((App)this).getASTCode();

		if(this instanceof Ifexpr)
			astCode = ((Ifexpr) this).getASTCode();

		if(this instanceof Condstate)
			astCode = ((Condstate) this).getASTCode();

		if(this instanceof Orop)
			astCode = ((Orop) this).getASTCode();

		return astCode;
	}
}

