package edu.handong.csee.plt;

import edu.handong.csee.plt.AriOperation.Addition;
import edu.handong.csee.plt.AriOperation.Subtraction;
import edu.handong.csee.plt.DefrdSub.*;
import edu.handong.csee.plt.FAE.*;
import edu.handong.csee.plt.FAEValue.*;


public class Interpreter {

	public Object interp(AST ast, DeferdSubAST ds) {

		if(ast instanceof Num) {

			return new numV(((Num)ast).getStrNum()); //Wrapped
		}
		
		if(ast instanceof Add) {

			Add add = (Add)ast;
			return ( new Addition().operateBinary( (numV)(interp(add.getLhs(), ds)), (numV) (interp(add.getRhs(), ds))) );

		}

		if(ast instanceof Sub){
			Sub sub = (Sub)ast;
			return ( new Subtraction().operateBinary( (numV)(interp(sub.getLhs(), ds)), (numV) (interp(sub.getRhs(), ds))) );
		}

		if (ast instanceof Id){
			Id id = (Id)ast;
			return lookUp.operateLookUp(id.getSymbol(), ds);
		}

		if (ast instanceof Fun) {

			Fun fun = (Fun)ast;

			return new ClosureV(fun.getParam(), fun.getBody(), ds);
		}


		if (ast instanceof App) {

			App app = (App)ast;

			Object f_val = (interp (app.getFunExpr(), ds) );
			Object a_val = (interp (app.getArgExpr(), ds) );

			return interp((((ClosureV)f_val).getBody()) , new aSub(((ClosureV)f_val).getParam(), ((FAEAST)a_val), ((ClosureV)f_val).getDs())) ;
		}

		return null;
	}
}
