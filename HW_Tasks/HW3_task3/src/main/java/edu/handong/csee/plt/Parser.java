package edu.handong.csee.plt;

import edu.handong.csee.plt.FAE.*;

import java.util.ArrayList;
import java.util.Stack;

public class Parser {

	public AST parse(String exampleCode) {

		ArrayList<String> subExpressions = splitExpressionAsSubExpressions(exampleCode);

		// num
		if (subExpressions.size() == 1 && isNumeric(subExpressions.get(0))) {

			return new Num(subExpressions.get(0));
		}

		// add
		if (subExpressions.get(0).equals("+")) {

			return new Add(parse(subExpressions.get(1)), parse(subExpressions.get(2)));
		}

		if (subExpressions.get(0).equals("-")) {

			return new Sub(parse(subExpressions.get(1)), parse(subExpressions.get(2)));
		}

		if (subExpressions.get(0).equals("*")) {

			return new Mul(parse(subExpressions.get(1)), parse(subExpressions.get(2)));
		}

		if (subExpressions.size() == 1 && isId(subExpressions.get(0))) {

			return new Id(subExpressions.get(0));
		}


		if (subExpressions.get(0).equals("with")) {

			ArrayList<String> splitSubExpression = splitExpressionAsSubExpressions(subExpressions.get(1));

			AST tempApp = new App(new Fun (splitSubExpression.get(0), parse(subExpressions.get(2))), parse(splitSubExpression.get(1)));

			if (isRecursion.isRec(tempApp)) {
//				System.out.println("It is recursion");
				return Desugar.doDesugar(splitSubExpression.get(0), splitSubExpression.get(1), subExpressions.get(2));
			}

			return new App(new Fun (splitSubExpression.get(0), parse(subExpressions.get(2))), parse(splitSubExpression.get(1)));
		}


		if(subExpressions.get(0).equals("fun")) {

			return new Fun(subExpressions.get(1), parse(subExpressions.get(2)));
		}


		if ( subExpressions.size() == 2 ){ //Need to think about more
			return new App (parse(subExpressions.get(0)), parse(subExpressions.get(1)));
		}

		if ( subExpressions.size() == 3 && isCond(subExpressions.get(0)) ) { //Cond Expressions

			return new Condstate(subExpressions.get(0), parse(subExpressions.get(1)), parse(subExpressions.get(2)));
		}

		if ( subExpressions.get(0).equals("if") ){

			return new Ifexpr(parse(subExpressions.get(1)),parse(subExpressions.get(2)), parse(subExpressions.get(3)));
		}

		if ( subExpressions.get(0).equals("or")) {
			return new Orop(parse(subExpressions.get(1)), parse(subExpressions.get(2)));
		}

		return new Fun("Yo", new AST()); //Dummy value to check
	}

	public static ArrayList<String> splitExpressionAsSubExpressions(String exampleCode) {


		// deal with brackets first.
		if ((exampleCode.startsWith("{") && !exampleCode.endsWith("}"))
				|| (!exampleCode.startsWith("{") && exampleCode.endsWith("}"))) {
			System.out.println("Syntax error");
			System.exit(0);
		}

		if (exampleCode.startsWith("{"))
			exampleCode = exampleCode.substring(1, exampleCode.length() - 1);

		return getSubExpressions(exampleCode);
	}

	public static ArrayList<String> getSubExpressions(String code) {

		ArrayList<String> subExpressions = new ArrayList<>();
		Stack<Character> stack = new Stack<>();

		String strBuffer = "";

		for (int i = 0; i < code.length(); i++) {
			if (code.charAt(i) == ' ' && stack.isEmpty()) {
				if (!strBuffer.isEmpty()) {
					subExpressions.add(strBuffer.trim());
					strBuffer = "";
				}
			} else {
				if (code.charAt(i) == '{') {
					stack.add('{');
				} else if (code.charAt(i) == '}' && !stack.isEmpty()) {
					stack.pop();
				}
			}

			strBuffer += code.charAt(i);
		}

		subExpressions.add(strBuffer.trim());

		return subExpressions;

	}

	private static boolean isNumeric(String str) {
		return str.matches("-?\\d+(\\.\\d+)?"); // match a number with optional '-' and decimal.
	}

	private static boolean isId(String str) {
		return str.matches("^[a-zA-Z_-]+$");
	}

	private static boolean isCond(String str) {
		return str.matches(".*[=><]=?.*");
	}


}
