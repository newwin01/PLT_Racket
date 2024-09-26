package edu.handong.csee.plt;

import java.util.ArrayList;
import java.util.Stack;

import edu.handong.csee.plt.FAE.*;

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

		if (subExpressions.size() == 1 && isId(subExpressions.get(0))) {

			return new Id(subExpressions.get(0));
		}


		if (subExpressions.get(0).equals("with")) {

			ArrayList<String> splitSubExpression = splitExpressionAsSubExpressions(subExpressions.get(1));

			return new App(new Fun (splitSubExpression.get(0), parse(subExpressions.get(2))), parse(splitSubExpression.get(1)));
		}


		if(subExpressions.get(0).equals("fun")) {

			return new Fun(subExpressions.get(1), parse(subExpressions.get(2)));
		}


		if ( subExpressions.size() == 2 ){ //Need to think about more
			return new App (parse(subExpressions.get(0)), parse(subExpressions.get(1)));
		}

		return null;
	}

	private ArrayList<String> splitExpressionAsSubExpressions(String exampleCode) {


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

	private ArrayList<String> getSubExpressions(String code) {

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

	public static boolean isNumeric(String str) {
		return str.matches("-?\\d+(\\.\\d+)?"); // match a number with optional '-' and decimal.
	}

	public static boolean isId(String str) {
		return str.matches("^[a-zA-Z_-]+$");
	}


}
