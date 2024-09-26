package edu.handong.csee.plt;

import edu.handong.csee.plt.FAE.AST;


public class Desugar {


    public static AST doDesugar(String functionIdentifier, String functionExpression, String functionCall){

        String firstTemplate = "{with {mk-rec {fun {body-proc} {with {fX {fun {fY} {with {f {fun {x} {{fY fY} x}}} {body-proc f}}}} {fX fX}}}} ";

        String firstWith = "{with {";
        String secondWith = " {mk-rec {fun {"+functionIdentifier+"} ";
        String lastBracket = "}}} ";
        String functionCallBracket = "}}";

        String templated = firstTemplate + firstWith + functionIdentifier + secondWith + functionExpression + lastBracket + functionCall + functionCallBracket;

        Parser parser = new Parser();

//        System.out.println(parser.parse(templated).getASTCode());

        return parser.parse(templated);
    }

}
