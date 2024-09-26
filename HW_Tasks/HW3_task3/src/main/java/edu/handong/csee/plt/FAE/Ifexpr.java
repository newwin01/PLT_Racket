package edu.handong.csee.plt.FAE;

public class Ifexpr extends AST {

    private AST compCondition = new AST();
    private AST ifTrue = new AST();
    private AST ifFalse = new AST();

    public Ifexpr(AST compCondition, AST ifTrue, AST ifFalse) {

        this.compCondition = compCondition;
        this.ifTrue = ifTrue;
        this.ifFalse = ifFalse;

    }

    public AST getCompCondition() {return compCondition;}
    public AST getIfTrue() {return ifTrue;}
    public AST getIfFalse() {return ifFalse;}

    @Override
    public String getASTCode() {return  "(if " + compCondition.getASTCode() + " " + ifTrue.getASTCode() + " " + ifFalse.getASTCode() + ")" ;}

}
